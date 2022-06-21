pub mod layout;
use std::sync::Arc;

use crate::{
    dbpanic,
    error::{IsSilenced, Silencable},
    hir::HirIdWrapper,
    hir_types::NominalId,
    interner::HirId,
    order::{ResolvedExpr, SubValueKind},
    types::Type,
};

use fxhash::{FxHashMap, FxHashSet};

use crate::{
    expr::{self, ExpressionHead},
    hir,
    interner::{FieldName, Identifier},
    order::Orders,
    types::TypeId,
};

#[salsa::query_group(AbsIntDatabase)]
pub trait AbsInt: Orders {}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Arg {
    Named(Identifier),
    From,
}

pub trait AbstractDomain<'a>: Sized + Clone + std::hash::Hash + Eq + std::fmt::Debug {
    type Err: IsSilenced;
    type DomainContext;
    fn widen(self, ctx: &mut AbsIntCtx<'a, Self>, other: Self) -> Result<(Self, bool), Self::Err>;
    fn join(self, ctx: &mut AbsIntCtx<'a, Self>, other: Self) -> Result<(Self, bool), Self::Err>;
    fn make_block(
        ctx: &mut AbsIntCtx<'a, Self>,
        id: hir::BlockId,
        ty: TypeId,
        fields: &FxHashMap<FieldName, Self>,
    ) -> Result<Self, Self::Err>;
    fn make_thunk(
        ctx: &mut AbsIntCtx<'a, Self>,
        id: hir::ParserDefId,
        ty: TypeId,
        args: &FxHashMap<Arg, Self>,
    ) -> Result<Self, Self::Err>;
    fn eval_expr(
        ctx: &mut AbsIntCtx<'a, Self>,
        expr: ExpressionHead<expr::KindWithData<ResolvedExpr, TypeId>, Self>,
    ) -> Result<Self, Self::Err>;
    fn typecast(self, ctx: &mut AbsIntCtx<'a, Self>, ty: TypeId) -> Result<Self, Self::Err>;
    fn get_arg(self, ctx: &mut AbsIntCtx<'a, Self>, arg: Arg) -> Result<Self, Self::Err>;
    fn bottom(ctx: &mut AbsIntCtx<'a, Self>) -> Self;
}

pub type AbstractExpression<Dom> = expr::Expression<expr::KindWithData<ResolvedExpr, Dom>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PdEvaluated<Dom: Clone + std::hash::Hash + Eq + std::fmt::Debug> {
    returned: Dom,
    expr_vals: Option<AbstractExpression<Dom>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockEvaluated<Dom: Clone + std::hash::Hash + Eq + std::fmt::Debug> {
    expr_vals: FxHashMap<hir::ExprId, AbstractExpression<Dom>>,
    vals: FxHashMap<HirId, Dom>,
    returned: Dom,
}

pub struct AbsIntCtx<'a, Dom: AbstractDomain<'a>> {
    pub db: &'a dyn AbsInt,
    pub dcx: Dom::DomainContext,

    type_substitutions: Arc<Vec<TypeId>>,

    depth: usize,
    current_pd: Dom,
    call_needs_fixpoint: FxHashSet<usize>,
    active_calls: FxHashMap<Dom, usize>,
    pd_result: FxHashMap<Dom, Option<PdEvaluated<Dom>>>,

    block_vars: FxHashMap<HirId, Dom>,
    block_expr_vals: FxHashMap<hir::ExprId, AbstractExpression<Dom>>,
    block_result: FxHashMap<(Dom, Dom), Option<BlockEvaluated<Dom>>>,
    active_block: Option<Dom>,

    errors: Vec<(Dom, Dom::Err)>,
}

impl<'a, Dom: AbstractDomain<'a>> AbsIntCtx<'a, Dom> {
    fn strip_error<T>(&mut self, x: Result<T, Dom::Err>) -> Option<T> {
        match x {
            Ok(x) => Some(x),
            Err(e) => {
                if !e.is_silenced() {
                    self.errors.push((self.current_pd.clone(), e));
                }
                None
            }
        }
    }

    fn type_substitutions(&self, ty: TypeId) -> Arc<Vec<TypeId>> {
        match self.db.lookup_intern_type(ty) {
            Type::Nominal(nom) => nom.ty_args,
            _ => panic!("type_substitutions called on non-nominal type"),
        }
    }

    fn subst_type(&self, ty: TypeId) -> TypeId {
        self.db
            .substitute_typevar(ty, self.type_substitutions.clone())
    }

    fn set_pd_ret(&mut self, evaluated: Option<PdEvaluated<Dom>>) -> Option<Dom> {
        let ret = evaluated.as_ref().map(|x| x.returned.clone());
        self.pd_result.insert(self.current_pd.clone(), evaluated);
        ret
    }

    fn eval_expr(
        &mut self,
        expr: expr::Expression<expr::KindWithData<ResolvedExpr, TypeId>>,
    ) -> Result<AbstractExpression<Dom>, Dom::Err> {
        expr.try_scan(&mut |expr| {
            let owned_expr = expr.make_owned();
            let ty = self.subst_type(*owned_expr.root_data());
            let subst_expr = owned_expr.map_data(|_| ty);
            let ret = Dom::eval_expr(self, subst_expr)?;
            ret.typecast(self, ty)
        })
    }

    fn eval_expr_with_ambience(
        &mut self,
        expr: expr::Expression<expr::KindWithData<ResolvedExpr, TypeId>>,
        from: Dom,
        result_type: TypeId,
    ) -> Result<(Dom, AbstractExpression<Dom>), Dom::Err> {
        let res = self.eval_expr(expr)?;
        let applied = ExpressionHead::new_dyadic(
            expr::OpWithData {
                inner: expr::ValBinOp::ParserApply,
                data: result_type,
            },
            [from, res.0.root_data().clone()],
        );
        let ret_val = Dom::eval_expr(self, applied)?;
        Ok((ret_val, res))
    }

    fn eval_pd_expr(
        &mut self,
        val: Dom,
        parserdef: &hir::ParserDef,
    ) -> Result<PdEvaluated<Dom>, Dom::Err> {
        let from = val.get_arg(self, Arg::From)?;
        let expr = self.db.resolve_expr(parserdef.to)?;
        let result_type = self.subst_type(self.db.parser_returns(parserdef.id)?.deref);
        let (ret_val, expr_vals) = self.eval_expr_with_ambience(expr, from, result_type)?;
        let ret = PdEvaluated {
            returned: ret_val,
            expr_vals: Some(expr_vals),
        };
        Ok(ret)
    }

    fn eval_pd_fixpoint(
        &mut self,
        pd: Dom,
        mut ret: Option<Dom>,
        parserdef: &hir::ParserDef,
    ) -> Option<Dom> {
        loop {
            let old_ret = ret?;
            let evaluated = self.eval_pd_expr(pd.clone(), parserdef);
            let widened_return = self.strip_error(evaluated).and_then(|mut evaluated| {
                let widened = old_ret.widen(self, evaluated.returned.clone());
                let (ret, changed) = self.strip_error(widened)?;
                evaluated.returned = ret;
                Some((evaluated, changed))
            });
            ret = widened_return.as_ref().map(|(x, _)| x.returned.clone());
            let stop = widened_return
                .as_ref()
                .map(|(_, changed)| *changed)
                .unwrap_or(true);
            self.set_pd_ret(widened_return.map(|x| x.0));
            if stop {
                return ret;
            }
        }
    }
    pub fn eval_pd(&mut self, pd: Dom, ty: TypeId) -> Option<Dom> {
        // TODO(8051): work out a way to cache when possible
        // we can't always cache because of fixpoints invalidating the cache
        if let Some(&depth) = self.active_calls.get(&pd) {
            self.call_needs_fixpoint.insert(depth);
            if let Some(pd) = self.pd_result.get(&pd) {
                return pd.as_ref().map(|x| x.returned.clone());
            }
            let bottom = Dom::bottom(self);
            self.pd_result.insert(
                pd,
                Some(PdEvaluated {
                    returned: bottom.clone(),
                    expr_vals: None,
                }),
            );
            return Some(bottom);
        }
        self.depth += 1;
        self.active_calls.insert(pd.clone(), self.depth);
        let old_pd = std::mem::replace(&mut self.current_pd, pd.clone());
        let new_type_substitutions = self.type_substitutions(ty);
        let old_type_substitutions =
            std::mem::replace(&mut self.type_substitutions, new_type_substitutions);
        let parserdef = match self.db.lookup_intern_type(ty) {
            Type::Nominal(nomhead) => match NominalId::from_nominal_head(&nomhead) {
                NominalId::Def(parserdef) => parserdef.lookup(self.db).ok()?,
                NominalId::Block(_) => panic!("called eval_pd with non-pd type"),
            },
            _ => panic!("called eval_pd with non-pd type"),
        };
        // to make sure
        self.call_needs_fixpoint.remove(&self.depth);

        let result = self.eval_pd_expr(pd.clone(), &parserdef);
        let result = self.strip_error(result);

        let mut ret = self.set_pd_ret(result);
        if self.call_needs_fixpoint.remove(&self.depth) {
            ret = self.eval_pd_fixpoint(pd.clone(), ret, &parserdef);
        }

        self.type_substitutions = old_type_substitutions;
        self.current_pd = old_pd;
        self.active_calls.remove(&pd);
        self.depth -= 1;

        ret
    }

    pub fn var_by_id(&self, id: HirId) -> Dom {
        match self.block_vars.get(&id) {
            Some(v) => v.clone(),
            None => dbpanic!(self.db, "Did not find variable {}", &id,),
        }
    }

    fn set_block_var(&mut self, id: HirId, val: Dom) {
        self.block_vars.insert(id, val);
    }

    pub fn active_block(&self) -> &Option<Dom> {
        &self.active_block
    }

    fn eval_block_impl(
        &mut self,
        block_id: hir::BlockId,
        from: Dom,
        result_type: TypeId,
    ) -> Result<Dom, Dom::Err> {
        let block = block_id.lookup(self.db)?;
        let order = self.db.block_serialization(block_id).silence()?.eval_order;
        for subvalue in order.iter() {
            match subvalue.kind {
                SubValueKind::Val => {}
                // TODO(8051): for now, we just use the from argument, but we should probably
                // start using the front and back subvalues for this in the future
                _ => continue,
            }
            let result_ty = self.subst_type(self.db.parser_type_at(subvalue.id)?);
            let val = match self.db.hir_node(subvalue.id)? {
                hir::HirNode::Let(statement) => {
                    let expr = self.db.resolve_expr(statement.expr)?;
                    let res_expr = self.eval_expr(expr)?;
                    let res = res_expr.0.root_data().clone();
                    self.block_expr_vals.insert(statement.expr, res_expr);
                    res.typecast(self, result_ty)?
                }
                hir::HirNode::Parse(statement) => {
                    let expr = self.db.resolve_expr(statement.expr)?;
                    let (res, res_expr) =
                        self.eval_expr_with_ambience(expr, from.clone(), result_ty)?;
                    self.block_expr_vals.insert(statement.expr, res_expr);
                    res
                }
                hir::HirNode::ChoiceIndirection(ind) => ind
                    .choices
                    .iter()
                    .try_fold(
                        None,
                        |acc: Option<Dom>, (_, choice_id)| -> Result<_, Dom::Err> {
                            let new = self.var_by_id(*choice_id);
                            let derefed = new.typecast(self, result_ty)?;
                            match acc {
                                None => Ok(Some(derefed)),
                                Some(acc) => Ok(Some(acc.join(self, derefed)?.0)),
                            }
                        },
                    )?
                    .unwrap(),
                _ => continue,
            };
            self.set_block_var(subvalue.id, val);
        }
        let root_context = block.root_context.lookup(self.db)?;
        let vars = root_context
            .vars
            .iter()
            .map(|(name, id)| (*name, self.var_by_id(*id.inner())))
            .collect::<FxHashMap<_, _>>();
        Dom::make_block(self, block_id, result_type, &vars)
    }

    pub fn eval_block(
        &mut self,
        block_id: hir::BlockId,
        block: Dom,
        from: Dom,
        result_type: TypeId,
    ) -> Option<Dom> {
        //if let Some(block_info) = self.block_result.get(&(block.clone(), from.clone())) {
        //    return block_info.as_ref().map(|x| x.returned.clone());
        //}
        let mut old_block_vars = std::mem::take(&mut self.block_vars);
        let mut old_block_expr = std::mem::take(&mut self.block_expr_vals);
        let old_block = std::mem::replace(&mut self.active_block, Some(block.clone()));

        let res = self.eval_block_impl(block_id, from.clone(), result_type);
        let res = self.strip_error(res);

        self.active_block = old_block;
        std::mem::swap(&mut self.block_expr_vals, &mut old_block_expr);
        std::mem::swap(&mut self.block_vars, &mut old_block_vars);

        let evaluated = res.clone().map(|returned| BlockEvaluated {
            expr_vals: old_block_expr,
            vals: old_block_vars,
            returned,
        });
        self.block_result.insert((from, block), evaluated);

        res
    }

    pub fn apply_thunk_arg(
        &mut self,
        pd_id: hir::ParserDefId,
        result_type: TypeId,
        from: Dom,
    ) -> Result<Dom, Dom::Err> {
        let mut args = FxHashMap::default();
        args.insert(Arg::From, from);
        Dom::make_thunk(self, pd_id, result_type, &args)
    }
}
