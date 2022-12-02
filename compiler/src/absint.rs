use std::sync::Arc;

use crate::{
    dbpanic,
    error::{IsSilenced, Silencable},
    expr::KindWithData,
    hir::HirIdWrapper,
    hir_types::NominalId,
    interner::DefId,
    order::SubValueKind,
    resolve::expr::ResolvedKind,
    source::SpanIndex,
    types::Type,
};

use fxhash::{FxHashMap, FxHashSet};

use crate::{
    expr::{self, ExpressionHead},
    hir,
    interner::FieldName,
    order::Orders,
    types::TypeId,
};

#[salsa::query_group(AbsIntDatabase)]
pub trait AbsInt: Orders {}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Arg {
    Named(DefId),
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
        expr: ExpressionHead<expr::KindWithData<ResolvedKind, TypeId>, (Self, TypeId)>,
    ) -> Result<Self, Self::Err>;
    fn typecast(self, ctx: &mut AbsIntCtx<'a, Self>, ty: TypeId)
        -> Result<(Self, bool), Self::Err>;
    fn get_arg(self, ctx: &mut AbsIntCtx<'a, Self>, arg: Arg) -> Result<Self, Self::Err>;
    fn bottom(ctx: &mut Self::DomainContext) -> Self;
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct AbstractExprInfo<Dom> {
    pub val: Dom,
    pub span: SpanIndex,
    pub ty: TypeId,
}

pub type AbstractExpression<Dom> =
    expr::Expression<expr::KindWithData<ResolvedKind, AbstractExprInfo<Dom>>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PdEvaluated<Dom: Clone + std::hash::Hash + Eq + std::fmt::Debug> {
    pub returned: Dom,
    pub from: Dom,
    pub expr_vals: Option<AbstractExpression<Dom>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockEvaluated<Dom: Clone + std::hash::Hash + Eq + std::fmt::Debug> {
    pub expr_vals: FxHashMap<hir::ExprId, AbstractExpression<Dom>>,
    pub from: Dom,
    pub vals: FxHashMap<DefId, Dom>,
    pub returned: Dom,
}

pub struct AbsIntCtx<'a, Dom: AbstractDomain<'a>> {
    pub db: &'a dyn AbsInt,
    pub dcx: Dom::DomainContext,

    type_substitutions: Arc<Vec<TypeId>>,

    current_pd: Dom,
    depth: usize,
    call_needs_fixpoint: FxHashSet<usize>,
    active_calls: FxHashMap<Dom, usize>,
    pd_result: FxHashMap<Dom, Option<PdEvaluated<Dom>>>,
    existing_pd: FxHashSet<(TypeId, Dom)>,
    new_pd: FxHashSet<(TypeId, Dom)>,

    block_vars: FxHashMap<DefId, Dom>,
    block_expr_vals: FxHashMap<hir::ExprId, AbstractExpression<Dom>>,
    block_result: FxHashMap<(Dom, Dom), Option<BlockEvaluated<Dom>>>,
    active_block: Option<Dom>,

    errors: Vec<(Dom, Dom::Err)>,
}

impl<'a, Dom: AbstractDomain<'a>> AbsIntCtx<'a, Dom> {
    pub fn new(db: &'a dyn AbsInt, mut dcx: Dom::DomainContext) -> Self {
        let bottom = Dom::bottom(&mut dcx);
        Self {
            db,
            dcx,
            current_pd: bottom,
            type_substitutions: Default::default(),
            depth: Default::default(),
            call_needs_fixpoint: Default::default(),
            active_calls: Default::default(),
            pd_result: Default::default(),
            existing_pd: Default::default(),
            new_pd: Default::default(),
            block_vars: Default::default(),
            block_expr_vals: Default::default(),
            block_result: Default::default(),
            active_block: Default::default(),
            errors: Default::default(),
        }
    }

    pub fn pd_result(&self) -> &FxHashMap<Dom, Option<PdEvaluated<Dom>>> {
        &self.pd_result
    }

    pub fn block_result(&self) -> &FxHashMap<(Dom, Dom), Option<BlockEvaluated<Dom>>> {
        &self.block_result
    }

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
        expr: expr::Expression<expr::KindWithData<ResolvedKind, (TypeId, SpanIndex)>>,
    ) -> Result<AbstractExpression<Dom>, Dom::Err> {
        expr.try_scan(&mut |expr| -> Result<AbstractExprInfo<Dom>, _> {
            let owned_expr = expr.make_owned();
            let span = owned_expr.root_data().1;
            let ty = self.subst_type(owned_expr.root_data().0);
            let subst_expr: ExpressionHead<KindWithData<_, TypeId>, _> =
                owned_expr.map_data(|_| ty).map_inner(|x| (x.val, x.ty));
            let ret = Dom::eval_expr(self, subst_expr)?;
            let casted_ret = ret.typecast(self, ty).map(|x| x.0)?;
            Ok(AbstractExprInfo {
                val: casted_ret,
                span,
                ty,
            })
        })
    }

    fn eval_expr_with_ambience(
        &mut self,
        expr: expr::Expression<expr::KindWithData<ResolvedKind, (TypeId, SpanIndex)>>,
        from: (Dom, TypeId),
        result_type: TypeId,
    ) -> Result<(Dom, AbstractExpression<Dom>), Dom::Err> {
        let res = self.eval_expr(expr)?;
        let roo = res.0.root_data().clone();
        let applied = ExpressionHead::new_dyadic(
            expr::OpWithData {
                inner: expr::ValBinOp::ParserApply,
                data: result_type,
            },
            [from, (roo.val, roo.ty)],
        );
        let ret_val = Dom::eval_expr(self, applied)?;
        let casted_ret = ret_val.typecast(self, result_type)?.0;
        Ok((casted_ret, res))
    }

    fn eval_pd_expr(
        &mut self,
        val: Dom,
        parserdef: &hir::ParserDef,
        from_ty: TypeId,
    ) -> Result<PdEvaluated<Dom>, Dom::Err> {
        let from = val.get_arg(self, Arg::From)?;
        let expr = self.db.parser_expr_at(parserdef.to)?;
        let result_type = self.subst_type(self.db.parser_returns(parserdef.id)?.deref);
        let (ret_val, expr_vals) =
            self.eval_expr_with_ambience(expr, (from.clone(), from_ty), result_type)?;
        let ret = PdEvaluated {
            returned: ret_val,
            from,
            expr_vals: Some(expr_vals),
        };
        Ok(ret)
    }

    fn eval_pd_fixpoint(
        &mut self,
        pd: Dom,
        mut ret: Option<Dom>,
        parserdef: &hir::ParserDef,
        from_ty: TypeId,
    ) -> Option<Dom> {
        loop {
            let old_ret = ret?;
            let evaluated = self.eval_pd_expr(pd.clone(), parserdef, from_ty);
            let widened_return = self.strip_error(evaluated).and_then(|mut evaluated| {
                let widened = old_ret.widen(self, evaluated.returned.clone());
                let (ret, changed) = self.strip_error(widened)?;
                evaluated.returned = ret;
                Some((evaluated, changed))
            });
            ret = widened_return.as_ref().map(|(x, _)| x.returned.clone());
            let stop = widened_return
                .as_ref()
                .map(|(_, changed)| !*changed)
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
            let bottom = Dom::bottom(&mut self.dcx);
            self.pd_result.insert(
                pd,
                Some(PdEvaluated {
                    returned: bottom.clone(),
                    from: bottom.clone(),
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
        let unsub_from_ty = self.db.parser_args(parserdef.id).ok()?.from.unwrap();
        let from_ty = self
            .db
            .substitute_typevar(unsub_from_ty, self.type_substitutions.clone());
        // to make sure
        self.call_needs_fixpoint.remove(&self.depth);
        let old_active_block = self.active_block.take();

        let result = self.eval_pd_expr(pd.clone(), &parserdef, from_ty);
        let result = self.strip_error(result);

        let mut ret = self.set_pd_ret(result);
        if self.call_needs_fixpoint.remove(&self.depth) {
            ret = self.eval_pd_fixpoint(pd.clone(), ret, &parserdef, from_ty);
        }

        self.active_block = old_active_block;
        self.type_substitutions = old_type_substitutions;
        self.current_pd = old_pd;
        self.active_calls.remove(&pd);
        self.depth -= 1;

        ret
    }

    pub fn var_by_id(&mut self, id: DefId) -> Result<Dom, Dom::Err> {
        if self.active_block.is_some() {
            match self.block_vars.get(&id) {
                Some(v) => Ok(v.clone()),
                None => dbpanic!(self.db, "Did not find variable {}", &id,),
            }
        } else {
            self.current_pd.clone().get_arg(self, Arg::Named(id))
        }
    }

    fn set_block_var(&mut self, id: DefId, val: Dom) {
        self.block_vars.insert(id, val);
    }

    pub fn active_block(&self) -> &Option<Dom> {
        &self.active_block
    }

    pub fn active_pd(&self) -> &Dom {
        &self.current_pd
    }

    fn eval_block_impl(
        &mut self,
        block_id: hir::BlockId,
        from: Dom,
        result_type: TypeId,
        arg_type: TypeId,
    ) -> Result<Dom, Dom::Err> {
        let block = block_id.lookup(self.db)?;
        let order = self.db.block_serialization(block_id).silence()?.eval_order;
        for subvalue_info in order.iter() {
            let subvalue = subvalue_info.val;
            match subvalue.kind {
                SubValueKind::Val => {}
                // TODO(8051): for now, we just use the from argument, but we should probably
                // start using the front and back subvalues for this in the future
                _ => continue,
            }
            let node = self.db.hir_node(subvalue.id)?;
            match node {
                hir::HirNode::Let(_)
                | hir::HirNode::Parse(_)
                | hir::HirNode::ChoiceIndirection(_) => {}
                _ => continue,
            }
            let result_ty = self.subst_type(self.db.parser_type_at(subvalue.id)?);
            let val = match self.db.hir_node(subvalue.id)? {
                hir::HirNode::Let(statement) => {
                    let expr = self.db.parser_expr_at(statement.expr)?;
                    let res_expr = self.eval_expr(expr)?;
                    let res = res_expr.0.root_data().val.clone();
                    self.set_block_var(statement.expr.0, res.clone());
                    self.block_expr_vals.insert(statement.expr, res_expr);
                    res.typecast(self, result_ty)?.0
                }
                hir::HirNode::Parse(statement) => {
                    let expr = self.db.parser_expr_at(statement.expr)?;
                    let (res, res_expr) =
                        self.eval_expr_with_ambience(expr, (from.clone(), arg_type), result_ty)?;
                    self.set_block_var(statement.expr.0, res_expr.0.root_data().val.clone());
                    self.block_expr_vals.insert(statement.expr, res_expr);
                    res
                }
                hir::HirNode::ChoiceIndirection(ind) => ind
                    .choices
                    .iter()
                    .try_fold(
                        None,
                        |acc: Option<Dom>, (_, choice_id)| -> Result<_, Dom::Err> {
                            let new = self.var_by_id(*choice_id)?;
                            let derefed = new.typecast(self, result_ty)?.0;
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

        if block.returns {
            let ret = block.root_context.0.child_field(self.db, FieldName::Return);
            let ret = self.var_by_id(ret)?;
            return Ok(ret);
        }

        let root_context = block.root_context.lookup(self.db)?;
        let vars = root_context
            .vars
            .iter()
            .map(|(name, id)| Ok((*name, self.var_by_id(*id.inner())?)))
            .collect::<Result<FxHashMap<_, _>, Dom::Err>>()?;
        Dom::make_block(self, block_id, result_type, &vars)
    }

    pub fn eval_block(
        &mut self,
        block_id: hir::BlockId,
        block: Dom,
        from: Dom,
        result_type: TypeId,
        arg_type: TypeId,
    ) -> Option<Dom> {
        //if let Some(block_info) = self.block_result.get(&(block.clone(), from.clone())) {
        //    return block_info.as_ref().map(|x| x.returned.clone());
        //}
        let mut old_block_vars = std::mem::take(&mut self.block_vars);
        let mut old_block_expr = std::mem::take(&mut self.block_expr_vals);
        let old_block = std::mem::replace(&mut self.active_block, Some(block.clone()));

        let res = self.eval_block_impl(block_id, from.clone(), result_type, arg_type);
        let res = self.strip_error(res);

        self.active_block = old_block;
        std::mem::swap(&mut self.block_expr_vals, &mut old_block_expr);
        std::mem::swap(&mut self.block_vars, &mut old_block_vars);

        let evaluated = res.clone().map(|returned| BlockEvaluated {
            expr_vals: old_block_expr,
            from: from.clone(),
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
        thunk_args: &[(Dom, TypeId)],
    ) -> Result<Dom, Dom::Err> {
        let mut args = FxHashMap::default();
        args.insert(Arg::From, from);
        let pd = pd_id.lookup(self.db)?;
        for (idx, (arg, _)) in thunk_args.iter().enumerate() {
            let def = pd.args.as_ref().unwrap()[idx].0;
            args.insert(Arg::Named(def), arg.clone());
        }
        let pd = Dom::make_thunk(self, pd_id, result_type, &args)?;
        if !self.existing_pd.contains(&(result_type, pd.clone())) {
            self.new_pd.insert((result_type, pd.clone()));
        }
        Ok(pd)
    }

    pub fn new_pds(&mut self) -> FxHashSet<(TypeId, Dom)> {
        let new_pd = std::mem::take(&mut self.new_pd);
        self.existing_pd.extend(new_pd.iter().cloned());
        new_pd
    }
}
