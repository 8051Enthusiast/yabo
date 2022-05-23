use std::sync::Arc;

use crate::{
    dbpanic,
    error::{Silencable, SilencedError},
    hir::HirIdWrapper,
    hir_types::IndirectionLevel,
    interner::HirId,
    order::{ResolvedExpr, SubValue, SubValueKind},
    types::Type,
};

use fxhash::{FxHashMap, FxHashSet};

use crate::{
    expr::{self, ExpressionHead},
    hir,
    hir_types::TyHirs,
    interner::{FieldName, Identifier},
    order::Orders,
    types::TypeId,
};

#[salsa::query_group(AbsIntDatabase)]
pub trait AbsInt: TyHirs + Orders {}

pub enum Arg {
    Named(Identifier),
    From,
}

pub trait AbstractDomain<'a>: Sized + Clone + std::hash::Hash + Eq + std::fmt::Debug {
    type Err: From<SilencedError>;
    fn widen(self, ctx: &mut AbsIntCtx<'a, Self>, other: Self) -> Result<(Self, bool), Self::Err>;
    fn join(self, ctx: &mut AbsIntCtx<'a, Self>, other: Self) -> Result<(Self, bool), Self::Err>;
    fn make_block(
        ctx: &mut AbsIntCtx<'a, Self>,
        id: hir::BlockId,
        fields: &FxHashMap<FieldName, Self>,
    ) -> Result<Self, Self::Err>;
    fn eval_expr(
        ctx: &mut AbsIntCtx<'a, Self>,
        expr: ExpressionHead<expr::KindWithData<ResolvedExpr, TypeId>, Self>,
    ) -> Result<Self, Self::Err>;
    fn convert_to_indirection_level(
        self,
        ctx: &mut AbsIntCtx<'a, Self>,
        level: IndirectionLevel,
    ) -> Result<Self, Self::Err>;
    fn get_arg(self, ctx: &mut AbsIntCtx<'a, Self>, arg: Arg) -> Result<Self, Self::Err>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct AbsIntCall {
    pub pd: hir::ParserDefId,
    pub pd_ty: TypeId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VarPlace {
    pub call: AbsIntCall,
    pub val: SubValue,
}

pub struct AbsIntCtx<'a, Dom: AbstractDomain<'a>> {
    pub db: &'a dyn AbsInt,
    current_call: AbsIntCall,
    currently_recursive: bool,
    active_calls: FxHashSet<AbsIntCall>,
    pd_vars: FxHashMap<VarPlace, Dom>,
    block_vars: FxHashMap<VarPlace, Dom>,
    type_substitutions: Arc<Vec<TypeId>>,
}

impl<'a, Dom: AbstractDomain<'a>> AbsIntCtx<'a, Dom> {
    pub fn new(db: &'a dyn AbsInt, root_call: AbsIntCall) -> Self {
        let mut active_calls = FxHashSet::default();
        active_calls.insert(root_call);
        let mut ret = Self {
            db,
            currently_recursive: false,
            pd_vars: Default::default(),
            current_call: root_call,
            active_calls,
            block_vars: Default::default(),
            type_substitutions: Default::default(),
        };
        ret.type_substitutions = ret.type_substitutions(root_call.pd_ty);
        ret
    }

    fn type_substitutions(&self, ty: TypeId) -> Arc<Vec<TypeId>> {
        match self.db.lookup_intern_type(ty) {
            Type::Nominal(nom) => nom.ty_args,
            _ => panic!("type_substitutions called on non-nominal type"),
        }
    }

    fn deref_to_type(&mut self, val: Dom, ty: TypeId) -> Result<Dom, Dom::Err> {
        let level = self.db.indirection_level(ty)?;
        Dom::convert_to_indirection_level(val, self, level)
    }

    fn subst_type(&self, ty: TypeId) -> TypeId {
        self.db
            .substitute_typevar(ty, self.type_substitutions.clone())
    }

    fn eval_expr(
        &mut self,
        expr: expr::Expression<expr::KindWithData<ResolvedExpr, TypeId>>,
    ) -> Result<expr::Expression<expr::KindWithData<ResolvedExpr, Dom>>, Dom::Err> {
        expr.try_scan(&mut |expr| {
            let owned_expr = expr.make_owned();
            let ty = self.subst_type(*owned_expr.root_data());
            let subst_expr = owned_expr.map_data(|_| ty);
            let ret = Dom::eval_expr(self, subst_expr)?;
            self.deref_to_type(ret, ty)
        })
    }

    fn eval_expr_with_ambience(
        &mut self,
        expr: expr::Expression<expr::KindWithData<ResolvedExpr, TypeId>>,
        from: Dom,
        result_type: TypeId,
    ) -> Result<Dom, Dom::Err> {
        let res = self.eval_expr(expr)?;
        let applied = ExpressionHead::new_dyadic(
            expr::OpWithData {
                inner: expr::ValBinOp::ParserApply,
                data: result_type,
            },
            [from, res.0.root_data().clone()],
        );
        let ret_val = Dom::eval_expr(self, applied)?;
        Ok(ret_val)
    }

    fn set_var(&mut self, var: SubValue, val: Dom) -> Result<(Dom, bool), Dom::Err> {
        let place = VarPlace {
            call: self.current_call,
            val: var,
        };
        let (new_val, is_changed) = match self.pd_vars.get(&place) {
            Some(v) => {
                if self.currently_recursive {
                    v.clone().widen(self, val)?
                } else {
                    v.clone().join(self, val)?
                }
            }
            None => (val, false),
        };
        self.pd_vars.insert(place, new_val.clone());
        Ok((new_val, is_changed))
    }

    fn set_block_var(&mut self, var: HirId, val: Dom) {
        self.block_vars.insert(
            VarPlace {
                call: self.current_call,
                val: SubValue::new_val(var),
            },
            val.clone(),
        );
    }

    fn with_call_context<T>(
        &mut self,
        call: AbsIntCall,
        f: impl FnOnce(&mut Self) -> Result<T, Dom::Err>,
    ) -> Result<T, Dom::Err> {
        let old_call = self.current_call;
        let old_currently_recursive = self.currently_recursive;
        self.current_call = call;
        self.currently_recursive = !self.active_calls.insert(call);
        let old_block_vars = std::mem::take(&mut self.block_vars);
        let new_type_substitutions = self.type_substitutions(call.pd_ty);
        let old_type_substitutions =
            std::mem::replace(&mut self.type_substitutions, new_type_substitutions);

        let ret = f(self);

        self.type_substitutions = old_type_substitutions;
        self.block_vars = old_block_vars;
        if !self.currently_recursive {
            self.active_calls.remove(&call);
        }
        self.currently_recursive = old_currently_recursive;
        self.current_call = old_call;

        ret
    }

    fn eval_impl(&mut self, call: AbsIntCall, val: Dom) -> Result<Dom, Dom::Err> {
        let (val, changed) = self.set_var(SubValue::new_val(call.pd.id()), val)?;
        let parserdef = call.pd.lookup(self.db)?;
        let ret_place = SubValue::new_val(parserdef.to.0);
        if !changed {
            let full_ret_place = VarPlace {
                call,
                val: ret_place,
            };
            return Ok(self.pd_vars[&full_ret_place].clone());
        }
        let from = val.get_arg(self, Arg::From)?;
        let expr = self.db.resolve_expr(parserdef.to)?;
        let result_type = self.subst_type(self.db.parser_returns(call.pd)?.deref);
        let ret_val = self.eval_expr_with_ambience(expr, from, result_type)?;
        Ok(self.set_var(ret_place, ret_val)?.0)
    }

    pub fn eval(&mut self, call: AbsIntCall, val: Dom) -> Result<Dom, Dom::Err> {
        self.with_call_context(call, |ctx| ctx.eval_impl(call, val))
    }

    pub fn var_by_id(&self, id: HirId) -> Dom {
        let place = VarPlace {
            call: self.current_call,
            val: SubValue::new_val(id),
        };
        match self.pd_vars.get(&place) {
            Some(v) => v.clone(),
            None => dbpanic!(
                self.db,
                "Did not find variable {} in {}",
                &id,
                &self.current_call.pd.0
            ),
        }
    }

    pub fn eval_block(&mut self, block_id: hir::BlockId, from: Dom) -> Result<Dom, Dom::Err> {
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
                    let res = self.eval_expr(expr)?.0.root_data().clone();
                    self.deref_to_type(res, result_ty)?
                }
                hir::HirNode::Parse(statement) => {
                    let expr = self.db.resolve_expr(statement.expr)?;
                    self.eval_expr_with_ambience(expr, from.clone(), result_ty)?
                }
                hir::HirNode::ChoiceIndirection(ind) => ind
                    .choices
                    .iter()
                    .try_fold(None, |acc, (_, choice_id)| -> Result<_, Dom::Err> {
                        let new = self.var_by_id(*choice_id);
                        let derefed = self.deref_to_type(new, result_ty)?;
                        match acc {
                            None => Ok(Some(derefed)),
                            Some(acc) => Ok(Some(acc.join(self, derefed)?.0)),
                        }
                    })?
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
        Dom::make_block(self, block_id, &vars)
    }
}
