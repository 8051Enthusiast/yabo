mod represent;

use std::{num::NonZeroU64, sync::Arc};

use hir::{BlockId, Hirs};
use yaboc_ast::expr;
use yaboc_base::{
    dbpanic,
    error::{IsSilenced, Silencable},
    interner::{DefId, FieldName},
    source::SpanIndex,
};
use yaboc_dependents::{Dependents, SubValueKind};
use yaboc_expr::{ExprHead, ExprIdx, Expression, FetchExpr, ShapedData, TakeRef};
use yaboc_hir as hir;
use yaboc_hir::HirIdWrapper;
use yaboc_hir_types::{FullTypeId, NominalId, TyHirs};
use yaboc_resolve::expr::Resolved;
use yaboc_types::{Type, TypeId, TypeInterner};

use fxhash::{FxHashMap, FxHashSet};

#[salsa::query_group(AbsIntDatabase)]
pub trait AbsInt: Dependents {}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Arg {
    Named(DefId),
    From,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Epoch(NonZeroU64);

impl Epoch {
    fn next(&self) -> Self {
        Self(NonZeroU64::new(self.0.get() + 1).unwrap())
    }
    
    fn update(&mut self) {
        *self = self.next();
    }
}

impl Default for Epoch {
    fn default() -> Self {
        Self(NonZeroU64::new(1).unwrap())
    }
}

pub trait AbstractDomain<'a>: Sized + Clone + std::hash::Hash + Eq + std::fmt::Debug {
    type Err: IsSilenced;
    type DB: ?Sized + AbsInt;
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
        args: &FxHashMap<Arg, (Self, TypeId)>,
    ) -> Result<Self, Self::Err>;
    fn eval_expr(
        ctx: &mut AbsIntCtx<'a, Self>,
        expr: ExprHead<Resolved, &(Self, TypeId)>,
        ty: TypeId,
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
    pub idx: usize,
    pub ty: TypeId,
}

pub type AbstractData<Dom> = ShapedData<Vec<(Dom, TypeId)>, Resolved>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PdEvaluated<Dom: Clone + std::hash::Hash + Eq + std::fmt::Debug> {
    pub returned: Dom,
    pub from: Dom,
    pub expr_vals: Option<AbstractData<Dom>>,
    pub typesubst: Arc<Vec<TypeId>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockEvaluated<Dom: Clone + std::hash::Hash + Eq + std::fmt::Debug> {
    pub id: BlockId,
    pub expr_vals: FxHashMap<hir::ExprId, AbstractData<Dom>>,
    pub from: Dom,
    pub vals: FxHashMap<DefId, Dom>,
    pub returned: Dom,
    pub typesubst: Arc<Vec<TypeId>>,
}

pub struct AbsIntCtx<'a, Dom: AbstractDomain<'a>> {
    pub db: &'a Dom::DB,
    pub dcx: Dom::DomainContext,

    // values in the current epoch might change later
    // because of fixpoint iteration, but values from
    // previous epochs are stable
    cache_epoch: Epoch,

    type_substitutions: Arc<Vec<TypeId>>,

    current_pd: Dom,
    depth: usize,
    call_needs_fixpoint: FxHashSet<usize>,
    active_calls: FxHashMap<Dom, usize>,
    pd_result: FxHashMap<Dom, (Option<PdEvaluated<Dom>>, Epoch)>,
    existing_pd: FxHashSet<(TypeId, Dom)>,
    new_pd: FxHashSet<(TypeId, Dom)>,

    block_vars: FxHashMap<DefId, Dom>,
    block_expr_vals: FxHashMap<hir::ExprId, AbstractData<Dom>>,
    block_result: FxHashMap<(Dom, Dom), Option<BlockEvaluated<Dom>>>,
    active_block: Option<Dom>,

    errors: Vec<(Dom, Dom::Err)>,
}

impl<'a, Dom: AbstractDomain<'a>> AbsIntCtx<'a, Dom> {
    pub fn new(db: &'a Dom::DB, mut dcx: Dom::DomainContext) -> Self {
        let bottom = Dom::bottom(&mut dcx);
        Self {
            db,
            dcx,
            current_pd: bottom,
            cache_epoch: Default::default(),
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

    pub fn pd_result(&self) -> &FxHashMap<Dom, (Option<PdEvaluated<Dom>>, Epoch)> {
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
        self.pd_result
            .insert(self.current_pd.clone(), (evaluated, self.cache_epoch));
        ret
    }

    fn eval_expr<'b, 'c>(
        &'b mut self,
        expr: impl Expression<Resolved, Part = (ExprHead<Resolved, ExprIdx<Resolved>>, &'c TypeId)>,
    ) -> Result<AbstractData<Dom>, Dom::Err> {
        expr.try_scan(
            |(expr, ty): (ExprHead<_, _>, _)| -> Result<(Dom, TypeId), Dom::Err> {
                let ty = self.subst_type(*ty);
                let ret = Dom::eval_expr(self, expr, ty)?;
                let casted_ret = ret.typecast(self, ty).map(|x| x.0)?;
                Ok((casted_ret, ty))
            },
        )
    }

    fn eval_expr_with_ambience<'b, 'c>(
        &'b mut self,
        expr: impl Expression<Resolved, Part = (ExprHead<Resolved, ExprIdx<Resolved>>, &'c TypeId)>,
        from: (Dom, TypeId),
        result_type: TypeId,
    ) -> Result<(Dom, AbstractData<Dom>), Dom::Err> {
        let res = self.eval_expr(expr)?;
        let root = res.root_data().clone();
        let applied = ExprHead::new_dyadic(expr::ValBinOp::ParserApply, [&from, &root]);
        let ret_val = Dom::eval_expr(self, applied, result_type)?;
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
        let expr = Resolved::expr_with_data::<FullTypeId>(self.db, parserdef.to)?;
        let result_type = self.subst_type(self.db.parser_returns(parserdef.id)?.deref);
        let (ret_val, expr_vals) =
            self.eval_expr_with_ambience(expr.take_ref(), (from.clone(), from_ty), result_type)?;
        let ret = PdEvaluated {
            returned: ret_val,
            from,
            expr_vals: Some(expr_vals),
            typesubst: self.type_substitutions.clone(),
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
        let new_type_substitutions = self.type_substitutions(ty);
        if let Some((pd, epoch)) = self.pd_result.get(&pd) {
            // if we are in the same epoch, the value still might
            // change due to a fixpoint
            if *epoch < self.cache_epoch {
                return Some(pd.as_ref()?.returned.clone());
            }
        }
        if let Some(&depth) = self.active_calls.get(&pd) {
            if self.call_needs_fixpoint.is_empty() {
                // we can only increment the epoch if we are not
                // currently in a fixpoint computation
                self.cache_epoch.update();
            }
            self.call_needs_fixpoint.insert(depth);
            if let Some((pd, _)) = self.pd_result.get(&pd) {
                return pd.as_ref().map(|x| x.returned.clone());
            }
            let bottom = Dom::bottom(&mut self.dcx);
            self.pd_result.insert(
                pd,
                (
                    Some(PdEvaluated {
                        returned: bottom.clone(),
                        from: bottom.clone(),
                        expr_vals: None,
                        typesubst: new_type_substitutions,
                    }),
                    self.cache_epoch,
                ),
            );
            return Some(bottom);
        }
        self.depth += 1;
        self.active_calls.insert(pd.clone(), self.depth);
        let old_pd = std::mem::replace(&mut self.current_pd, pd.clone());
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
        if self.call_needs_fixpoint.is_empty() {
            // update the epoch since we are not in a fixpoint computation
            // so that the current value does not get invalidated
            // in case we do enter a fixpoint computation later
            self.cache_epoch.update();
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
                    let expr = Resolved::expr_with_data::<FullTypeId>(self.db, statement.expr)?;
                    let res_expr = self.eval_expr(expr.take_ref())?;
                    let res = res_expr.root_data();
                    self.set_block_var(statement.expr.0, res.0.clone());
                    let ret = res.0.clone().typecast(self, result_ty)?.0;
                    self.block_expr_vals.insert(statement.expr, res_expr);
                    ret
                }
                hir::HirNode::Parse(statement) => {
                    let expr = Resolved::expr_with_data::<FullTypeId>(self.db, statement.expr)?;
                    let (res, res_expr) = self.eval_expr_with_ambience(
                        expr.take_ref(),
                        (from.clone(), arg_type),
                        result_ty,
                    )?;
                    self.set_block_var(statement.expr.0, res_expr.root_data().0.clone());
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
            id: block_id,
            expr_vals: old_block_expr,
            from: from.clone(),
            vals: old_block_vars,
            returned,
            typesubst: self.type_substitutions.clone(),
        });
        self.block_result.insert((from, block), evaluated);

        res
    }

    pub fn apply_thunk_arg(
        &mut self,
        pd_id: hir::ParserDefId,
        result_type: TypeId,
        from: (Dom, TypeId),
        thunk_args: &[(Dom, TypeId)],
    ) -> Result<Dom, Dom::Err> {
        let mut args = FxHashMap::default();
        args.insert(Arg::From, from);
        let pd = pd_id.lookup(self.db)?;
        for (idx, (arg, ty)) in thunk_args.iter().enumerate() {
            let def = pd.args.as_ref().unwrap()[idx].0;
            args.insert(Arg::Named(def), (arg.clone(), *ty));
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
