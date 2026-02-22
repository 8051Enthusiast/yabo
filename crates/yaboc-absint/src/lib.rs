mod represent;

use std::num::NonZeroU64;

use hir::{BlockId, Hirs};
use yaboc_base::{
    databased_display::DatabasedDisplay,
    dbpanic,
    error::{IsSilenced, Silencable},
    interner::{DefId, FieldName},
    source::SpanIndex,
};
use yaboc_dependents::{Dependents, SubValueKind};
use yaboc_expr::{ExprHead, ExprIdx, Expression, FetchExpr, ShapedData, TakeRef};
use yaboc_hir::{self as hir, BlockReturnKind, DefKind};
use yaboc_hir::{HirIdWrapper, ParserDefId};
use yaboc_resolve::expr::{self, Resolved};
use yaboc_types::TypeId;

use fxhash::{FxHashMap, FxHashSet};

#[salsa::query_group(AbsIntDatabase)]
pub trait AbsInt: Dependents {}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Arg {
    Named(DefId),
    From,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Epoch(NonZeroU64);

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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EpochVal<T> {
    val: T,
    epoch: Epoch,
}

pub type MaybeEpoch<T> = EpochVal<Option<T>>;

impl<T> EpochVal<T> {
    fn new(val: T, epoch: Epoch) -> Self {
        Self { val, epoch }
    }

    pub fn val(&self) -> &T {
        &self.val
    }

    pub fn into_val(self) -> T {
        self.val
    }

    fn val_with_epoch(&self, epoch: Epoch) -> Option<&T> {
        if self.epoch < epoch {
            Some(&self.val)
        } else {
            None
        }
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
        fields: &FxHashMap<FieldName, Self>,
    ) -> Result<Self, Self::Err>;
    fn make_thunk(
        ctx: &mut AbsIntCtx<'a, Self>,
        id: hir::ParserDefId,
        args: &FxHashMap<Arg, Self>,
    ) -> Result<Self, Self::Err>;
    fn eval_expr(
        ctx: &mut AbsIntCtx<'a, Self>,
        expr: ExprHead<Resolved, &Self>,
    ) -> Result<Self, Self::Err>;
    fn evaluate(self, ctx: &mut AbsIntCtx<'a, Self>) -> Result<(Self, bool), Self::Err>;
    fn normalize(self, ctx: &mut AbsIntCtx<'a, Self>) -> Result<(Self, bool), Self::Err>;
    fn get_arg(self, ctx: &mut AbsIntCtx<'a, Self>, arg: Arg) -> Result<Self, Self::Err>;
    fn bottom(ctx: &mut Self::DomainContext) -> Self;
    fn unit(ctx: &mut Self::DomainContext) -> Self;
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct AbstractExprInfo<Dom> {
    pub val: Dom,
    pub span: SpanIndex,
    pub idx: usize,
    pub ty: TypeId,
}

pub type AbstractData<Dom> = ShapedData<Vec<Dom>, Resolved>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PdEvaluated<Dom: Clone + std::hash::Hash + Eq + std::fmt::Debug> {
    pub returned: Dom,
    pub from: Option<Dom>,
    pub expr_vals: Option<AbstractData<Dom>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LambdaEvaluated<Dom: Clone + std::hash::Hash + Eq + std::fmt::Debug> {
    pub returned: Dom,
    pub expr_vals: AbstractData<Dom>,
}

impl<Dom: Clone + std::hash::Hash + Eq + std::fmt::Debug> From<LambdaEvaluated<Dom>>
    for PdEvaluated<Dom>
{
    fn from(lambda: LambdaEvaluated<Dom>) -> Self {
        Self {
            returned: lambda.returned,
            from: None,
            expr_vals: Some(lambda.expr_vals),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockEvaluated<Dom: Clone + std::hash::Hash + Eq + std::fmt::Debug> {
    pub id: BlockId,
    pub expr_vals: FxHashMap<hir::ExprId, AbstractData<Dom>>,
    pub from: Option<Dom>,
    pub vals: FxHashMap<DefId, Dom>,
    pub returned: Dom,
}

type CallSite<Dom> = (Option<Dom>, Dom);

pub struct AbsIntCtx<'a, Dom: AbstractDomain<'a>> {
    pub db: &'a Dom::DB,
    pub dcx: Dom::DomainContext,

    // values in the current epoch might change later
    // because of fixpoint iteration, but values from
    // previous epochs are stable
    cache_epoch: Epoch,

    current_ambience: Option<Dom>,

    current_pd: Dom,
    depth: usize,
    call_needs_fixpoint: FxHashSet<usize>,
    active_calls: FxHashMap<Dom, usize>,
    pd_result: FxHashMap<Dom, MaybeEpoch<PdEvaluated<Dom>>>,

    block_vars: FxHashMap<DefId, Dom>,
    block_expr_vals: FxHashMap<hir::ExprId, AbstractData<Dom>>,
    block_result: FxHashMap<CallSite<Dom>, MaybeEpoch<BlockEvaluated<Dom>>>,
    active_env: Dom,

    lambda_result: FxHashMap<Dom, MaybeEpoch<LambdaEvaluated<Dom>>>,

    errors: Vec<(Dom, Dom::Err)>,
}

impl<'a, Dom: AbstractDomain<'a> + DatabasedDisplay<Dom::DB>> AbsIntCtx<'a, Dom> {
    pub fn new(db: &'a Dom::DB, mut dcx: Dom::DomainContext) -> Self {
        let bottom = Dom::bottom(&mut dcx);
        Self {
            db,
            dcx,
            current_pd: bottom.clone(),
            cache_epoch: Default::default(),
            current_ambience: Default::default(),
            depth: Default::default(),
            call_needs_fixpoint: Default::default(),
            active_calls: Default::default(),
            pd_result: Default::default(),
            block_vars: Default::default(),
            block_expr_vals: Default::default(),
            block_result: Default::default(),
            lambda_result: Default::default(),
            active_env: bottom,
            errors: Default::default(),
        }
    }

    pub fn pd_result(&self) -> &FxHashMap<Dom, MaybeEpoch<PdEvaluated<Dom>>> {
        &self.pd_result
    }

    pub fn block_result(&self) -> &FxHashMap<CallSite<Dom>, MaybeEpoch<BlockEvaluated<Dom>>> {
        &self.block_result
    }

    pub fn lambda_result(&self) -> &FxHashMap<Dom, MaybeEpoch<LambdaEvaluated<Dom>>> {
        &self.lambda_result
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

    fn set_pd_ret(&mut self, evaluated: Option<PdEvaluated<Dom>>) -> Option<Dom> {
        let ret = evaluated.as_ref().map(|x| x.returned.clone());
        self.pd_result.insert(
            self.current_pd.clone(),
            EpochVal::new(evaluated, self.cache_epoch),
        );
        ret
    }

    fn eval_expr(
        &mut self,
        expr: impl Expression<Resolved, Part = ExprHead<Resolved, ExprIdx<Resolved>>>,
    ) -> Result<AbstractData<Dom>, Dom::Err> {
        expr.try_scan(|expr: ExprHead<_, _>| -> Result<Dom, Dom::Err> {
            let ret = Dom::eval_expr(self, expr)?;
            let normalized = ret.normalize(self)?.0;
            Ok(normalized)
        })
    }

    fn eval_expr_with_ambience(
        &mut self,
        expr: impl Expression<Resolved, Part = ExprHead<Resolved, ExprIdx<Resolved>>>,
    ) -> Result<(Dom, AbstractData<Dom>), Dom::Err> {
        let res = self.eval_expr(expr)?;
        let root = res.root_data().clone();
        let Some(from) = self.current_ambience.clone() else {
            panic!("called eval_expr_with_ambience without ambience")
        };
        let applied = ExprHead::new_dyadic(expr::ValBinOp::ParserApply, [&from, &root]);
        let ret_val = Dom::eval_expr(self, applied)?;
        let normalized = ret_val.normalize(self)?.0;
        Ok((normalized, res))
    }

    fn eval_pd_expr(&mut self, parserdef: &hir::ParserDef) -> Result<PdEvaluated<Dom>, Dom::Err> {
        let expr = Resolved::fetch_expr(self.db, parserdef.to)?;
        let ((ret_val, expr_vals), from) = if let Some(from) = self.current_ambience.clone() {
            (self.eval_expr_with_ambience(expr.take_ref())?, Some(from))
        } else {
            let res = self.eval_expr(expr.take_ref())?;
            let root = res.root_data().clone();
            ((root, res), None)
        };
        let returned = if parserdef.kind == DefKind::Def {
            ret_val.evaluate(self)?.0
        } else {
            ret_val.normalize(self)?.0
        };
        let ret = PdEvaluated {
            returned,
            from,
            expr_vals: Some(expr_vals),
        };
        Ok(ret)
    }

    fn eval_pd_fixpoint(
        &mut self,
        mut ret: Option<Dom>,
        parserdef: &hir::ParserDef,
    ) -> Option<Dom> {
        loop {
            let old_ret = ret?;
            let evaluated = self.eval_pd_expr(parserdef);
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
    pub fn eval_pd(&mut self, pd_val: Dom, pd: ParserDefId) -> Option<Dom> {
        if let Some(val) = self.pd_result.get(&pd_val) {
            // if we are in the same epoch, the value still might
            // change due to a fixpoint
            if let Some(pd) = val.val_with_epoch(self.cache_epoch) {
                return Some(pd.as_ref()?.returned.clone());
            }
        }
        let parserdef = pd.lookup(self.db).ok()?;
        if let Some(&depth) = self.active_calls.get(&pd_val) {
            if self.call_needs_fixpoint.is_empty() {
                // we can only increment the epoch if we are not
                // currently in a fixpoint computation
                self.cache_epoch.update();
            }
            self.call_needs_fixpoint.insert(depth);
            if let Some(pd) = self.pd_result.get(&pd_val) {
                return pd.val().as_ref().map(|x| x.returned.clone());
            }
            let bottom = Dom::bottom(&mut self.dcx);
            self.pd_result.insert(
                pd_val,
                EpochVal::new(
                    Some(PdEvaluated {
                        returned: bottom.clone(),
                        from: parserdef.from.is_some().then(|| bottom.clone()),
                        expr_vals: None,
                    }),
                    self.cache_epoch,
                ),
            );
            return Some(bottom);
        }
        self.depth += 1;
        self.active_calls.insert(pd_val.clone(), self.depth);
        let old_pd = std::mem::replace(&mut self.current_pd, pd_val.clone());
        let from = if parserdef.from.is_some() {
            pd_val.clone().get_arg(self, Arg::From).ok()
        } else {
            None
        };
        let old_ambienece = std::mem::replace(&mut self.current_ambience, from);
        if self.call_needs_fixpoint.contains(&self.depth) {
            panic!("contains fixpoint depth that was expected to be removed")
        }
        self.call_needs_fixpoint.remove(&self.depth);
        let old_active_env = std::mem::replace(&mut self.active_env, pd_val.clone());

        let result = self.eval_pd_expr(&parserdef);
        let result = self.strip_error(result);

        let mut ret = self.set_pd_ret(result);
        if self.call_needs_fixpoint.contains(&self.depth) {
            ret = self.eval_pd_fixpoint(ret, &parserdef);
            self.call_needs_fixpoint.remove(&self.depth);
        }
        if self.call_needs_fixpoint.is_empty() {
            // update the epoch since we are not in a fixpoint computation
            // so that the current value does not get invalidated
            // in case we do enter a fixpoint computation later
            self.cache_epoch.update();
        }

        self.active_env = old_active_env;
        self.current_ambience = old_ambienece;
        self.current_pd = old_pd;
        self.active_calls.remove(&pd_val);
        self.depth -= 1;

        ret
    }

    pub fn var_by_id(&mut self, id: DefId) -> Result<Dom, Dom::Err> {
        match self.block_vars.get(&id) {
            Some(v) => Ok(v.clone()),
            None => dbpanic!(self.db, "Did not find variable {}", &id,),
        }
    }

    fn set_block_var(&mut self, id: DefId, val: Dom) {
        self.block_vars.insert(id, val);
    }

    pub fn active_env(&self) -> &Dom {
        &self.active_env
    }

    pub fn active_pd(&self) -> &Dom {
        &self.current_pd
    }

    pub fn active_ambience(&self) -> &Option<Dom> {
        &self.current_ambience
    }

    fn eval_block_impl(&mut self, block_id: hir::BlockId) -> Result<Dom, Dom::Err> {
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
            let val = match self.db.hir_node(subvalue.id)? {
                hir::HirNode::Let(statement) => {
                    let expr = Resolved::fetch_expr(self.db, statement.expr)?;
                    let res_expr = self.eval_expr(expr.take_ref())?;
                    let res = res_expr.root_data();
                    self.set_block_var(statement.expr.0, res.clone());
                    self.block_expr_vals
                        .insert(statement.expr, res_expr.clone());
                    res.clone()
                }
                hir::HirNode::Parse(statement) => {
                    let expr = Resolved::fetch_expr(self.db, statement.expr)?;
                    let (res, res_expr) = self.eval_expr_with_ambience(expr.take_ref())?;
                    self.set_block_var(statement.expr.0, res_expr.root_data().clone());
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
                            match acc {
                                None => Ok(Some(new)),
                                Some(acc) => Ok(Some(acc.join(self, new)?.0)),
                            }
                        },
                    )?
                    .unwrap(),
                _ => continue,
            };
            self.set_block_var(subvalue.id, val);
        }

        match block.returns {
            BlockReturnKind::Returns => {
                let ret = block.root_context.0.child_field(self.db, FieldName::Return);
                let ret = self.var_by_id(ret)?;
                return Ok(ret);
            }
            BlockReturnKind::Nothing => {
                return Ok(Dom::unit(&mut self.dcx));
            }
            BlockReturnKind::Fields => {}
        }

        let root_context = block.root_context.lookup(self.db)?;
        let vars = root_context
            .vars
            .iter()
            .map(|(name, id)| Ok((*name, self.var_by_id(*id.inner())?)))
            .collect::<Result<FxHashMap<_, _>, Dom::Err>>()?;
        Dom::make_block(self, block_id, &vars)
    }

    pub fn eval_block(
        &mut self,
        block_id: hir::BlockId,
        block: Dom,
        from: Option<Dom>,
    ) -> Option<Dom> {
        if let Some(val) = self.block_result.get(&(from.clone(), block.clone())) {
            if let Some(block_info) = val.val_with_epoch(self.cache_epoch) {
                return block_info.as_ref().map(|x| x.returned.clone());
            }
        }
        let mut old_block_vars = std::mem::take(&mut self.block_vars);
        let mut old_block_expr = std::mem::take(&mut self.block_expr_vals);
        let old_env = std::mem::replace(&mut self.active_env, block.clone());
        let old_ambience = std::mem::replace(&mut self.current_ambience, from.clone());

        let res = self.eval_block_impl(block_id);
        let res = self.strip_error(res);

        self.current_ambience = old_ambience;
        self.active_env = old_env;
        std::mem::swap(&mut self.block_expr_vals, &mut old_block_expr);
        std::mem::swap(&mut self.block_vars, &mut old_block_vars);

        let evaluated = res.clone().map(|returned| BlockEvaluated {
            id: block_id,
            expr_vals: old_block_expr,
            from: from.clone(),
            vals: old_block_vars,
            returned,
        });
        self.block_result.insert(
            (from.clone(), block),
            EpochVal::new(evaluated, self.cache_epoch),
        );

        res
    }

    pub fn eval_lambda_impl(
        &mut self,
        lambda_id: hir::LambdaId,
    ) -> Result<LambdaEvaluated<Dom>, Dom::Err> {
        let lambda = lambda_id.lookup(self.db)?;
        let expr = Resolved::fetch_expr(self.db, lambda.expr)?;
        let expr_res = self.eval_expr(expr.take_ref())?;
        let ret = expr_res.root_data().clone();
        let ret = LambdaEvaluated {
            returned: ret,
            expr_vals: expr_res,
        };
        Ok(ret)
    }

    pub fn eval_lambda(&mut self, lambda_id: hir::LambdaId, lambda: Dom) -> Option<Dom> {
        let old_env = std::mem::replace(&mut self.active_env, lambda.clone());

        let res = self.eval_lambda_impl(lambda_id);
        let res = self.strip_error(res);

        self.active_env = old_env;

        let ret = res.as_ref().map(|evaluated| evaluated.returned.clone());
        if let Some(evaluated) = res {
            self.lambda_result
                .insert(lambda, EpochVal::new(Some(evaluated), self.cache_epoch));
        }

        ret
    }

    pub fn apply_thunk_arg(
        &mut self,
        pd_id: hir::ParserDefId,
        from: Dom,
        thunk_args: &[Dom],
    ) -> Result<Dom, Dom::Err> {
        let mut args = FxHashMap::default();
        args.insert(Arg::From, from);
        let pd = pd_id.lookup(self.db)?;
        for (idx, arg) in thunk_args.iter().enumerate() {
            let def = pd.args.as_ref().unwrap()[idx].0;
            args.insert(Arg::Named(def), arg.clone());
        }
        let pd = Dom::make_thunk(self, pd_id, &args)?;
        Ok(pd)
    }
}
