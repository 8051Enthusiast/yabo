mod represent;

use std::num::NonZeroU64;

use hir::{BlockId, Hirs};
use yaboc_ast::expr::BtMarkKind;
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

    fn update(&mut self) -> Self {
        *self = self.next();
        return *self;
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

pub trait AbstractDomain<'a>:
    Sized + Clone + std::hash::Hash + Eq + std::fmt::Debug + DatabasedDisplay<Self::DB>
{
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
pub struct PdEvaluated<Dom> {
    pub returned: Dom,
    pub from: Option<Dom>,
    pub expr_vals: Option<AbstractData<Dom>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LambdaEvaluated<Dom> {
    pub returned: Dom,
    pub expr_vals: Option<AbstractData<Dom>>,
}

impl<Dom: Clone + std::hash::Hash + Eq + std::fmt::Debug> From<LambdaEvaluated<Dom>>
    for PdEvaluated<Dom>
{
    fn from(lambda: LambdaEvaluated<Dom>) -> Self {
        Self {
            returned: lambda.returned,
            from: None,
            expr_vals: lambda.expr_vals,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockEvaluated<Dom> {
    pub id: BlockId,
    pub expr_vals: FxHashMap<hir::ExprId, AbstractData<Dom>>,
    pub from: Option<Dom>,
    pub vals: FxHashMap<DefId, Dom>,
    pub returned: Dom,
}

#[derive(Clone, Copy)]
enum EvaluationInput<'a> {
    ParserDef(&'a hir::ParserDef),
    Block(BlockId),
    Lambda(hir::LambdaId),
}

enum Evaluated<Dom> {
    ParserDef(PdEvaluated<Dom>),
    Lambda(LambdaEvaluated<Dom>),
    Block(BlockEvaluated<Dom>),
}

impl<Dom> Evaluated<Dom> {
    fn returned(&self) -> &Dom {
        match self {
            Evaluated::ParserDef(pd_evaluated) => &pd_evaluated.returned,
            Evaluated::Lambda(lambda_evaluated) => &lambda_evaluated.returned,
            Evaluated::Block(block_evaluated) => &block_evaluated.returned,
        }
    }

    fn returned_mut(&mut self) -> &mut Dom {
        match self {
            Evaluated::ParserDef(pd_evaluated) => &mut pd_evaluated.returned,
            Evaluated::Lambda(lambda_evaluated) => &mut lambda_evaluated.returned,
            Evaluated::Block(block_evaluated) => &mut block_evaluated.returned,
        }
    }
}

type CallSite<Dom> = (Option<Dom>, Dom);

pub struct AbsIntCtx<'a, Dom: AbstractDomain<'a>> {
    pub db: &'a Dom::DB,
    pub dcx: Dom::DomainContext,

    // values in the current epoch might change later
    // because of fixpoint iteration, but values from
    // previous epochs are stable
    cache_epoch: Epoch,
    active_calls: FxHashMap<CallSite<Dom>, usize>,
    depth: usize,
    call_needs_fixpoint: FxHashSet<usize>,

    current_ambience: Option<Dom>,

    results: FxHashMap<CallSite<Dom>, MaybeEpoch<Evaluated<Dom>>>,
    current_pd: Dom,

    block_vars: FxHashMap<DefId, Dom>,
    block_expr_vals: FxHashMap<hir::ExprId, AbstractData<Dom>>,
    active_env: Dom,

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
            block_vars: Default::default(),
            block_expr_vals: Default::default(),
            active_env: bottom,
            results: Default::default(),
            errors: Default::default(),
        }
    }

    pub fn pd_result(&self, val: &Dom) -> Option<&PdEvaluated<Dom>> {
        let result = self
            .results
            .get(&(None, val.clone()))?
            .val_with_epoch(self.cache_epoch)?
            .as_ref()?;
        match result {
            Evaluated::ParserDef(pd_evaluated) => Some(&pd_evaluated),
            _ => panic!("Not a pd result!"),
        }
    }

    pub fn block_result(&self, val: &CallSite<Dom>) -> Option<&BlockEvaluated<Dom>> {
        let result = self
            .results
            .get(val)?
            .val_with_epoch(self.cache_epoch)?
            .as_ref()?;
        match result {
            Evaluated::Block(block) => Some(&block),
            _ => panic!("Not a block result!"),
        }
    }

    pub fn lambda_result(&self, val: &Dom) -> Option<&LambdaEvaluated<Dom>> {
        let result = self
            .results
            .get(&(None, val.clone()))?
            .val_with_epoch(self.cache_epoch)?
            .as_ref()?;
        match result {
            Evaluated::Lambda(lambda) => Some(lambda),
            _ => panic!("Not a lambda result!"),
        }
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
        bt: Option<BtMarkKind>,
    ) -> Result<(Dom, AbstractData<Dom>), Dom::Err> {
        let res = self.eval_expr(expr)?;
        let root = res.root_data().clone();
        let Some(from) = self.current_ambience.clone() else {
            panic!("called eval_expr_with_ambience without ambience")
        };
        let applied = ExprHead::new_dyadic(expr::ValBinOp::ParserApply(bt), [&from, &root]);
        let ret_val = Dom::eval_expr(self, applied)?;
        let normalized = ret_val.normalize(self)?.0;
        Ok((normalized, res))
    }

    fn eval_pd_expr(&mut self, parserdef: &hir::ParserDef) -> Result<PdEvaluated<Dom>, Dom::Err> {
        let expr = Resolved::fetch_expr(self.db, parserdef.to)?;
        let ((ret_val, expr_vals), from) = if let Some(from) = self.current_ambience.clone() {
            (
                self.eval_expr_with_ambience(expr.take_ref(), parserdef.bt)?,
                Some(from),
            )
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

    fn set_result(
        &mut self,
        mut evaluated: Option<Evaluated<Dom>>,
        site: CallSite<Dom>,
    ) -> Option<Dom> {
        let ret = evaluated.as_mut().map(|x| x.returned().clone());
        let epoch = self.cache_epoch;
        self.results.insert(site, EpochVal::new(evaluated, epoch));
        ret
    }

    fn eval_impl(&mut self, input: EvaluationInput<'_>) -> Result<Evaluated<Dom>, Dom::Err> {
        match input {
            EvaluationInput::ParserDef(parserdef) => {
                self.eval_pd_expr(parserdef).map(Evaluated::ParserDef)
            }
            EvaluationInput::Block(block_id) => {
                self.eval_block_impl(block_id).map(Evaluated::Block)
            }
            EvaluationInput::Lambda(lambda_id) => {
                self.eval_lambda_impl(lambda_id).map(Evaluated::Lambda)
            }
        }
    }

    fn eval_fixpoint(
        &mut self,
        mut ret: Option<Dom>,
        input: EvaluationInput<'_>,
        site: CallSite<Dom>,
    ) -> Option<Dom> {
        loop {
            let old_ret = ret?;
            let evaluated = self.eval_impl(input);
            let widened_return = self.strip_error(evaluated).and_then(|mut evaluated| {
                let widened = old_ret.widen(self, evaluated.returned().clone());
                let (ret, changed) = self.strip_error(widened)?;
                *evaluated.returned_mut() = ret;
                Some((evaluated, changed))
            });
            ret = widened_return.as_ref().map(|(x, _)| x.returned().clone());
            let stop = widened_return
                .as_ref()
                .map(|(_, changed)| !*changed)
                .unwrap_or(true);
            self.set_result(widened_return.map(|x| x.0), site.clone());
            if stop {
                return ret;
            }
        }
    }

    fn bottom_evaluated(&mut self, input: EvaluationInput) -> Evaluated<Dom> {
        let bottom = Dom::bottom(&mut self.dcx);
        match input {
            EvaluationInput::ParserDef(parserdef) => Evaluated::ParserDef(PdEvaluated {
                returned: bottom.clone(),
                from: parserdef.from.is_some().then(|| bottom.clone()),
                expr_vals: None,
            }),
            EvaluationInput::Block(block_id) => Evaluated::Block(BlockEvaluated {
                id: block_id,
                expr_vals: Default::default(),
                from: None,
                vals: Default::default(),
                returned: bottom,
            }),
            EvaluationInput::Lambda(_) => Evaluated::Lambda(LambdaEvaluated {
                returned: bottom.clone(),
                expr_vals: None,
            }),
        }
    }

    fn enter_eval(&mut self, input: EvaluationInput<'_>, site: CallSite<Dom>) -> Option<Dom> {
        let mut epoch = self.cache_epoch;
        if let Some(val) = self.results.get(&site) {
            if let Some(pd) = val.val_with_epoch(epoch) {
                return Some(pd.as_ref()?.returned().clone());
            }
        }
        if let Some(&depth) = self.active_calls.get(&site) {
            if self.call_needs_fixpoint.is_empty() {
                // we can only increment the epoch if we are not
                // currently in a fixpoint computation
                epoch = self.cache_epoch.update();
            }
            self.call_needs_fixpoint.insert(depth);
            if let Some(val) = self.results.get(&site) {
                return val.val().as_ref().map(|x| x.returned().clone());
            }
            let bottom = self.bottom_evaluated(input);
            let bottom_return = bottom.returned().clone();
            self.results
                .insert(site, EpochVal::new(Some(bottom), epoch));
            return Some(bottom_return);
        }

        self.depth += 1;
        self.active_calls.insert(site.clone(), self.depth);
        if self.call_needs_fixpoint.contains(&self.depth) {
            panic!("contains fixpoint depth that was expected to be removed")
        }

        None
    }

    fn do_eval(&mut self, input: EvaluationInput<'_>, site: CallSite<Dom>) -> Option<Dom> {
        let result = self.eval_impl(input.clone());
        let result = self.strip_error(result);

        let mut ret = self.set_result(result, site.clone());
        if self.call_needs_fixpoint.contains(&self.depth) {
            ret = self.eval_fixpoint(ret, input, site);
            self.call_needs_fixpoint.remove(&self.depth);
        }
        ret
    }

    fn leave_eval(&mut self, site: CallSite<Dom>) {
        if self.call_needs_fixpoint.is_empty() {
            // update the epoch since we are not in a fixpoint computation
            // so that the current value does not get invalidated
            // in case we do enter a fixpoint computation later
            self.cache_epoch.update();
        }

        self.depth -= 1;
        self.active_calls.remove(&site);
    }

    pub fn eval_pd(&mut self, pd_val: Dom, pd: ParserDefId) -> Option<Dom> {
        let parserdef = pd.lookup(self.db).ok()?;
        if let Some(cached) = self.enter_eval(
            EvaluationInput::ParserDef(&parserdef),
            (None, pd_val.clone()),
        ) {
            return Some(cached);
        }
        let old_pd = std::mem::replace(&mut self.current_pd, pd_val.clone());
        let from = if parserdef.from.is_some() {
            pd_val.clone().get_arg(self, Arg::From).ok()
        } else {
            None
        };
        let old_ambience = std::mem::replace(&mut self.current_ambience, from);
        if self.call_needs_fixpoint.contains(&self.depth) {
            panic!("contains fixpoint depth that was expected to be removed")
        }
        let old_active_env = std::mem::replace(&mut self.active_env, pd_val.clone());

        let ret = self.do_eval(
            EvaluationInput::ParserDef(&parserdef),
            (None, pd_val.clone()),
        );
        self.active_env = old_active_env;
        self.current_ambience = old_ambience;
        self.current_pd = old_pd;
        self.leave_eval((None, pd_val.clone()));

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

    fn eval_block_impl(&mut self, block_id: hir::BlockId) -> Result<BlockEvaluated<Dom>, Dom::Err> {
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
                    let (res, res_expr) =
                        self.eval_expr_with_ambience(expr.take_ref(), statement.bt)?;
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

        let returned = match block.returns {
            BlockReturnKind::Returns => {
                let ret = block.root_context.0.child_field(self.db, FieldName::Return);
                self.var_by_id(ret)?
            }
            BlockReturnKind::Nothing => Dom::unit(&mut self.dcx),
            BlockReturnKind::Fields => {
                let root_context = block.root_context.lookup(self.db)?;
                let vars = root_context
                    .vars
                    .iter()
                    .map(|(name, id)| Ok((*name, self.var_by_id(*id.inner())?)))
                    .collect::<Result<FxHashMap<_, _>, Dom::Err>>()?;
                Dom::make_block(self, block_id, &vars)?
            }
        };

        Ok(BlockEvaluated {
            id: block_id,
            expr_vals: std::mem::take(&mut self.block_expr_vals),
            from: self.active_ambience().clone(),
            vals: std::mem::take(&mut self.block_vars),
            returned,
        })
    }

    pub fn eval_block(
        &mut self,
        block_id: hir::BlockId,
        block: Dom,
        from: Option<Dom>,
    ) -> Option<Dom> {
        if let Some(val) = self.enter_eval(
            EvaluationInput::Block(block_id),
            (from.clone(), block.clone()),
        ) {
            return Some(val);
        }
        let old_block_vars = std::mem::take(&mut self.block_vars);
        let old_block_expr = std::mem::take(&mut self.block_expr_vals);
        let old_env = std::mem::replace(&mut self.active_env, block.clone());
        let old_ambience = std::mem::replace(&mut self.current_ambience, from.clone());

        let res = self.do_eval(
            EvaluationInput::Block(block_id),
            (from.clone(), block.clone()),
        );

        self.current_ambience = old_ambience;
        self.active_env = old_env;
        self.block_expr_vals = old_block_expr;
        self.block_vars = old_block_vars;

        self.leave_eval((from.clone(), block.clone()));
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
            expr_vals: Some(expr_res),
        };
        Ok(ret)
    }

    pub fn eval_lambda(&mut self, lambda_id: hir::LambdaId, lambda: Dom) -> Option<Dom> {
        if let Some(cached) =
            self.enter_eval(EvaluationInput::Lambda(lambda_id), (None, lambda.clone()))
        {
            return Some(cached);
        }
        let old_env = std::mem::replace(&mut self.active_env, lambda.clone());

        let ret = self.do_eval(EvaluationInput::Lambda(lambda_id), (None, lambda.clone()));

        self.active_env = old_env;

        self.leave_eval((None, lambda));
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
