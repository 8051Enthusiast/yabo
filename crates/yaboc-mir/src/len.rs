use std::{rc::Rc, sync::Arc};

use fxhash::{FxHashMap, FxHashSet};
use yaboc_base::{
    error::{SResult, Silencable},
    interner::DefId,
};
use yaboc_constraint::{LenVal, LenVals};
use yaboc_constraint::{Origin, PdLenTerm};
use yaboc_dependents::{
    requirements::{NeededBy, RequirementMatrix},
    BacktrackStatus, BlockSerialization, SubValue, SubValueKind,
};
use yaboc_expr::{ExprHead, ExprIdx, Expression, FetchExpr, TakeRef};
use yaboc_hir::{
    BlockId, ContextId, ExprId, HirIdWrapper, HirNode, LetStatement, ParseStatement, ParserDef,
    ParserDefId, ParserPredecessor, StructChoice,
};
use yaboc_hir_types::FullTypeId;
use yaboc_len::Val;
use yaboc_resolve::expr::Resolved;
use yaboc_resolve::expr::ValVarOp;
use yaboc_types::{Type, TypeId};

use crate::{
    expr::ConvertExpr, ExceptionRetreat, Function, FunctionWriter, IntBinOp, Mirs, Place,
    PlaceInfo, PlaceOrigin, PlaceRef, ReturnStatus,
};

pub struct LenMirCtx<'a> {
    w: ConvertExpr<'a>,
    db: &'a dyn Mirs,
    vals: Arc<LenVals>,
    terms: Arc<PdLenTerm>,
    expr_use: FxHashMap<ExprId, Rc<[bool]>>,
    // the vectors in this hash table are in reverse order
    subctx_rev_orders: FxHashMap<ContextId, Vec<SubValue>>,
    is_dep: FxHashSet<SubValue>,
    int: TypeId,
    block: Option<BlockId>,
}

impl<'a> LenMirCtx<'a> {
    fn expr_use(&mut self, expr_id: ExprId) -> SResult<Rc<[bool]>> {
        if let Some(used) = self.expr_use.get(&expr_id) {
            return Ok(used.clone());
        }
        let expr = Resolved::expr_with_data::<BacktrackStatus>(self.db, expr_id)?;
        let expr_ref = expr.take_ref();
        let mut used = vec![false; expr_ref.len()];
        *used.last_mut().unwrap() = true;
        for (idx, (term, _)) in expr_ref.iter_parts().enumerate().rev() {
            if !used[idx] {
                continue;
            }
            if let ExprHead::Variadic(ValVarOp::PartialApply(_), args) = term {
                let call_site = Origin::Expr(expr_id, ExprIdx::new_from_usize(idx));
                let call_deps = self.vals.call_sites[&call_site].clone();
                used[args[0].as_usize()] = true;
                for (i, arg_idx) in args[1..].iter().enumerate() {
                    let is_used = call_deps[i];
                    used[arg_idx.as_usize()] = is_used;
                }
            } else {
                for arg_idx in term.inner_refs() {
                    used[arg_idx.as_usize()] = true;
                }
            }
        }
        let used: Rc<[bool]> = used.into();
        self.expr_use.insert(expr_id, used.clone());
        Ok(used)
    }

    fn expr_deps(&mut self, expr_id: ExprId) -> SResult<()> {
        let expr = Resolved::fetch_expr(self.db, expr_id)?;
        let used = self.expr_use(expr_id)?;
        for term in expr
            .take_ref()
            .iter_parts()
            .zip(used.iter())
            .filter_map(|(t, u)| u.then_some(t))
        {
            yaboc_dependents::add_term_val_refs(self.db, self.block, &term, |val| {
                self.is_dep.insert(SubValue::new_val(val));
            })
        }
        Ok(())
    }

    fn back_val_at_id(&self, id: DefId) -> LenVal {
        let subval = SubValue::new_back(id);
        let len_expr_idx = self.terms.vals[&subval];
        self.vals.vals[len_expr_idx].clone()
    }

    fn val_at_id(&self, id: DefId) -> LenVal {
        let subval = SubValue::new_val(id);
        let len_expr_idx = self.terms.vals[&subval];
        self.vals.vals[len_expr_idx].clone()
    }

    fn parse_deps(&mut self, parse: &ParseStatement) -> SResult<ContextId> {
        let val = self.back_val_at_id(parse.id.0);
        assert!(!val.is_dynamic());
        if !matches!(val, Val::Const(..)) {
            self.expr_deps(parse.expr)?;
        }
        if let ParserPredecessor::After(prev) = parse.front {
            self.is_dep.insert(SubValue::new_back(prev));
        }
        Ok(parse.parent_context)
    }

    fn let_deps(&mut self, let_statement: &LetStatement) -> SResult<ContextId> {
        let expr_id = let_statement.expr;
        let val = self.val_at_id(let_statement.id.0);
        assert!(!val.is_dynamic());
        self.expr_deps(expr_id)?;
        Ok(let_statement.context)
    }

    fn context_deps(&mut self, context_id: ContextId) -> SResult<()> {
        let context = context_id.lookup(self.db)?;
        if let Some((_, end)) = context.endpoints {
            self.is_dep.insert(SubValue::new_back(end));
        }
        Ok(())
    }

    fn choice_deps(&mut self, choice: &StructChoice) -> SResult<ContextId> {
        let val = self.back_val_at_id(choice.id.0);
        assert!(!val.is_dynamic());
        for subcontext in &choice.subcontexts {
            self.context_deps(*subcontext)?;
        }
        if let Some([ParserPredecessor::After(prev), _]) = choice.endpoints {
            self.is_dep.insert(SubValue::new_back(prev));
        }
        Ok(choice.parent_context)
    }

    fn block_len_deps(&mut self, root_context: ContextId, s: &BlockSerialization) -> SResult<()> {
        self.context_deps(root_context)?;
        for subval in s.eval_order.iter().rev() {
            if !self.is_dep.contains(&subval.val) {
                continue;
            }
            let node = self.db.hir_node(subval.val.id)?;
            let parent_context = match (node, subval.val.kind) {
                (HirNode::Parse(p), SubValueKind::Back) => self.parse_deps(&p)?,
                (HirNode::Let(l), SubValueKind::Val) => self.let_deps(&l)?,
                (HirNode::Choice(c), SubValueKind::Back) => self.choice_deps(&c)?,
                _ => panic!("unexpected node kind"),
            };
            self.subctx_rev_orders
                .entry(parent_context)
                .or_default()
                .push(subval.val);
        }
        Ok(())
    }

    fn init_captures(&mut self, block: BlockId) -> SResult<()> {
        let captures = self.db.captures(block);
        for capture in captures.iter() {
            let ty = self.db.parser_type_at(*capture)?;
            let place = self.w.f.add_place(PlaceInfo {
                place: Place::Captured(self.w.f.fun.cap(), *capture),
                ty,
                remove_bt: false,
            });
            self.w.register_place(SubValue::new_val(*capture), place);
        }
        Ok(())
    }

    fn init_args(&mut self, pd: &ParserDef) -> SResult<()> {
        let arg_tys = self.db.parser_args(pd.id)?.args.unwrap_or_default();
        let arg_ids = pd.args.clone().unwrap_or_default();
        for (arg_id, arg_ty) in arg_ids.iter().zip(arg_tys.iter()) {
            let place = self.w.f.add_place(PlaceInfo {
                place: Place::Captured(self.w.f.fun.cap(), arg_id.0),
                ty: *arg_ty,
                remove_bt: false,
            });
            self.w.register_place(SubValue::new_val(arg_id.0), place);
        }
        Ok(())
    }

    fn build_expr(&mut self, expr_id: ExprId) -> SResult<PlaceRef> {
        let used = self.expr_use(expr_id)?;
        let expr = Resolved::expr_with_data::<((ExprIdx<Resolved>, FullTypeId), RequirementMatrix)>(
            self.db, expr_id,
        )?;
        self.w
            .convert_expr(expr_id, &expr, None, |idx| used[idx.as_usize()], NeededBy::Val.into())
    }

    fn build_let(&mut self, subval: SubValue, let_statement: &LetStatement) -> SResult<()> {
        let ty = self.db.parser_type_at(let_statement.id.0)?;
        let origin = PlaceOrigin::Node(let_statement.id.0);
        let place = self.w.f.new_stack_place(ty, origin);
        self.w.register_place(subval, place);
        let expr_place = self.build_expr(let_statement.expr)?;
        self.w.copy(expr_place, place);
        Ok(())
    }

    fn build_parse(&mut self, subval: SubValue, parse: &ParseStatement) -> SResult<()> {
        let origin = PlaceOrigin::Ambient(self.block.unwrap(), parse.id.0);
        let len_place = self.w.f.new_stack_place(self.int, origin);
        let val = self.back_val_at_id(parse.id.0);
        if let Val::Const(0, c) = val {
            let n = i64::try_from(c).unwrap();
            self.w.f.load_int(n, len_place);
        } else {
            let expr_place = self.build_expr(parse.expr)?;
            let expr_tys = self.db.parser_expr_at(parse.expr)?;
            let expr_ty = *expr_tys.root_data();
            let ldt_ty = self.db.least_deref_type(expr_ty)?;
            let ldt_place = if ldt_ty != expr_ty {
                let expr_origin = PlaceOrigin::Node(parse.expr.0);
                let ldt_place = self.w.f.new_stack_place(ldt_ty, expr_origin);
                self.w.copy(expr_place, ldt_place);
                ldt_place
            } else {
                expr_place
            };
            self.w.f.len_call(ldt_place, len_place, self.w.retreat);
        }

        if let ParserPredecessor::After(prev) = parse.front {
            let prev_place = self.w.back_place_at_def(prev).unwrap();
            let sum_place = self.w.f.new_stack_place(self.int, origin);
            self.w
                .f
                .int_bin_op(sum_place, IntBinOp::Plus, prev_place, len_place);
            self.w.register_place(subval, sum_place);
        } else {
            self.w.register_place(subval, len_place);
        }

        Ok(())
    }

    fn build_choice(&mut self, subval: SubValue, choice: &StructChoice) -> SResult<()> {
        let origin = PlaceOrigin::Ambient(self.block.unwrap(), choice.id.0);
        let len_place = self.w.f.new_stack_place(self.int, origin);
        let val = self.back_val_at_id(choice.id.0);
        let cont_bb = self.w.f.new_bb();
        if let Val::Const(0, c) = val {
            let n = i64::try_from(c).unwrap();
            self.w.f.load_int(n, len_place);
        } else {
            let start_bb = self.w.f.current_bb;
            let mut next_bb = cont_bb;
            let original_retreat = self.w.retreat;

            for context in choice.subcontexts.iter().rev() {
                let current_bb = self.w.f.new_bb();
                self.w.f.set_bb(current_bb);
                if next_bb != cont_bb {
                    self.w.retreat = ExceptionRetreat {
                        backtrack: next_bb,
                        eof: next_bb,
                        error: next_bb,
                    };
                }
                self.build_context(*context, len_place)?;
                self.w.f.branch(cont_bb);

                next_bb = current_bb;
            }

            self.w.f.set_bb(start_bb);
            self.w.f.branch(next_bb);
            self.w.f.set_bb(cont_bb);
            self.w.retreat = original_retreat;
        }

        if let Some([ParserPredecessor::After(prev), _]) = choice.endpoints {
            let prev_place = self.w.back_place_at_def(prev).unwrap();
            let sum_place = self.w.f.new_stack_place(self.int, origin);
            self.w
                .f
                .int_bin_op(sum_place, IntBinOp::Plus, prev_place, len_place);
            self.w.register_place(subval, sum_place);
        } else {
            self.w.register_place(subval, len_place);
        }

        Ok(())
    }

    fn build_context(&mut self, context_id: ContextId, end_place: PlaceRef) -> SResult<()> {
        let context = context_id.lookup(self.db)?;
        let Some((_, last)) = context.endpoints else {
            self.w.f.load_int(0, end_place);
            return Ok(());
        };
        for subval in self
            .subctx_rev_orders
            .remove(&context_id)
            .unwrap()
            .into_iter()
            .rev()
        {
            match self.db.hir_node(subval.id)? {
                yaboc_hir::HirNode::Let(l) => self.build_let(subval, &l)?,
                yaboc_hir::HirNode::Parse(p) => self.build_parse(subval, &p)?,
                yaboc_hir::HirNode::Choice(c) => self.build_choice(subval, &c)?,
                _ => panic!("unexpected node in len dependencies"),
            }
        }
        let last_place = self.w.back_place_at_def(last).unwrap();
        self.w.f.copy(last_place, end_place, self.w.retreat.error);
        Ok(())
    }

    pub fn new_block(db: &dyn Mirs, id: BlockId) -> SResult<Function> {
        let block = id.lookup(db)?;
        let mut f = FunctionWriter::new_block(db, &block, NeededBy::Val.into())?;
        let retreat = f.make_top_level_retreat();
        f.set_bb(f.fun.entry());
        let mut w = ConvertExpr::new(db, f, retreat, Default::default());
        let pd = db.hir_parent_parserdef(id.0)?;
        let terms = db.len_term(pd)?;
        let block_idx = terms.block_locs[&id];
        let vals = db.len_vals(pd);
        // this is needed for soundness as otherwise we could end up
        // with false dependencies on undefined arguments/captures
        if let Val::Const(0, c) = vals.vals[block_idx] {
            let n = i64::try_from(c).unwrap();
            w.f.load_int(n, w.f.fun.ret().unwrap());
            w.f.ret(ReturnStatus::Ok);
            return Ok(w.f.fun);
        }
        if let Val::Dynamic = vals.vals[block_idx] {
            w.f.ret(ReturnStatus::Error);
            return Ok(w.f.fun);
        }
        // we have to check with static values whether they depend
        // on a parsed value and if they do, we must return an error
        // as we can not calculate it without the argument
        if let Val::Static(..) = vals.vals[block_idx] {
            let arg_deps = db.len_arg_deps(pd)?;
            let arg_count = db.argnum(pd)?.unwrap_or_default();
            if arg_deps[block_idx][arg_count] {
                w.f.ret(ReturnStatus::Error);
                return Ok(w.f.fun);
            }
        }
        let mut lenctx = LenMirCtx {
            db,
            w,
            block: Some(id),
            subctx_rev_orders: FxHashMap::default(),
            int: db.int(),
            vals,
            terms,
            expr_use: Default::default(),
            is_dep: Default::default(),
        };
        lenctx.init_captures(id)?;
        let s = lenctx.db.block_serialization(id).silence()?;
        let root_context = block.root_context.lookup(db)?;
        lenctx.block_len_deps(root_context.id, &s)?;

        let ret = lenctx.w.f.fun.ret().unwrap();
        lenctx.build_context(root_context.id, ret)?;
        lenctx.w.f.ret(ReturnStatus::Ok);
        Ok(lenctx.w.f.fun)
    }

    pub fn new_pd(db: &dyn Mirs, id: ParserDefId) -> SResult<Function> {
        let pd = id.lookup(db)?;
        let mut f = FunctionWriter::new_pd(db, id, NeededBy::Val.into())?;
        let retreat = f.make_top_level_retreat();
        f.set_bb(f.fun.entry());
        let mut w = ConvertExpr::new(db, f, retreat, Default::default());
        let terms = db.len_term(id)?;
        let vals = db.len_vals(id);
        if let Val::Const(_, c) = vals.fun_val {
            let n = i64::try_from(c).unwrap();
            w.f.load_int(n, w.f.fun.ret().unwrap());
            w.f.ret(ReturnStatus::Ok);
            return Ok(w.f.fun);
        }
        if let Val::Dynamic = vals.fun_val {
            w.f.ret(ReturnStatus::Error);
            return Ok(w.f.fun);
        }
        let mut lenctx = LenMirCtx {
            db,
            w,
            block: None,
            subctx_rev_orders: FxHashMap::default(),
            int: db.int(),
            vals,
            terms,
            expr_use: Default::default(),
            is_dep: Default::default(),
        };
        lenctx.init_args(&pd)?;
        let expr_place = lenctx.build_expr(pd.to)?;
        lenctx
            .w
            .f
            .len_call(expr_place, lenctx.w.f.fun.ret().unwrap(), retreat);
        lenctx.w.f.ret(ReturnStatus::Ok);
        Ok(lenctx.w.f.fun)
    }

    pub fn new_if(db: &'a dyn Mirs, ty: TypeId) -> SResult<Function> {
        let Type::ParserArg { result, arg } = db.lookup_intern_type(ty) else {
            panic!("Expected ParserArg, got {ty:?}")
        };
        let mut f = FunctionWriter::new(ty, Some(arg), result, NeededBy::Val.into());
        let top_level_retreat = f.make_top_level_retreat();
        let inner_fun_place = f.add_place(PlaceInfo {
            place: Place::Front(f.fun.cap()),
            ty,
            remove_bt: false,
        });
        f.set_bb(f.fun.entry());
        f.len_call(inner_fun_place, f.fun.ret().unwrap(), top_level_retreat);
        f.ret(ReturnStatus::Ok);
        Ok(f.fun)
    }
}
