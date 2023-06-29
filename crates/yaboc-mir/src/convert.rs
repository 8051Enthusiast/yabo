use std::{
    collections::{hash_map::Entry, BTreeMap, BTreeSet},
    sync::Arc,
};

use fxhash::{FxHashMap, FxHashSet};

use hir::{HirConstraintId, HirNodeKind};
use yaboc_base::{
    dbpanic,
    error::SResult,
    interner::{DefId, FieldName, PathComponent},
};
use yaboc_dependents::{
    BlockSerialization, NeededBy, RequirementMatrix, RequirementSet, SubValue, SubValueKind,
};
use yaboc_expr::{ExprIdx, Expression, FetchExpr};
use yaboc_hir::{
    self as hir, variable_set::VarStatus, BlockId, ChoiceId, ContextId, ExprId, HirIdWrapper,
    HirNode, ParserDefId, ParserPredecessor,
};
use yaboc_hir_types::FullTypeId;
use yaboc_resolve::expr::Resolved;
use yaboc_types::{Type, TypeId};

use crate::{expr::ConvertExpr, CallMeta};

use super::{
    BBRef, ExceptionRetreat, Function, FunctionWriter, MirInstr, Mirs, Place, PlaceInfo,
    PlaceOrigin, PlaceRef, ReturnStatus,
};

pub struct ConvertCtx<'a> {
    w: ConvertExpr<'a>,
    db: &'a dyn Mirs,
    top_level_retreat: ExceptionRetreat,
    context_bb: FxHashMap<ContextId, (BBRef, BBRef)>,
    current_context: Option<ContextId>,
    context_data: FxHashMap<ContextId, ContextData>,
    processed_parse_sites: FxHashSet<DefId>,
    req: RequirementSet,
    req_transformer: Arc<BTreeMap<DefId, RequirementMatrix>>,
    tails: Arc<BTreeSet<DefId>>,
    returns_self: bool,
}

#[derive(Debug)]
pub struct ContextData {
    field_ids: FxHashMap<FieldName, DefId>,
    field_move_targets: FxHashMap<DefId, DefId>,
    affected_discriminants: FxHashSet<FieldName>,
    ends: Option<(DefId, DefId)>,
    backtracks_to: Option<ContextId>,
    parent_choice: Option<ChoiceId>,
}

impl ContextData {
    pub fn build_context_tree(
        db: &dyn Mirs,
        root: ContextId,
    ) -> SResult<FxHashMap<ContextId, ContextData>> {
        let mut hashmap = Default::default();
        let parent_fields = Default::default();
        let affected_discriminants = Default::default();
        Self::build_subcontext(
            db,
            root,
            &mut hashmap,
            &parent_fields,
            None,
            &affected_discriminants,
            None,
        )?;
        Ok(hashmap)
    }

    fn build_subcontext(
        db: &dyn Mirs,
        ctx: ContextId,
        map: &mut FxHashMap<ContextId, ContextData>,
        parent_fields: &FxHashMap<FieldName, DefId>,
        parent_choice: Option<ChoiceId>,
        affected_discriminants: &FxHashSet<FieldName>,
        backtracks_to: Option<ContextId>,
    ) -> SResult<()> {
        let mut affected_discriminants = affected_discriminants.clone();
        let context = ctx.lookup(db)?;
        let field_map: FxHashMap<FieldName, DefId> = context
            .vars
            .iter()
            .map(|(f, id)| (*f, *id.inner()))
            .collect();
        let field_move_targets: FxHashMap<DefId, DefId> = field_map
            .iter()
            .filter_map(|(field, id)| Some((*id, *parent_fields.get(field)?)))
            .collect();

        let mut subchoice_affected_discriminants: FxHashMap<_, FxHashSet<_>> = Default::default();
        let mut subchoices = FxHashMap::default();
        for child in context.children.iter() {
            let node = db.hir_node(*child)?;
            match node {
                hir::HirNode::Choice(c) => {
                    subchoices.insert(c.id, c.subcontexts.clone());
                }
                hir::HirNode::ChoiceIndirection(c) => {
                    let field = match db.lookup_intern_hir_path(c.id.0).pop() {
                        Some(PathComponent::Named(field)) => field,
                        _ => panic!("choice indirection not field!"),
                    };
                    match context
                        .vars
                        .get(field)
                        .expect("could not find variable of choice indirection in context")
                    {
                        VarStatus::Always(_) => continue,
                        VarStatus::Maybe(_) => {}
                    }
                    // move responsibility of setting discriminant to contexts inside subchoice
                    affected_discriminants.remove(&field);
                    subchoice_affected_discriminants
                        .entry(c.target_choice)
                        .or_default()
                        .insert(field);
                }
                _ => continue,
            };
        }
        for (choice, subcontx) in subchoices.iter() {
            let affected_discriminants = subchoice_affected_discriminants
                .remove(choice)
                .unwrap_or_default();
            for (i, &ctx) in subcontx.iter().enumerate() {
                let next_context = subcontx.get(i + 1).copied().or(backtracks_to);
                Self::build_subcontext(
                    db,
                    ctx,
                    map,
                    &field_map,
                    Some(*choice),
                    &affected_discriminants,
                    next_context,
                )?;
            }
        }
        map.insert(
            ctx,
            ContextData {
                ends: context.endpoints,
                field_ids: field_map,
                field_move_targets,
                affected_discriminants,
                backtracks_to,
                parent_choice,
            },
        );
        Ok(())
    }
}

impl<'a> ConvertCtx<'a> {
    // gets the bbs for the context and initializes if necessary
    fn context_bb(&mut self, context: ContextId) -> (BBRef, BBRef) {
        match self.context_bb.entry(context) {
            Entry::Occupied(o) => *o.get(),
            Entry::Vacant(v) => {
                let bb = self.w.f.new_bb();
                v.insert((bb, bb));
                (bb, bb)
            }
        }
    }

    pub fn change_context(&mut self, context: ContextId) {
        if Some(context) == self.current_context {
            return;
        }
        if let Some(ctx) = self.current_context {
            self.context_bb
                .get_mut(&ctx)
                .expect("uninitialized context")
                .1 = self.w.f.current_bb;
        }
        self.current_context = Some(context);
        let current_end = self.context_bb(context).1;
        self.w.f.set_bb(current_end);
        // if you end up with a crash here, that means a subvalue was used after
        // the context was terminated, which indicates a problem in dependents
        let context_backtrack = self.context_data[&context].backtracks_to;

        self.w.retreat.backtrack = if let Some(bt) = context_backtrack {
            self.context_bb(bt).0
        } else {
            self.top_level_retreat.backtrack
        }
    }

    fn terminate_context(&mut self, context: ContextId, superchoice: ChoiceId, cont: BBRef) {
        let Some(&(_, end_bb)) = self.context_bb.get(&context) else {
            // context was never referenced and contains nothing
            self.context_data
                .remove(&context)
                .expect("context not initialized or removed multiple times");
            return;
        };
        self.change_context(context);
        let context_data = self
            .context_data
            .remove(&context)
            .expect("context not initialized or removed multiple times");
        if let Some(MirInstr::ParseCall(.., None)) = self.w.f.fun.bb(end_bb).ins.last() {
            // we have a tail call, we cannot copy anything or set discriminants
            return;
        }
        for (from, to) in context_data.field_move_targets.iter() {
            let to_place = match self.w.val_place_at_def(*to) {
                Some(p) => p,
                None => continue,
            };
            let from_place = self.w.val_place_at_def(*from).unwrap();
            self.w.copy(from_place, to_place);
        }
        if self.returns_self {
            if let Some(ret) = self.w.f.fun.ret() {
                for field in context_data.affected_discriminants {
                    let has_field = context_data.field_ids.get(&field).is_some();
                    self.w
                        .f
                        .append_ins(MirInstr::SetDiscriminant(ret, field, has_field));
                }
            }
        }
        if let Some(to_place) = self.w.back_place_at_def(superchoice.0) {
            let from_place = if let Some((_, end)) = context_data.ends {
                self.w.back_place_at_def(end)
            } else {
                self.w.front_place_at_def(superchoice.0)
            }
            .expect("could not find last parse end of context");
            self.w.copy(from_place, to_place);
        }
        self.w.f.branch(cont);
    }

    fn end_choice(&mut self, subcontexts: &[ContextId], id: ChoiceId) {
        let parent_context = self
            .current_context
            .expect("end choice called without context");
        let first_context_bb = match subcontexts.get(0) {
            Some(x) => self.context_bb[x].0,
            None => return,
        };
        self.w.f.branch(first_context_bb);
        let cont = self.w.f.new_bb();
        self.w.f.set_bb(cont);
        for context in subcontexts.iter() {
            self.terminate_context(*context, id, cont)
        }
        self.change_context(parent_context);
    }

    fn let_statement(&mut self, statement: &hir::LetStatement) {
        let target = self
            .w
            .val_place_at_def(statement.id.0)
            .expect("let statement has no place");
        let expr_val = self
            .w
            .val_place_at_def(statement.expr.0)
            .expect("let statement's expression has no place");
        self.w.copy(expr_val, target);
    }

    fn call_expr(&mut self, call_loc: DefId, expr: ExprId, call_info: CallMeta) -> SResult<()> {
        let parser_fun = self.w.val_place_at_def(expr.0).unwrap();
        let parser_ty = self.w.f.fun.place(parser_fun).ty;
        let ldt_parser = self.db.least_deref_type(parser_ty)?;
        let ldt_parser_fun = self.w.copy_if_different_levels(
            ldt_parser,
            parser_ty,
            None,
            PlaceOrigin::Node(expr.0),
            |_, _| Ok(parser_fun),
        )?;
        let addr = self.w.front_place_at_def(call_loc).unwrap();
        let ret = if call_info.req.contains(NeededBy::Val) {
            if call_info.tail {
                self.w.f.fun.ret()
            } else {
                Some(self.w.val_place_at_def(call_loc).unwrap())
            }
        } else {
            None
        };
        let ty = self.w.f.fun.place(self.w.f.fun.arg()).ty;
        let retlen = if call_info.req.contains(NeededBy::Len) {
            if call_info.tail {
                self.w.f.fun.retlen()
            } else {
                Some(self.w.create_current_ins_retlen_place(ty, call_loc))
            }
        } else {
            None
        };
        if call_info.tail {
            if addr != self.w.f.fun.arg() {
                self.w.copy(addr, self.w.f.fun.arg());
            }
            self.w.f.tail_parse_call(
                call_info,
                self.w.f.fun.arg(),
                ldt_parser_fun,
                ret,
                self.w.f.fun.retlen(),
            );
        } else {
            self.w
                .f
                .parse_call(call_info, addr, ldt_parser_fun, ret, retlen, self.w.retreat);
        }
        Ok(())
    }

    fn call_info_at_id(&self, id: DefId) -> CallMeta {
        CallMeta {
            req: self.req_transformer[&id] * self.req,
            tail: self.tails.contains(&id),
        }
    }

    fn parse_statement(&mut self, parse: &hir::ParseStatement) -> SResult<()> {
        if !self.processed_parse_sites.insert(parse.id.0) {
            return Ok(());
        }
        let info = self.call_info_at_id(parse.id.0);
        if !info.req.is_empty() {
            self.call_expr(parse.id.0, parse.expr, info)?
        }
        Ok(())
    }

    pub fn parserdef(&mut self, pd: &hir::ParserDef) -> SResult<()> {
        if !self.processed_parse_sites.insert(pd.id.0) {
            return Ok(());
        }
        let info = self.call_info_at_id(pd.id.0);
        if info.req.is_empty() {
            return Ok(());
        }
        let call_loc = pd.id.0;
        let expr = pd.to;
        let parser_fun = self.w.val_place_at_def(expr.0).unwrap();
        let parser_ty = self.w.f.fun.place(parser_fun).ty;
        let ldt_parser = self.db.least_deref_type(parser_ty)?;
        let ldt_parser_fun = self.w.copy_if_different_levels(
            ldt_parser,
            parser_ty,
            None,
            PlaceOrigin::Node(expr.0),
            |_, _| Ok(parser_fun),
        )?;
        let addr = self.w.front_place_at_def(call_loc).unwrap();
        let ret = info
            .req
            .contains(NeededBy::Val)
            .then(|| self.w.val_place_at_def(call_loc).unwrap());
        let retlen = info
            .req
            .contains(NeededBy::Len)
            .then(|| self.w.back_place_at_def(call_loc).unwrap());
        self.w
            .f
            .tail_parse_call(info, addr, ldt_parser_fun, ret, retlen);
        Ok(())
    }

    pub fn if_parser(&mut self, constr: HirConstraintId) -> SResult<()> {
        let constr = self.db.lookup_intern_hir_constraint(constr);
        let ldt_ret = if let Some(ret) = self.w.f.fun.ret() {
            let ret_ty = self.w.f.fun.place(ret).ty;
            let ldt_ret = self.db.least_deref_type(ret_ty)?;
            Some(ldt_ret)
        } else {
            None
        };
        let tmp_place = self.w.f.new_stack_place(ldt_ret.unwrap(), PlaceOrigin::Ret);
        let arg_ty = self.w.f.fun.place(self.w.f.fun.arg()).ty;
        let arg_tmp_place = self.w.f.new_stack_place(arg_ty, PlaceOrigin::Arg);
        let retlen = self.req.contains(NeededBy::Len).then(|| self.w.f.fun.arg());
        let fun_place = self.w.f.fun.cap();
        let fun_ty = self.w.f.fun.place(fun_place).ty;
        let inner_fun_place = self.w.f.add_place(PlaceInfo {
            place: Place::Front(fun_place),
            ty: fun_ty,
            remove_bt: false,
        });

        // the following parse call might mutate the argument, therefore we make a copy
        self.w.copy(self.w.f.fun.arg(), arg_tmp_place);

        let mut first_call_req = self.req | NeededBy::Val;
        let mut retreat = self.w.retreat;
        if constr.has_no_eof {
            first_call_req |= NeededBy::Len;
            retreat.eof = retreat.backtrack;
        }

        self.w.f.parse_call(
            CallMeta {
                req: first_call_req,
                tail: false,
            },
            self.w.f.fun.arg(),
            inner_fun_place,
            Some(tmp_place),
            retlen,
            retreat,
        );
        let convert_succ = self.w.f.new_bb();
        let root = constr.expr.root();
        self.w
            .convert_constraint(&constr.expr, root, tmp_place, convert_succ)?;
        self.w.f.set_bb(convert_succ);
        let ret_if_needed = self
            .req
            .contains(NeededBy::Val)
            .then(|| self.w.f.fun.ret().unwrap());
        if (self.req & NeededBy::Val).is_empty() {
            return Ok(());
        }
        self.w.f.parse_call(
            CallMeta {
                req: self.req & NeededBy::Val,
                tail: false,
            },
            arg_tmp_place,
            inner_fun_place,
            ret_if_needed,
            None,
            self.w.retreat,
        );
        Ok(())
    }

    fn copy_predecessor(&mut self, pred: ParserPredecessor, id: DefId) {
        let from_place = match pred {
            ParserPredecessor::ChildOf(c) => {
                let parent_choice = self.context_data[&c].parent_choice;
                match parent_choice {
                    Some(x) => self.w.front_place_at_def(x.0).unwrap(),
                    None => self.w.f.fun.arg(),
                }
            }
            ParserPredecessor::After(p) => self.w.back_place_at_def(p).unwrap(),
        };
        let to_place = self.w.front_place_at_def(id).unwrap();
        self.w.copy(from_place, to_place);
    }

    fn parse_statement_front(&mut self, parse: &hir::ParseStatement) {
        self.copy_predecessor(parse.front, parse.id.0)
    }

    pub fn add_sub_value(&mut self, sub_value: SubValue) -> SResult<()> {
        match self.db.hir_node(sub_value.id)? {
            hir::HirNode::Let(l) => {
                if sub_value.kind != SubValueKind::Val {
                    return Ok(());
                }
                self.change_context(l.context);
                self.let_statement(&l)
            }
            hir::HirNode::Expr(e) => {
                if sub_value.kind != SubValueKind::Val {
                    return Ok(());
                }
                if let Some(ctx) = e.parent_context {
                    self.change_context(ctx)
                }
                let resolved_expr =
                    Resolved::expr_with_data::<(ExprIdx<Resolved>, FullTypeId)>(self.db, e.id)?;
                let place = self.w.val_place_at_def(e.id.0).unwrap();
                self.w
                    .convert_expr(e.id, &resolved_expr, Some(place), |_| true)?;
            }
            hir::HirNode::Parse(p) => {
                self.change_context(p.parent_context);
                match sub_value.kind {
                    SubValueKind::Front => self.parse_statement_front(&p),
                    SubValueKind::Back | SubValueKind::Val | SubValueKind::Bt => {
                        self.parse_statement(&p)?
                    }
                }
            }
            hir::HirNode::Choice(c) => {
                self.change_context(c.parent_context);
                match sub_value.kind {
                    SubValueKind::Val => self.end_choice(&c.subcontexts, c.id),
                    SubValueKind::Front => {
                        if let Some([front, _]) = c.endpoints {
                            self.copy_predecessor(front, c.id.0)
                        } else {
                            return Ok(());
                        }
                    }
                    // pushed by individual contexts
                    SubValueKind::Back => return Ok(()),
                    SubValueKind::Bt => return Ok(()),
                }
            }
            // choice indirections are pushed by individual contexts and not pulled
            hir::HirNode::ChoiceIndirection(_) => return Ok(()),
            // arg and return place already have this function
            hir::HirNode::Block(b) if sub_value.kind == SubValueKind::Back => {
                // if the last call is a tail call, we don't need this
                let bb = self.w.f.current_bb;
                if let Some(MirInstr::ParseCall(.., None)) = self.w.f.fun.bb(bb).ins.last() {
                    return Ok(());
                }
                let last_place_back = self.context_data[&b.root_context]
                    .ends
                    .map(|x| self.w.back_place_at_def(x.1).unwrap())
                    .unwrap_or_else(|| self.w.f.fun.arg());
                let current_place = self.w.back_place_at_def(b.id.0).unwrap();
                self.w.copy(last_place_back, current_place);
            }
            hir::HirNode::Block(_) => return Ok(()),
            hir::HirNode::Module(_)
            | hir::HirNode::Context(_)
            | hir::HirNode::TExpr(_)
            | hir::HirNode::ArgDef(_)
            | hir::HirNode::Import(_)
            | hir::HirNode::ParserDef(_)
            | hir::HirNode::Array(_) => panic!("invalid subvalue encountered"),
        };
        Ok(())
    }

    fn block_places(
        db: &dyn Mirs,
        block: &hir::Block,
        requirements: RequirementSet,
        order: &BlockSerialization,
        f: &mut FunctionWriter,
    ) -> SResult<FxHashMap<SubValue, PlaceRef>> {
        let ambient_type = f.fun.place(f.fun.arg()).ty;
        let mut places: FxHashMap<SubValue, PlaceRef> = Default::default();
        let root_context = block.root_context.lookup(db)?;
        let returned_vals = if requirements.contains(NeededBy::Val) {
            if block.returns {
                [block.root_context.0.child_field(db, FieldName::Return)]
                    .into_iter()
                    .collect()
            } else {
                root_context.vars.set.values().map(|x| *x.inner()).collect()
            }
        } else {
            FxHashSet::default()
        };
        for value in order.eval_order.iter() {
            if (value.requirements & requirements).is_empty() {
                continue;
            }
            let val = value.val;
            let hirval = db.hir_node(val.id)?;
            if val.id == block.id.0
                && val.kind == SubValueKind::Back
                && requirements.contains(NeededBy::Len)
            {
                places.insert(val, f.fun.retlen().unwrap());
            } else if matches!(val.kind, SubValueKind::Back | SubValueKind::Front) {
                if val.kind == SubValueKind::Back && hirval.is_kind(HirNodeKind::Parse.into()) {
                    continue;
                }
                let place = Place::Stack(f.new_stack_ref(PlaceOrigin::Ambient(block.id, val.id)));
                let place_ref = f.add_place(PlaceInfo {
                    place,
                    ty: ambient_type,
                    remove_bt: false,
                });
                places.insert(val, place_ref);
            } else if matches!(val.kind, SubValueKind::Val) {
                let place = if returned_vals.contains(&val.id) {
                    if block.returns {
                        f.fun.place(f.fun.ret().unwrap()).place
                    } else {
                        Place::Field(f.fun.ret().unwrap(), val.id)
                    }
                } else {
                    Place::Stack(f.new_stack_ref(PlaceOrigin::Node(val.id)))
                };
                let ty: TypeId = match &hirval {
                    HirNode::Let(_)
                    | HirNode::Parse(_)
                    | HirNode::ChoiceIndirection(_)
                    | HirNode::ArgDef(_) => db.parser_type_at(val.id)?,
                    HirNode::Expr(e) => *db.parser_expr_at(e.id)?.root_data(),
                    HirNode::Choice(_) => continue,
                    HirNode::Block(_) => continue,
                    HirNode::TExpr(_)
                    | HirNode::Array(_)
                    | HirNode::Module(_)
                    | HirNode::Context(_)
                    | HirNode::Import(_)
                    | HirNode::ParserDef(_) => {
                        dbpanic!(db, "subvalue {} should not occur here", &val)
                    }
                };
                let place_ref = f.add_place(PlaceInfo {
                    place,
                    ty,
                    remove_bt: false,
                });
                places.insert(val, place_ref);
            }
        }
        Ok(places)
    }

    pub fn new_block_builder(
        db: &'a dyn Mirs,
        id: BlockId,
        requirements: RequirementSet,
        order: &BlockSerialization,
    ) -> SResult<Self> {
        let block = id.lookup(db)?;
        let mut f = FunctionWriter::new_block(db, &block, requirements)?;
        let mut top_level_retreat = f.make_top_level_retreat();
        if !requirements.contains(NeededBy::Backtrack) {
            top_level_retreat.backtrack = top_level_retreat.error;
        }
        let retreat = top_level_retreat;
        let places = Self::block_places(db, &block, requirements, order, &mut f)?;
        let context_data: FxHashMap<ContextId, ContextData> =
            ContextData::build_context_tree(db, block.root_context)?;
        let mut context_bb: FxHashMap<ContextId, (BBRef, BBRef)> = FxHashMap::default();
        context_bb.insert(block.root_context, (f.fun.entry(), f.fun.entry()));
        let current_context: Option<ContextId> = Some(block.root_context);
        let returns_self: bool = requirements.contains(NeededBy::Val) && !block.returns;
        let processed_parse_sites = FxHashSet::default();
        let req_transformer = order.parse_requirements.clone();
        let tails = order.tails.clone();
        f.set_bb(f.fun.entry());
        let w = ConvertExpr::new(db, f, retreat, places);
        Ok(ConvertCtx {
            db,
            top_level_retreat,
            context_bb,
            current_context,
            context_data,
            returns_self,
            processed_parse_sites,
            req: requirements,
            req_transformer,
            tails,
            w,
        })
    }

    fn pd_places(
        db: &dyn Mirs,
        id: ParserDefId,
        f: &mut FunctionWriter,
    ) -> SResult<FxHashMap<SubValue, PlaceRef>> {
        let mut places: FxHashMap<SubValue, PlaceRef> = FxHashMap::default();
        let pd = id.lookup(db)?;
        let expr = db.parser_expr_at(pd.to)?;
        let expr_place = Place::Stack(f.new_stack_ref(PlaceOrigin::Expr(pd.to, expr.root())));
        let expr_place_ref = f.add_place(PlaceInfo {
            place: expr_place,
            ty: *expr.root_data(),
            remove_bt: false,
        });
        places.insert(SubValue::new_val(pd.to.0), expr_place_ref);
        places.insert(SubValue::new_front(id.0), f.fun.arg());
        if let Some(ret) = f.fun.ret() {
            places.insert(SubValue::new_val(id.0), ret);
        }
        if let Some(retlen) = f.fun.retlen() {
            places.insert(SubValue::new_back(id.0), retlen);
        }
        Ok(places)
    }

    pub fn new_parserdef_builder(
        db: &'a dyn Mirs,
        id: ParserDefId,
        requirements: RequirementSet,
    ) -> SResult<Self> {
        let mut f = FunctionWriter::new_pd(db, id, requirements)?;
        let mut top_level_retreat = f.make_top_level_retreat();
        if !requirements.contains(NeededBy::Backtrack) {
            top_level_retreat.backtrack = top_level_retreat.error;
        }
        let retreat = top_level_retreat;
        let context_data = FxHashMap::default();
        let context_bb = FxHashMap::default();
        let places = Self::pd_places(db, id, &mut f)?;
        let current_context = None;
        let returns_self = false;
        let processed_parse_sites = FxHashSet::default();
        let mut req_transformer = BTreeMap::default();
        req_transformer.insert(id.0, RequirementMatrix::id());
        let req_transformer = Arc::new(req_transformer);
        let mut tails = BTreeSet::default();
        tails.insert(id.0);
        let tails = Arc::new(tails);
        f.set_bb(f.fun.entry());
        let w = ConvertExpr::new(db, f, retreat, places);
        Ok(ConvertCtx {
            db,
            top_level_retreat,
            context_bb,
            current_context,
            context_data,
            returns_self,
            processed_parse_sites,
            req: requirements,
            req_transformer,
            tails,
            w,
        })
    }

    pub fn new_if_builder(
        db: &'a dyn Mirs,
        ty: TypeId,
        requirements: RequirementSet,
        is_try: bool,
    ) -> SResult<Self> {
        let Type::ParserArg { result, arg } = db.lookup_intern_type(ty) else {
            panic!("Expected ParserArg, got {ty:?}")
        };
        let mut f = FunctionWriter::new(ty, arg, result, requirements | NeededBy::Val);
        let mut top_level_retreat = f.make_top_level_retreat();
        if !requirements.contains(NeededBy::Backtrack) || is_try {
            top_level_retreat.backtrack = top_level_retreat.error;
        }
        let retreat = top_level_retreat;
        f.set_bb(f.fun.entry());
        let w = ConvertExpr::new(db, f, retreat, Default::default());
        Ok(ConvertCtx {
            db,
            top_level_retreat,
            req: requirements,
            context_bb: Default::default(),
            current_context: Default::default(),
            context_data: Default::default(),
            returns_self: false,
            processed_parse_sites: Default::default(),
            req_transformer: Default::default(),
            tails: Default::default(),
            w,
        })
    }

    pub fn finish_fun(mut self) -> Function {
        let bb = self.w.f.fun.bb(self.w.f.current_bb);
        // don't add a return statement if we have a tail call
        if let Some(MirInstr::ParseCall(.., None)) = bb.ins.last() {
            return self.w.f.fun;
        }
        self.w.f.ret(ReturnStatus::Ok);
        self.w.f.fun
    }
}
