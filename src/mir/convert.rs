use std::collections::hash_map::Entry;

use fxhash::{FxHashMap, FxHashSet};

use crate::{
    dbpanic,
    error::SResult,
    expr::{
        self, ConstraintBinOp, ConstraintUnOp, Dyadic, ExprIter, ExpressionHead, Monadic, ValBinOp,
        ValUnOp, WiggleKind,
    },
    hir::{
        self, variable_set::VarStatus, BlockId, ChoiceId, ContextId, ExprId, HirIdWrapper, HirNode,
        ParserDefId, ParserPredecessor,
    },
    interner::{DefId, FieldName, PathComponent},
    order::{
        expr::ResolvedAtom, BlockSerialization, SubValue, SubValueInfo, SubValueKind,
        TypedResolvedExpr,
    },
    types::{PrimitiveType, Type, TypeId},
};

use super::{
    BBRef, CallKind, Comp, DupleField, ExceptionRetreat, Function, FunctionWriter, IntBinOp,
    IntUnOp, MirInstr, Mirs, Place, PlaceInfo, PlaceRef, ReturnStatus, Val,
};

pub struct ConvertCtx<'a> {
    db: &'a dyn Mirs,
    int: TypeId,
    f: FunctionWriter,
    retreat: ExceptionRetreat,
    top_level_retreat: ExceptionRetreat,
    places: FxHashMap<SubValue, PlaceRef>,
    context_bb: FxHashMap<ContextId, (BBRef, BBRef)>,
    current_context: Option<ContextId>,
    context_data: FxHashMap<ContextId, ContextData>,
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
                    match subchoice_affected_discriminants.entry(c.target_choice) {
                        Entry::Occupied(mut entry) => {
                            entry.get_mut().insert(field);
                        }
                        Entry::Vacant(entry) => {
                            let mut new_map = FxHashSet::default();
                            new_map.insert(field);
                            entry.insert(new_map);
                        }
                    };
                }
                _ => continue,
            };
        }
        for (choice, subcontx) in subchoices.iter() {
            let affected_discriminants = subchoice_affected_discriminants
                .remove(choice)
                .unwrap_or_default();
            for (i, &ctx) in subcontx.iter().enumerate() {
                let next_context = subcontx.get(i + 1).map(|x| *x).or(backtracks_to);
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
    fn val_place_at_def(&self, id: DefId) -> Option<PlaceRef> {
        self.places
            .get(&SubValue {
                kind: SubValueKind::Val,
                id,
            })
            .map(|x| *x)
    }
    fn front_place_at_def(&self, id: DefId) -> Option<PlaceRef> {
        self.places
            .get(&SubValue {
                kind: SubValueKind::Front,
                id,
            })
            .map(|x| *x)
    }
    fn back_place_at_def(&self, id: DefId) -> Option<PlaceRef> {
        self.places
            .get(&SubValue {
                kind: SubValueKind::Back,
                id,
            })
            .map(|x| *x)
    }
    pub fn change_context(&mut self, context: ContextId) {
        if Some(context) == self.current_context {
            return;
        }
        if let Some(ctx) = self.current_context {
            self.context_bb
                .get_mut(&ctx)
                .expect("uninitialized context")
                .1 = self.f.current_bb;
        }
        self.current_context = Some(context);
        self.f.set_bb(self.context_bb[&context].1);
        let context_backtrack = self.context_data[&context].backtracks_to;

        self.retreat.backtrack = context_backtrack
            .map(|x| self.context_bb[&x].0)
            .unwrap_or(self.top_level_retreat.backtrack);
    }

    fn make_place_ref(&mut self, place: Place, ty: TypeId) -> PlaceRef {
        let place_info = PlaceInfo { place, ty };
        self.f.add_place(place_info)
    }
    fn new_stack_place(&mut self, ty: TypeId) -> PlaceRef {
        let new_place = Place::Stack(self.f.new_stack_ref());
        self.make_place_ref(new_place, ty)
    }
    fn unwrap_or_stack(&mut self, place: Option<PlaceRef>, ty: TypeId) -> PlaceRef {
        place.unwrap_or_else(|| self.new_stack_place(ty))
    }
    fn copy(&mut self, origin: PlaceRef, target: PlaceRef) {
        self.f
            .append_ins(MirInstr::Copy(target, origin, self.retreat.error));
    }
    fn copy_if_different_types(
        &mut self,
        target_ty: TypeId,
        inner_ty: TypeId,
        place: Option<PlaceRef>,
        cont: impl FnOnce(&mut Self, Option<PlaceRef>) -> SResult<PlaceRef>,
    ) -> SResult<PlaceRef> {
        if target_ty == inner_ty {
            return cont(self, place);
        }
        let inner = cont(self, None)?;
        let target = self.unwrap_or_stack(place, target_ty);
        self.copy(inner, target);
        Ok(target)
    }
    fn load_var(&mut self, var: DefId, ty: TypeId, place: Option<PlaceRef>) -> Option<PlaceRef> {
        let place_ref = self.val_place_at_def(var)?;
        if self.f.fun.place(place_ref).ty == ty && place.is_none() {
            return Some(place_ref);
        }
        let new_place = self.new_stack_place(ty);
        self.copy(place_ref, new_place);
        Some(new_place)
    }
    fn load_captured(
        &mut self,
        captured: DefId,
        ty: TypeId,
        place: Option<PlaceRef>,
    ) -> SResult<PlaceRef> {
        let cap_ty = self.db.parser_type_at(captured)?;
        let place_ref = self.f.add_place(PlaceInfo {
            place: Place::Field(self.f.fun.cap(), captured),
            ty: cap_ty,
        });
        if ty == cap_ty && place.is_none() {
            return Ok(place_ref);
        }
        let new_place = self.new_stack_place(ty);
        self.copy(place_ref, new_place);
        Ok(new_place)
    }
    fn load_int(&mut self, n: i64, ty: TypeId, place: Option<PlaceRef>) -> PlaceRef {
        let place_ref = self.unwrap_or_stack(place, ty);
        self.f.append_ins(MirInstr::LoadVal(place_ref, Val::Int(n)));
        place_ref
    }
    fn load_char(&mut self, c: u32, ty: TypeId, place: Option<PlaceRef>) -> PlaceRef {
        let place_ref = self.unwrap_or_stack(place, ty);
        self.f
            .append_ins(MirInstr::LoadVal(place_ref, Val::Char(c)));
        place_ref
    }
    fn create_block_parser(
        &mut self,
        block: BlockId,
        ty: TypeId,
        place: Option<PlaceRef>,
    ) -> SResult<PlaceRef> {
        let captures = self.db.captures(block);
        let place_ref = self.unwrap_or_stack(place, ty);
        for captured in captures.iter() {
            let cap_ty = self.db.parser_type_at(*captured)?;
            let bp_ref = |ctx: &mut Self, block| {
                ctx.f.add_place(PlaceInfo {
                    place: Place::Field(block, *captured),
                    ty: cap_ty,
                })
            };

            let target = bp_ref(self, place_ref);

            let origin = match self.val_place_at_def(*captured) {
                Some(p) => p,
                None => bp_ref(self, self.f.fun.cap()),
            };

            self.copy(origin, target);
        }
        Ok(place_ref)
    }
    fn convert_expr(
        &mut self,
        expr: &TypedResolvedExpr,
        place: Option<PlaceRef>,
    ) -> SResult<PlaceRef> {
        let ty = *expr.0.root_data();
        Ok(match &expr.0 {
            ExpressionHead::Niladic(n) => match &n.inner {
                ResolvedAtom::Val(val) => self
                    .load_var(*val, ty, place)
                    .expect("Invalid refernce to variable"),
                ResolvedAtom::Captured(cap) => self.load_captured(*cap, ty, place)?,
                ResolvedAtom::Number(n) => self.load_int(*n, ty, place),
                ResolvedAtom::Char(c) => self.load_char(*c, ty, place),
                ResolvedAtom::ParserDef(_) | ResolvedAtom::Single => {
                    self.unwrap_or_stack(place, ty)
                }
                ResolvedAtom::Block(block) => self.create_block_parser(*block, ty, place)?,
            },
            ExpressionHead::Monadic(Monadic { op, inner }) => {
                let inner_ty = *inner.0.root_data();
                let recurse = |ctx: &mut Self, plc| ctx.convert_expr(&inner, plc);
                match &op.inner {
                    ValUnOp::Not | ValUnOp::Neg => {
                        let op: IntUnOp = (&op.inner).try_into().unwrap();
                        let inner =
                            self.copy_if_different_types(self.int, inner_ty, None, recurse)?;
                        let place_ref = self.unwrap_or_stack(place, ty);
                        self.f.append_ins(MirInstr::IntUn(place_ref, op, inner));
                        place_ref
                    }
                    ValUnOp::Pos => {
                        self.copy_if_different_types(self.int, inner_ty, None, recurse)?
                    }
                    ValUnOp::Wiggle(constr, kind) => {
                        let place_ref =
                            self.copy_if_different_types(self.int, inner_ty, None, recurse)?;
                        let place_ldt = self.db.least_deref_type(ty)?;
                        let ldt_ref = self.new_stack_place(place_ldt);
                        self.copy(place_ref, ldt_ref);
                        let old_backtrack = self.retreat.backtrack;
                        self.retreat.backtrack = match kind {
                            WiggleKind::If => self.retreat.backtrack,
                            WiggleKind::Try => self.retreat.error,
                            WiggleKind::Wiggly => self.retreat.error,
                        };
                        let cont = self.f.new_bb();
                        self.convert_constraint(&constr, ldt_ref, cont)?;
                        self.f.set_bb(cont);
                        self.retreat.backtrack = old_backtrack;
                        place_ref
                    }
                    ValUnOp::Dot(field) => {
                        let inner_ldt = self.db.least_deref_type(inner_ty)?;
                        let block_ref =
                            self.copy_if_different_types(inner_ldt, inner_ty, None, recurse)?;
                        let place_ref = self.unwrap_or_stack(place, ty);
                        self.f.append_ins(MirInstr::Field(
                            place_ref,
                            block_ref,
                            *field,
                            self.retreat.error,
                        ));
                        place_ref
                    }
                }
            }
            ExpressionHead::Dyadic(Dyadic {
                op,
                inner: [left, right],
            }) => {
                let left_ty = *left.0.root_data();
                let right_ty = *right.0.root_data();
                let lrecurse = |ctx: &mut Self, plc| ctx.convert_expr(left, plc);
                let rrecurse = |ctx: &mut Self, plc| ctx.convert_expr(right, plc);
                match &op.inner {
                    ValBinOp::And
                    | ValBinOp::Xor
                    | ValBinOp::Or
                    | ValBinOp::ShiftR
                    | ValBinOp::ShiftL
                    | ValBinOp::Minus
                    | ValBinOp::Plus
                    | ValBinOp::Div
                    | ValBinOp::Modulo
                    | ValBinOp::Mul => {
                        let op: IntBinOp = (&op.inner).try_into().unwrap();
                        let left =
                            self.copy_if_different_types(self.int, left_ty, None, lrecurse)?;
                        let right =
                            self.copy_if_different_types(self.int, right_ty, None, rrecurse)?;
                        let place_ref = self.unwrap_or_stack(place, ty);
                        self.f
                            .append_ins(MirInstr::IntBin(place_ref, op, left, right));
                        place_ref
                    }
                    ValBinOp::LesserEq
                    | ValBinOp::Lesser
                    | ValBinOp::GreaterEq
                    | ValBinOp::Greater
                    | ValBinOp::Uneq
                    | ValBinOp::Equals => {
                        let op: Comp = (&op.inner).try_into().unwrap();
                        let left =
                            self.copy_if_different_types(self.int, left_ty, None, lrecurse)?;
                        let right =
                            self.copy_if_different_types(self.int, right_ty, None, rrecurse)?;
                        let place_ref = self.unwrap_or_stack(place, ty);
                        self.f
                            .append_ins(MirInstr::Comp(place_ref, op, left, right));
                        place_ref
                    }
                    ValBinOp::ParserApply => {
                        let left_ldt = self.db.least_deref_type(left_ty)?;
                        let left =
                            self.copy_if_different_types(left_ldt, left_ty, None, lrecurse)?;
                        let right = rrecurse(self, None)?;
                        let place_ref = self.unwrap_or_stack(place, ty);
                        self.f.append_ins(MirInstr::Call(
                            place_ref,
                            CallKind::Val,
                            left,
                            right,
                            self.retreat,
                        ));
                        place_ref
                    }
                    ValBinOp::Else => {
                        let place_ref = self.unwrap_or_stack(place, ty);
                        let right_bb = self.f.new_bb();
                        let continue_bb = self.f.new_bb();
                        let old_backtrack = self.retreat.backtrack;
                        self.retreat.backtrack = right_bb;
                        self.copy_if_different_types(ty, left_ty, Some(place_ref), lrecurse)?;
                        self.f.set_jump(continue_bb);
                        self.f.set_bb(right_bb);
                        self.retreat.backtrack = old_backtrack;
                        self.copy_if_different_types(ty, right_ty, Some(place_ref), rrecurse)?;
                        self.f.set_jump(continue_bb);
                        self.f.set_bb(continue_bb);
                        place_ref
                    }
                    ValBinOp::Compose => {
                        let place_ref = self.unwrap_or_stack(place, ty);
                        let left_ldt = self.db.least_deref_type(left_ty)?;
                        let right_ldt = self.db.least_deref_type(right_ty)?;
                        let left_place = self.f.add_place(PlaceInfo {
                            place: Place::DupleField(place_ref, DupleField::First),
                            ty: left_ldt,
                        });
                        let right_place = self.f.add_place(PlaceInfo {
                            place: Place::DupleField(place_ref, DupleField::Second),
                            ty: right_ldt,
                        });
                        self.copy_if_different_types(
                            left_ldt,
                            left_ty,
                            Some(left_place),
                            lrecurse,
                        )?;
                        self.copy_if_different_types(
                            right_ldt,
                            right_ty,
                            Some(right_place),
                            rrecurse,
                        )?;
                        place_ref
                    }
                }
            }
        })
    }
    fn convert_constraint(
        &mut self,
        expr: &hir::ConstraintExpression,
        val: PlaceRef,
        mut cont: BBRef,
    ) -> SResult<()> {
        match &expr.0 {
            ExpressionHead::Niladic(n) => {
                self.f.append_ins(MirInstr::AssertVal(
                    val,
                    n.inner.clone(),
                    self.retreat.backtrack,
                ));
                self.f.set_jump(cont);
                Ok(())
            }
            ExpressionHead::Monadic(Monadic { op, inner }) => match op.inner {
                ConstraintUnOp::Not => {
                    std::mem::swap(&mut cont, &mut self.retreat.backtrack);
                    self.convert_constraint(inner, val, cont)?;
                    self.retreat.backtrack = cont;
                    Ok(())
                }
                ConstraintUnOp::Dot(_) => todo!(),
            },
            ExpressionHead::Dyadic(Dyadic { op, inner }) => match op.inner {
                ConstraintBinOp::And => {
                    let right_bb = self.f.new_bb();
                    self.convert_constraint(&*inner[0], val, right_bb)?;
                    self.f.set_bb(right_bb);
                    self.convert_constraint(&*inner[1], val, cont)?;
                    Ok(())
                }
                ConstraintBinOp::Or => {
                    let right_bb = self.f.new_bb();
                    let old_backtrack = self.retreat.backtrack;
                    self.retreat.backtrack = right_bb;
                    self.convert_constraint(&*inner[0], val, cont)?;
                    self.f.set_bb(right_bb);
                    self.retreat.backtrack = old_backtrack;
                    self.convert_constraint(&*inner[1], val, cont)?;
                    Ok(())
                }
            },
        }
    }
    fn terminate_context(&mut self, context: ContextId, superchoice: ChoiceId, cont: BBRef) {
        self.change_context(context);
        let context_data = self
            .context_data
            .remove(&context)
            .expect("context not initialized or removed multiple times");
        for (from, to) in context_data.field_move_targets.iter() {
            let to_place = match self.val_place_at_def(*to) {
                Some(p) => p,
                None => continue,
            };
            let from_place = self.val_place_at_def(*from).unwrap();
            self.copy(from_place, to_place);
        }
        if self.returns_self {
            for field in context_data.affected_discriminants {
                let has_field = context_data.field_ids.get(&field).is_some();
                self.f.append_ins(MirInstr::SetDiscriminant(
                    self.f.fun.ret(),
                    field,
                    has_field,
                ));
            }
        }
        if let Some(to_place) = self.back_place_at_def(superchoice.0) {
            let from_place = if let Some((_, end)) = context_data.ends {
                self.back_place_at_def(end)
            } else {
                self.front_place_at_def(superchoice.0)
            }
            .expect("could not find last parse end of context");
            self.copy(from_place, to_place);
        }
        self.f.set_jump(cont);
    }
    fn end_choice(&mut self, subcontexts: &[ContextId], id: ChoiceId) {
        let parent_context = self
            .current_context
            .expect("end choice called without context");
        let first_context_bb = match subcontexts.get(0) {
            Some(x) => self.context_bb[x].0,
            None => return,
        };
        self.f.set_jump(first_context_bb);
        let cont = self.f.new_bb();
        self.f.set_bb(cont);
        for context in subcontexts.iter() {
            self.terminate_context(*context, id, cont)
        }
        self.change_context(parent_context);
    }
    fn let_statement(&mut self, statement: &hir::LetStatement) {
        let target = self
            .val_place_at_def(statement.id.0)
            .expect("let statement has no place");
        let expr_val = self
            .val_place_at_def(statement.expr.0)
            .expect("let statement's expression has no place");
        self.copy(expr_val, target);
    }
    fn call_expr(&mut self, call_loc: DefId, expr: ExprId, call_kind: CallKind) {
        let parser_fun = self.val_place_at_def(expr.0).unwrap();
        let addr = self.front_place_at_def(call_loc).unwrap();
        let target = match call_kind {
            CallKind::Len => self.back_place_at_def(call_loc),
            CallKind::Val => self.val_place_at_def(call_loc),
        }
        .unwrap();
        self.f.append_ins(MirInstr::Call(
            target,
            call_kind,
            parser_fun,
            addr,
            self.retreat,
        ));
    }
    fn parse_statement_val(&mut self, parse: &hir::ParseStatement) {
        self.call_expr(parse.id.0, parse.expr, CallKind::Val)
    }
    fn parse_statement_back(&mut self, parse: &hir::ParseStatement) {
        self.call_expr(parse.id.0, parse.expr, CallKind::Len)
    }
    fn parserdef_val(&mut self, pd: &hir::ParserDef) {
        self.call_expr(pd.id.0, pd.to, CallKind::Val)
    }
    fn parserdef_len(&mut self, pd: &hir::ParserDef) {
        self.call_expr(pd.id.0, pd.to, CallKind::Len)
    }
    fn copy_predecessor(&mut self, pred: ParserPredecessor, id: DefId) {
        let from_place = match pred {
            ParserPredecessor::ChildOf(c) => {
                let parent_choice = self.context_data[&c].parent_choice;
                match parent_choice {
                    Some(x) => self.front_place_at_def(x.0).unwrap(),
                    None => self.f.fun.arg(),
                }
            }
            ParserPredecessor::After(p) => self.back_place_at_def(p).unwrap(),
        };
        let to_place = self.front_place_at_def(id).unwrap();
        self.copy(from_place, to_place);
    }
    fn parse_statement_front(&mut self, parse: &hir::ParseStatement) {
        self.copy_predecessor(parse.front, parse.id.0)
    }
    pub fn add_sub_value(&mut self, sub_value: SubValue) -> SResult<()> {
        match self.db.hir_node(sub_value.id)? {
            hir::HirNode::Let(l) => {
                self.change_context(l.context);
                self.let_statement(&l)
            }
            hir::HirNode::Expr(e) => {
                if let Some(ctx) = e.parent_context {
                    self.change_context(ctx)
                }
                let resolved_expr = self.db.resolve_expr(e.id)?;
                let place = self.val_place_at_def(e.id.0).unwrap();
                self.convert_expr(&resolved_expr, Some(place))?;
            }
            hir::HirNode::Parse(p) => {
                self.change_context(p.parent_context);
                match sub_value.kind {
                    SubValueKind::Val => self.parse_statement_val(&p),
                    SubValueKind::Front => self.parse_statement_front(&p),
                    SubValueKind::Back => self.parse_statement_back(&p),
                }
            }
            hir::HirNode::Choice(c) => {
                self.change_context(c.parent_context);
                match sub_value.kind {
                    SubValueKind::Val => self.end_choice(&c.subcontexts, c.id),
                    SubValueKind::Front => self.copy_predecessor(c.front, c.id.0),
                    // pushed by individual contexts
                    SubValueKind::Back => return Ok(()),
                }
            }
            hir::HirNode::ParserDef(pd) => {
                match sub_value.kind {
                    SubValueKind::Val => self.parserdef_val(&pd),
                    // already at right place
                    SubValueKind::Front => return Ok(()),
                    SubValueKind::Back => self.parserdef_len(&pd),
                }
            }
            // choice indirections are pushed by individual contexts and not pulled
            hir::HirNode::ChoiceIndirection(_) => return Ok(()),
            // arg and return place already have this function
            hir::HirNode::Block(_) => return Ok(()),
            hir::HirNode::Module(_)
            | hir::HirNode::Context(_)
            | hir::HirNode::TExpr(_)
            | hir::HirNode::Array(_) => panic!("invalid subvalue encountered"),
        };
        Ok(())
    }

    fn block_places(
        db: &dyn Mirs,
        block: &hir::Block,
        call_kind: CallKind,
        order: &BlockSerialization,
        f: &mut FunctionWriter,
    ) -> SResult<FxHashMap<SubValue, PlaceRef>> {
        let ambient_type = f.fun.place(f.fun.arg()).ty;
        let mut places: FxHashMap<SubValue, PlaceRef> = Default::default();
        let returned_vals: FxHashSet<DefId> = match call_kind {
            CallKind::Val => block
                .root_context
                .lookup(db)?
                .vars
                .set
                .values()
                .map(|x| *x.inner())
                .collect(),
            CallKind::Len => FxHashSet::default(),
        };
        for value in order.eval_order.iter().filter(match call_kind {
            CallKind::Len => |x: &&SubValueInfo| x.rdepends_back,
            CallKind::Val => |x: &&SubValueInfo| x.rdepends_val,
        }) {
            let val = value.val;
            if val.id == block.id.0 && val.kind == SubValueKind::Back && call_kind == CallKind::Len
            {
                places.insert(val, f.fun.ret());
            } else if matches!(val.kind, SubValueKind::Back | SubValueKind::Front) {
                let place = Place::Stack(f.new_stack_ref());
                let place_ref = f.add_place(PlaceInfo {
                    place,
                    ty: ambient_type,
                });
                places.insert(val, place_ref);
            } else {
                let place = if returned_vals.contains(&val.id) {
                    Place::Field(f.fun.ret(), val.id)
                } else {
                    Place::Stack(f.new_stack_ref())
                };
                let ty: TypeId = match db.hir_node(val.id)? {
                    HirNode::Let(_) | HirNode::Parse(_) | HirNode::ChoiceIndirection(_) => {
                        db.parser_type_at(val.id)?
                    }
                    HirNode::Expr(e) => *db.resolve_expr(e.id)?.0.root_data(),
                    HirNode::Choice(_) => continue,
                    HirNode::Block(_) => continue,
                    HirNode::TExpr(_)
                    | HirNode::Array(_)
                    | HirNode::Module(_)
                    | HirNode::Context(_)
                    | HirNode::ParserDef(_) => {
                        dbpanic!(db, "subvalue {} should not occur here", &val)
                    }
                };
                let place_ref = f.add_place(PlaceInfo { place, ty });
                places.insert(val, place_ref);
            }
        }
        Ok(places)
    }

    pub fn new_block_builder(
        db: &'a dyn Mirs,
        id: BlockId,
        call_kind: CallKind,
        order: &BlockSerialization,
    ) -> SResult<Self> {
        let block = id.lookup(db)?;
        let block_expr = db.resolve_expr(block.enclosing_expr)?;
        let block_ty = ExprIter::new(&block_expr)
            .find_map(|x| match &x.0 {
                ExpressionHead::Niladic(expr::OpWithData {
                    inner: ResolvedAtom::Block(b),
                    data,
                }) if *b == id => Some(*data),
                _ => None,
            })
            .expect("could not find block within enclosing expression");
        let (arg_ty, ret_ty) = match db.lookup_intern_type(block_ty) {
            Type::ParserArg { result, arg } => (arg, result),
            _ => dbpanic!(db, "should have been a parser type, was {}", &block_ty),
        };
        let mut f: FunctionWriter = FunctionWriter::new(block_ty, arg_ty, ret_ty);
        let top_level_retreat = f.make_top_level_retreat();
        let retreat = top_level_retreat;
        let int: TypeId = db.intern_type(Type::Primitive(PrimitiveType::Int));
        let places = Self::block_places(db, &block, call_kind, order, &mut f)?;
        let context_data: FxHashMap<ContextId, ContextData> =
            ContextData::build_context_tree(db, block.root_context)?;
        let mut context_bb: FxHashMap<ContextId, (BBRef, BBRef)> = FxHashMap::default();
        for context in context_data.keys() {
            let new_bb = if *context == block.root_context {
                f.fun.entry()
            } else {
                f.new_bb()
            };
            context_bb.insert(*context, (new_bb, new_bb));
        }
        let current_context: Option<ContextId> = None;
        let returns_self: bool = call_kind == CallKind::Val;
        f.set_bb(f.fun.entry());
        Ok(ConvertCtx {
            db,
            int,
            f,
            retreat,
            top_level_retreat,
            places,
            context_bb,
            current_context,
            context_data,
            returns_self,
        })
    }

    fn pd_places(
        db: &dyn Mirs,
        id: ParserDefId,
        call_kind: CallKind,
        from: TypeId,
        f: &mut FunctionWriter,
    ) -> SResult<FxHashMap<SubValue, PlaceRef>> {
        let mut places: FxHashMap<SubValue, PlaceRef> = FxHashMap::default();
        let pd = id.lookup(db)?;
        let expr_ty = *db.resolve_expr(pd.to)?.0.root_data();
        let expr_place = Place::Stack(f.new_stack_ref());
        let expr_place_ref = f.add_place(PlaceInfo {
            place: expr_place,
            ty: expr_ty,
        });
        places.insert(SubValue::new_val(pd.to.0), expr_place_ref);
        let arg_place_ref = match call_kind {
            CallKind::Len => f.fun.arg(),
            CallKind::Val => {
                let place = Place::From(f.fun.arg());
                f.add_place(PlaceInfo { place, ty: from })
            }
        };
        places.insert(SubValue::new_front(id.0), arg_place_ref);
        let ret_kind = match call_kind {
            CallKind::Len => SubValueKind::Back,
            CallKind::Val => SubValueKind::Val,
        };
        places.insert(
            SubValue {
                kind: ret_kind,
                id: id.0,
            },
            f.fun.ret(),
        );
        Ok(places)
    }

    pub fn new_parserdef_builder(
        db: &'a dyn Mirs,
        id: ParserDefId,
        call_kind: CallKind,
    ) -> SResult<Self> {
        let sig = db.parser_args(id)?;
        let from = sig.from.unwrap_or(db.intern_type(Type::Any));
        let ret_ty = match call_kind {
            CallKind::Len => from,
            CallKind::Val => db.parser_returns(id)?.deref,
        };
        let fun_ty = match call_kind {
            CallKind::Len => db.intern_type(Type::ParserArg {
                result: sig.thunk,
                arg: from,
            }),
            CallKind::Val => db.intern_type(Type::Any),
        };
        let arg_ty = match call_kind {
            CallKind::Len => from,
            CallKind::Val => sig.thunk,
        };
        let mut f = FunctionWriter::new(fun_ty, arg_ty, ret_ty);
        let top_level_retreat = f.make_top_level_retreat();
        let retreat = top_level_retreat;
        let int: TypeId = db.intern_type(Type::Primitive(PrimitiveType::Int));
        let context_data = FxHashMap::default();
        let context_bb = FxHashMap::default();
        let places = Self::pd_places(db, id, call_kind, from, &mut f)?;
        let current_context = None;
        let returns_self = false;
        f.set_bb(f.fun.entry());
        Ok(ConvertCtx {
            db,
            int,
            f,
            retreat,
            top_level_retreat,
            places,
            context_bb,
            current_context,
            context_data,
            returns_self,
        })
    }

    pub fn finish_fun(mut self) -> Function {
        self.f.set_return(ReturnStatus::Ok);
        self.f.fun
    }
}
