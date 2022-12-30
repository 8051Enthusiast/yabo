use std::{
    collections::{hash_map::Entry, BTreeMap},
    sync::Arc,
};

use enumflags2::make_bitflags;
use fxhash::{FxHashMap, FxHashSet};

use hir::{HirConstraintId, HirNodeKind};
use yaboc_ast::expr::{
    self, ConstraintBinOp, ConstraintUnOp, Dyadic, ExprIter, Expression, ExpressionHead, Ignorable,
    KindWithData, Monadic, ValBinOp, ValUnOp, Variadic, WiggleKind,
};
use yaboc_base::{
    dbpanic,
    error::SResult,
    interner::{DefId, FieldName, PathComponent},
    source::SpanIndex,
};
use yaboc_dependents::{
    BlockSerialization, NeededBy, RequirementMatrix, RequirementSet, SubValue, SubValueKind,
};
use yaboc_hir::{
    self as hir, variable_set::VarStatus, BlockId, ChoiceId, ContextId, ExprId, HirIdWrapper,
    HirNode, ParserDefId, ParserPredecessor,
};
use yaboc_resolve::expr::{ResolvedAtom, ResolvedKind};
use yaboc_types::{PrimitiveType, Type, TypeId};

use crate::InsRef;

use super::{
    BBRef, Comp, ExceptionRetreat, Function, FunctionWriter, IntBinOp, IntUnOp, MirInstr, Mirs,
    Place, PlaceInfo, PlaceOrigin, PlaceRef, ReturnStatus, Val,
};

pub type TypedIndexedExpr = Expression<KindWithData<ResolvedKind, (TypeId, SpanIndex, usize)>>;
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
    processed_parse_sites: FxHashSet<DefId>,
    req: RequirementSet,
    req_transformer: Arc<BTreeMap<DefId, RequirementMatrix>>,
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
    fn val_place_at_def(&self, id: DefId) -> Option<PlaceRef> {
        self.places
            .get(&SubValue {
                kind: SubValueKind::Val,
                id,
            })
            .copied()
    }

    fn front_place_at_def(&self, id: DefId) -> Option<PlaceRef> {
        self.places
            .get(&SubValue {
                kind: SubValueKind::Front,
                id,
            })
            .copied()
    }

    fn back_place_at_def(&self, id: DefId) -> Option<PlaceRef> {
        self.places
            .get(&SubValue {
                kind: SubValueKind::Back,
                id,
            })
            .copied()
    }

    // gets the bbs for the context and initializes if necessary
    fn context_bb(&mut self, context: ContextId) -> (BBRef, BBRef) {
        match self.context_bb.entry(context) {
            Entry::Occupied(o) => *o.get(),
            Entry::Vacant(v) => {
                let bb = self.f.new_bb();
                v.insert((bb, bb));
                (bb, bb)
            }
        }
    }

    fn next_ins(&self) -> InsRef {
        let current_bb = self.f.current_bb;
        let offset = self.f.fun.bb(current_bb).ins.len();
        InsRef(current_bb, offset as u32)
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
        let current_end = self.context_bb(context).1;
        self.f.set_bb(current_end);
        // if you end up with a crash here, that means a subvalue was used after
        // the context was terminated, which indicates a problem in dependents
        let context_backtrack = self.context_data[&context].backtracks_to;

        self.retreat.backtrack = if let Some(bt) = context_backtrack {
            self.context_bb(bt).0
        } else {
            self.top_level_retreat.backtrack
        }
    }

    fn make_place_ref(&mut self, place: Place, ty: TypeId) -> PlaceRef {
        let place_info = PlaceInfo {
            place,
            ty,
            remove_bt: false,
        };
        self.f.add_place(place_info)
    }

    fn new_stack_place(&mut self, ty: TypeId, origin: PlaceOrigin) -> PlaceRef {
        let new_place = Place::Stack(self.f.new_stack_ref(origin));
        self.make_place_ref(new_place, ty)
    }

    fn new_remove_bt_stack_place(&mut self, ty: TypeId, origin: PlaceOrigin) -> PlaceRef {
        let new_place = Place::Stack(self.f.new_stack_ref(origin));
        let place_info = PlaceInfo {
            place: new_place,
            ty,
            remove_bt: true,
        };
        self.f.add_place(place_info)
    }

    fn unwrap_or_stack(
        &mut self,
        place: Option<PlaceRef>,
        ty: TypeId,
        origin: PlaceOrigin,
    ) -> PlaceRef {
        place.unwrap_or_else(|| self.new_stack_place(ty, origin))
    }

    fn copy(&mut self, origin: PlaceRef, target: PlaceRef) {
        self.f.copy(origin, target, self.retreat.error);
    }

    fn copy_if_different_levels(
        &mut self,
        target_ty: TypeId,
        inner_ty: TypeId,
        place: Option<PlaceRef>,
        origin: PlaceOrigin,
        cont: impl FnOnce(&mut Self, Option<PlaceRef>) -> SResult<PlaceRef>,
    ) -> SResult<PlaceRef> {
        if self.db.deref_level(target_ty)? == self.db.deref_level(inner_ty)? {
            return cont(self, place);
        }
        let inner = cont(self, None)?;
        let target = self.unwrap_or_stack(place, target_ty, origin);
        self.copy(inner, target);
        Ok(target)
    }

    fn load_var(
        &mut self,
        var: DefId,
        ty: TypeId,
        place: Option<PlaceRef>,
        origin: PlaceOrigin,
        remove_bt: bool,
    ) -> Option<PlaceRef> {
        let place_ref = self.val_place_at_def(var)?;
        if self.f.fun.place(place_ref).ty == ty && place.is_none() && !remove_bt {
            return Some(place_ref);
        }
        if let Some(place) = place {
            if remove_bt {
                let between_place = self.new_remove_bt_stack_place(ty, origin);
                self.copy(place_ref, between_place);
                self.copy(between_place, place);
                return Some(place);
            }
        }
        let new_place = self.unwrap_or_stack(place, ty, origin);
        self.copy(place_ref, new_place);
        Some(new_place)
    }

    fn load_captured(
        &mut self,
        captured: DefId,
        ty: TypeId,
        place: Option<PlaceRef>,
        origin: PlaceOrigin,
        remove_bt: bool,
    ) -> SResult<PlaceRef> {
        let cap_ty = self.db.parser_type_at(captured)?;
        let place_ref = self.f.add_place(PlaceInfo {
            place: Place::Captured(self.f.fun.cap(), captured),
            ty: cap_ty,
            remove_bt: false,
        });
        if self.db.deref_level(ty) == self.db.deref_level(cap_ty) && place.is_none() && !remove_bt {
            return Ok(place_ref);
        }
        if let Some(place) = place {
            if remove_bt {
                let between_place = self.new_remove_bt_stack_place(ty, origin);
                self.copy(place_ref, between_place);
                self.copy(between_place, place);
                return Ok(place);
            }
        }
        let new_place = self.unwrap_or_stack(place, ty, origin);
        self.copy(place_ref, new_place);
        Ok(new_place)
    }

    fn load_int(
        &mut self,
        n: i64,
        ty: TypeId,
        place: Option<PlaceRef>,
        origin: PlaceOrigin,
    ) -> PlaceRef {
        let place_ref = self.unwrap_or_stack(place, ty, origin);
        self.f
            .append_ins(MirInstr::StoreVal(place_ref, Val::Int(n)));
        place_ref
    }

    fn load_char(
        &mut self,
        c: u32,
        ty: TypeId,
        place: Option<PlaceRef>,
        origin: PlaceOrigin,
    ) -> PlaceRef {
        let place_ref = self.unwrap_or_stack(place, ty, origin);
        self.f
            .append_ins(MirInstr::StoreVal(place_ref, Val::Char(c)));
        place_ref
    }

    fn load_bool(
        &mut self,
        b: bool,
        ty: TypeId,
        place: Option<PlaceRef>,
        origin: PlaceOrigin,
    ) -> PlaceRef {
        let place_ref = self.unwrap_or_stack(place, ty, origin);
        self.f
            .append_ins(MirInstr::StoreVal(place_ref, Val::Bool(b)));
        place_ref
    }

    fn create_block_parser(
        &mut self,
        block: BlockId,
        ty: TypeId,
        place: Option<PlaceRef>,
        origin: PlaceOrigin,
    ) -> SResult<PlaceRef> {
        let captures = self.db.captures(block);
        let place_ref = self.unwrap_or_stack(place, ty, origin);
        for captured in captures.iter() {
            let cap_ty = self.db.parser_type_at(*captured)?;
            let bp_ref = |ctx: &mut Self, block| {
                ctx.f.add_place(PlaceInfo {
                    place: Place::Captured(block, *captured),
                    ty: cap_ty,
                    remove_bt: false,
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
        expr_id: ExprId,
        expr: &TypedIndexedExpr,
        place: Option<PlaceRef>,
    ) -> SResult<PlaceRef> {
        let (ty, _, idx) = *expr.0.root_data();
        let origin = PlaceOrigin::Expr(expr_id, idx);
        Ok(match &expr.0 {
            ExpressionHead::Niladic(n) => match &n.inner {
                ResolvedAtom::Val(val, backtracks) => self
                    .load_var(*val, ty, place, origin, !*backtracks)
                    .expect("Invalid refernce to variable"),
                ResolvedAtom::Captured(cap, backtracks) => {
                    self.load_captured(*cap, ty, place, origin, !*backtracks)?
                }
                ResolvedAtom::Number(n) => self.load_int(*n, ty, place, origin),
                ResolvedAtom::Char(c) => self.load_char(*c, ty, place, origin),
                ResolvedAtom::Bool(b) => self.load_bool(*b, ty, place, origin),
                ResolvedAtom::ParserDef(_, _)
                | ResolvedAtom::Single
                | ResolvedAtom::Nil
                | ResolvedAtom::Regex(..) => self.unwrap_or_stack(place, ty, origin),
                ResolvedAtom::Block(block) => {
                    self.create_block_parser(*block, ty, place, origin)?
                }
            },
            ExpressionHead::Monadic(Monadic { op, inner }) => {
                let inner_ty = inner.0.root_data().0;
                let inner_origin = PlaceOrigin::Expr(expr_id, inner.0.root_data().2);
                let recurse = |ctx: &mut Self, plc| ctx.convert_expr(expr_id, inner, plc);
                match &op.inner {
                    ValUnOp::Not | ValUnOp::Neg => {
                        let op: IntUnOp = (&op.inner).try_into().unwrap();
                        let inner = self.copy_if_different_levels(
                            self.int,
                            inner_ty,
                            None,
                            inner_origin,
                            recurse,
                        )?;
                        let place_ref = self.unwrap_or_stack(place, ty, origin);
                        self.f.append_ins(MirInstr::IntUn(place_ref, op, inner));
                        place_ref
                    }
                    ValUnOp::Wiggle(constr, kind) => {
                        if matches!(self.db.lookup_intern_type(ty), Type::ParserArg { .. }) {
                            let place_ref = self.unwrap_or_stack(place, ty, origin);
                            let inner_place = self.f.add_place(PlaceInfo {
                                place: Place::Front(place_ref),
                                ty,
                                remove_bt: false,
                            });
                            self.copy_if_different_levels(
                                ty,
                                inner_ty,
                                Some(inner_place),
                                inner_origin,
                                recurse,
                            )?;
                            place_ref
                        } else {
                            let place_ref = self.copy_if_different_levels(
                                self.int, inner_ty, place, origin, recurse,
                            )?;
                            let place_ldt = self.db.least_deref_type(ty)?;
                            let ldt_ref = self.new_stack_place(place_ldt, inner_origin);
                            self.copy(place_ref, ldt_ref);
                            let old_backtrack = self.retreat.backtrack;
                            self.retreat.backtrack = match kind {
                                WiggleKind::If => self.retreat.backtrack,
                                WiggleKind::Try => self.retreat.error,
                            };
                            let cont = self.f.new_bb();
                            self.convert_constraint(*constr, ldt_ref, cont)?;
                            self.f.set_bb(cont);
                            self.retreat.backtrack = old_backtrack;
                            place_ref
                        }
                    }
                    ValUnOp::Dot(field, bt) => {
                        let inner_ldt = self.db.least_deref_type(inner_ty)?;
                        let block_ref = self.copy_if_different_levels(
                            inner_ldt,
                            inner_ty,
                            None,
                            inner_origin,
                            recurse,
                        )?;
                        let place_ref = self.unwrap_or_stack(place, ty, origin);
                        let backtrack = if *bt {
                            self.retreat.backtrack
                        } else {
                            self.retreat.error
                        };
                        self.f
                            .field(block_ref, *field, place_ref, self.retreat.error, backtrack);
                        place_ref
                    }
                }
            }
            ExpressionHead::Dyadic(Dyadic {
                op,
                inner: [left, right],
            }) => {
                let left_ty = left.0.root_data().0;
                let right_ty = right.0.root_data().0;
                let lrecurse = |ctx: &mut Self, plc| ctx.convert_expr(expr_id, left, plc);
                let rrecurse = |ctx: &mut Self, plc| ctx.convert_expr(expr_id, right, plc);
                let lorigin = PlaceOrigin::Expr(expr_id, left.0.root_data().2);
                let rorigin = PlaceOrigin::Expr(expr_id, right.0.root_data().2);
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
                        let left = self
                            .copy_if_different_levels(self.int, left_ty, None, lorigin, lrecurse)?;
                        let right = self.copy_if_different_levels(
                            self.int, right_ty, None, rorigin, rrecurse,
                        )?;
                        let place_ref = self.unwrap_or_stack(place, ty, origin);
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
                        let left = self
                            .copy_if_different_levels(self.int, left_ty, None, lorigin, lrecurse)?;
                        let right = self.copy_if_different_levels(
                            self.int, right_ty, None, rorigin, rrecurse,
                        )?;
                        let place_ref = self.unwrap_or_stack(place, ty, origin);
                        self.f
                            .append_ins(MirInstr::Comp(place_ref, op, left, right));
                        place_ref
                    }
                    ValBinOp::ParserApply => {
                        let left_ldt = self.db.least_deref_type(left_ty)?;
                        let left = self
                            .copy_if_different_levels(left_ldt, left_ty, None, lorigin, lrecurse)?;
                        let right = rrecurse(self, None)?;
                        let place_ref = self.unwrap_or_stack(place, ty, origin);
                        self.f.parse_call(
                            make_bitflags!(NeededBy::{Val | Backtrack}),
                            left,
                            right,
                            Some(place_ref),
                            None,
                            self.retreat,
                        );
                        place_ref
                    }
                    ValBinOp::Else => {
                        let place_ref = self.unwrap_or_stack(place, ty, origin);
                        let right_bb = self.f.new_bb();
                        let continue_bb = self.f.new_bb();
                        let old_backtrack = self.retreat.backtrack;
                        self.retreat.backtrack = right_bb;
                        self.copy_if_different_levels(
                            ty,
                            left_ty,
                            Some(place_ref),
                            lorigin,
                            lrecurse,
                        )?;
                        self.f.branch(continue_bb);
                        self.f.set_bb(right_bb);
                        self.retreat.backtrack = old_backtrack;
                        self.copy_if_different_levels(
                            ty,
                            right_ty,
                            Some(place_ref),
                            rorigin,
                            rrecurse,
                        )?;
                        self.f.branch(continue_bb);
                        self.f.set_bb(continue_bb);
                        place_ref
                    }
                    ValBinOp::Compose => unreachable!(),
                }
            }
            ExpressionHead::Variadic(Variadic { inner, .. }) => {
                let fun_ty = inner[0].0.root_data().0;
                let fun_ldt = self.db.least_deref_type(fun_ty)?;
                let fun_origin = PlaceOrigin::Expr(expr_id, inner[0].0.root_data().2);
                let fun_arg_num =
                    if let Type::FunctionArg(_, args) = self.db.lookup_intern_type(fun_ldt) {
                        args.len()
                    } else {
                        unreachable!()
                    };
                let fun_place = self.copy_if_different_levels(
                    fun_ldt,
                    fun_ty,
                    None,
                    fun_origin,
                    |ctx, plc| ctx.convert_expr(expr_id, &inner[0], plc),
                )?;
                let place_ref = self.unwrap_or_stack(place, ty, origin);
                let inner_results = inner[1..]
                    .iter()
                    .map(|inner| self.convert_expr(expr_id, inner, None))
                    .collect::<Result<Vec<_>, _>>()?;
                // fun_arg_num: 6           (available arguments)
                // inner_results.len(): 4   (given arguments)
                // _________________________
                // | 5 | 4 | 3 | 2 | 1 | 0 | <- index for set_arg functions (which lowers to vtable->set_arg_info[-1 - index])
                // |[0]|[1]|[2]|[3]|___|___| <- inner_results ([i])
                // \_______________/\______/
                //    applied args  unapplied
                let first_arg_index = fun_arg_num as u64 - 1;
                self.f.apply_args(
                    fun_place,
                    inner_results,
                    place_ref,
                    first_arg_index,
                    self.retreat.error,
                );
                place_ref
            }
        })
    }

    fn convert_constraint(
        &mut self,
        expr: HirConstraintId,
        val: PlaceRef,
        mut cont: BBRef,
    ) -> SResult<()> {
        match self.db.lookup_intern_hir_constraint(expr) {
            ExpressionHead::Niladic(n) => {
                self.f.assert_val(val, n.inner, self.retreat.backtrack);
                self.f.branch(cont);
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
                    self.convert_constraint(inner[0], val, right_bb)?;
                    self.f.set_bb(right_bb);
                    self.convert_constraint(inner[1], val, cont)?;
                    Ok(())
                }
                ConstraintBinOp::Or => {
                    let right_bb = self.f.new_bb();
                    let old_backtrack = self.retreat.backtrack;
                    self.retreat.backtrack = right_bb;
                    self.convert_constraint(inner[0], val, cont)?;
                    self.f.set_bb(right_bb);
                    self.retreat.backtrack = old_backtrack;
                    self.convert_constraint(inner[1], val, cont)?;
                    Ok(())
                }
            },
            ExpressionHead::Variadic(v) => v.ignore(),
        }
    }

    fn terminate_context(&mut self, context: ContextId, superchoice: ChoiceId, cont: BBRef) {
        if !self.context_bb.contains_key(&context) {
            // context was never referenced and contains nothing
            self.context_data
                .remove(&context)
                .expect("context not initialized or removed multiple times");
            return;
        }
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
            if let Some(ret) = self.f.fun.ret() {
                for field in context_data.affected_discriminants {
                    let has_field = context_data.field_ids.get(&field).is_some();
                    self.f
                        .append_ins(MirInstr::SetDiscriminant(ret, field, has_field));
                }
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
        self.f.branch(cont);
    }

    fn end_choice(&mut self, subcontexts: &[ContextId], id: ChoiceId) {
        let parent_context = self
            .current_context
            .expect("end choice called without context");
        let first_context_bb = match subcontexts.get(0) {
            Some(x) => self.context_bb[x].0,
            None => return,
        };
        self.f.branch(first_context_bb);
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

    fn call_expr(
        &mut self,
        call_loc: DefId,
        expr: ExprId,
        call_kind: RequirementSet,
    ) -> SResult<()> {
        let parser_fun = self.val_place_at_def(expr.0).unwrap();
        let parser_ty = self.f.fun.place(parser_fun).ty;
        let ldt_parser = self.db.least_deref_type(parser_ty)?;
        let ldt_parser_fun = self.copy_if_different_levels(
            ldt_parser,
            parser_ty,
            None,
            PlaceOrigin::Node(expr.0),
            |_, _| Ok(parser_fun),
        )?;
        let addr = self.front_place_at_def(call_loc).unwrap();
        let ret = call_kind
            .contains(NeededBy::Val)
            .then(|| self.val_place_at_def(call_loc).unwrap());
        let ty = self.f.fun.place(self.f.fun.arg()).ty;
        let retlen = if call_kind.contains(NeededBy::Len) {
            let place = self.f.add_place(PlaceInfo {
                place: Place::ModifiedBy(self.next_ins()),
                ty,
                remove_bt: false,
            });
            self.places.insert(SubValue::new_back(call_loc), place);
            Some(place)
        } else {
            None
        };
        self.f
            .parse_call(call_kind, addr, ldt_parser_fun, ret, retlen, self.retreat);
        Ok(())
    }

    fn req_at_id(&self, id: DefId) -> RequirementSet {
        self.req_transformer[&id] * self.req
    }

    fn parse_statement(&mut self, parse: &hir::ParseStatement) -> SResult<()> {
        if !self.processed_parse_sites.insert(parse.id.0) {
            return Ok(());
        }
        let req = self.req_at_id(parse.id.0);
        if !req.is_empty() {
            self.call_expr(parse.id.0, parse.expr, req)?
        }
        Ok(())
    }

    pub fn parserdef(&mut self, pd: &hir::ParserDef) -> SResult<()> {
        if !self.processed_parse_sites.insert(pd.id.0) {
            return Ok(());
        }
        let req = self.req_at_id(pd.id.0);
        if req.is_empty() {
            return Ok(());
        }
        let call_loc = pd.id.0;
        let expr = pd.to;
        let parser_fun = self.val_place_at_def(expr.0).unwrap();
        let parser_ty = self.f.fun.place(parser_fun).ty;
        let ldt_parser = self.db.least_deref_type(parser_ty)?;
        let ldt_parser_fun = self.copy_if_different_levels(
            ldt_parser,
            parser_ty,
            None,
            PlaceOrigin::Node(expr.0),
            |_, _| Ok(parser_fun),
        )?;
        let addr = self.front_place_at_def(call_loc).unwrap();
        let ret = req
            .contains(NeededBy::Val)
            .then(|| self.val_place_at_def(call_loc).unwrap());
        let retlen = req
            .contains(NeededBy::Len)
            .then(|| self.back_place_at_def(call_loc).unwrap());
        self.f
            .parse_call(req, addr, ldt_parser_fun, ret, retlen, self.retreat);
        Ok(())
    }

    pub fn if_parser(&mut self, constr: HirConstraintId) -> SResult<()> {
        let ldt_ret = if let Some(ret) = self.f.fun.ret() {
            let ret_ty = self.f.fun.place(ret).ty;
            let ldt_ret = self.db.least_deref_type(ret_ty)?;
            Some(ldt_ret)
        } else {
            None
        };
        let tmp_place = self.new_stack_place(ldt_ret.unwrap(), PlaceOrigin::Ret);
        let arg_ty = self.f.fun.place(self.f.fun.arg()).ty;
        let arg_tmp_place = self.new_stack_place(arg_ty, PlaceOrigin::Arg);
        let retlen = self.req.contains(NeededBy::Len).then(|| self.f.fun.arg());
        let fun_place = self.f.fun.cap();
        let fun_ty = self.f.fun.place(fun_place).ty;
        let inner_fun_place = self.f.add_place(PlaceInfo {
            place: Place::Front(fun_place),
            ty: fun_ty,
            remove_bt: false,
        });

        // the following parse call might mutate the argument, therefore we make a copy
        self.copy(self.f.fun.arg(), arg_tmp_place);

        self.f.parse_call(
            self.req | NeededBy::Val,
            self.f.fun.arg(),
            inner_fun_place,
            Some(tmp_place),
            retlen,
            self.retreat,
        );
        let convert_succ = self.f.new_bb();
        self.convert_constraint(constr, tmp_place, convert_succ)?;
        self.f.set_bb(convert_succ);
        let ret_if_needed = self
            .req
            .contains(NeededBy::Val)
            .then(|| self.f.fun.ret().unwrap());
        if (self.req & NeededBy::Val).is_empty() {
            return Ok(());
        }
        self.f.parse_call(
            self.req & NeededBy::Val,
            arg_tmp_place,
            inner_fun_place,
            ret_if_needed,
            None,
            self.retreat,
        );
        Ok(())
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
                let mut idx: usize = 0;
                let resolved_expr = self.db.parser_expr_at(e.id)?.map_mut(&mut |f| {
                    let res = (f.0, f.1, idx);
                    idx += 1;
                    res
                });
                let place = self.val_place_at_def(e.id.0).unwrap();
                self.convert_expr(e.id, &resolved_expr, Some(place))?;
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
                let last_place_back = self.context_data[&b.root_context]
                    .ends
                    .map(|x| self.back_place_at_def(x.1).unwrap())
                    .unwrap_or_else(|| self.f.fun.arg());
                let current_place = self.back_place_at_def(b.id.0).unwrap();
                self.copy(last_place_back, current_place);
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
                    HirNode::Expr(e) => db.parser_expr_at(e.id)?.0.root_data().0,
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
        let block_expr = db.parser_expr_at(block.enclosing_expr)?;
        let block_ty = ExprIter::new(&block_expr)
            .find_map(|x| match &x.0 {
                ExpressionHead::Niladic(expr::OpWithData {
                    inner: ResolvedAtom::Block(b),
                    data,
                }) if *b == id => Some(*data),
                _ => None,
            })
            .expect("could not find block within enclosing expression")
            .0;
        let Type::ParserArg { result, arg } = db.lookup_intern_type(block_ty) else {
            dbpanic!(db, "should have been a parser type, was {}", &block_ty)
        };
        let mut f: FunctionWriter = FunctionWriter::new(block_ty, arg, result, requirements);
        let mut top_level_retreat = f.make_top_level_retreat();
        if !requirements.contains(NeededBy::Backtrack) {
            top_level_retreat.backtrack = top_level_retreat.error;
        }
        let retreat = top_level_retreat;
        let int: TypeId = db.intern_type(Type::Primitive(PrimitiveType::Int));
        let places = Self::block_places(db, &block, requirements, order, &mut f)?;
        let context_data: FxHashMap<ContextId, ContextData> =
            ContextData::build_context_tree(db, block.root_context)?;
        let mut context_bb: FxHashMap<ContextId, (BBRef, BBRef)> = FxHashMap::default();
        context_bb.insert(block.root_context, (f.fun.entry(), f.fun.entry()));
        let current_context: Option<ContextId> = Some(block.root_context);
        let returns_self: bool = requirements.contains(NeededBy::Val) && !block.returns;
        let processed_parse_sites = FxHashSet::default();
        let req_transformer = order.parse_requirements.clone();
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
            processed_parse_sites,
            req: requirements,
            req_transformer,
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
        let expr_ty = expr.0.root_data().0;
        let mut idx = 0;
        let idx = expr.fold(&mut |_| {
            let i = idx;
            idx += 1;
            i
        });
        let expr_place = Place::Stack(f.new_stack_ref(PlaceOrigin::Expr(pd.to, idx)));
        let expr_place_ref = f.add_place(PlaceInfo {
            place: expr_place,
            ty: expr_ty,
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
        let sig = db.parser_args(id)?;
        let from = sig.from.unwrap_or_else(|| db.intern_type(Type::Any));
        let thunk = db.intern_type(Type::Nominal(sig.thunk));
        let ret_ty = db.parser_returns(id)?.deref;
        let fun_ty = db.intern_type(Type::ParserArg {
            result: thunk,
            arg: from,
        });
        let arg_ty = from;
        let mut f = FunctionWriter::new(fun_ty, arg_ty, ret_ty, requirements);
        let mut top_level_retreat = f.make_top_level_retreat();
        if !requirements.contains(NeededBy::Backtrack) {
            top_level_retreat.backtrack = top_level_retreat.error;
        }
        let retreat = top_level_retreat;
        let int: TypeId = db.intern_type(Type::Primitive(PrimitiveType::Int));
        let context_data = FxHashMap::default();
        let context_bb = FxHashMap::default();
        let places = Self::pd_places(db, id, &mut f)?;
        let current_context = None;
        let returns_self = false;
        let processed_parse_sites = FxHashSet::default();
        let mut req_transformer = BTreeMap::default();
        req_transformer.insert(id.0, RequirementMatrix::id());
        let req_transformer = Arc::new(req_transformer);
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
            processed_parse_sites,
            req: requirements,
            req_transformer,
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
        let int: TypeId = db.intern_type(Type::Primitive(PrimitiveType::Int));
        f.set_bb(f.fun.entry());
        Ok(ConvertCtx {
            db,
            int,
            f,
            retreat,
            top_level_retreat,
            req: requirements,
            places: Default::default(),
            context_bb: Default::default(),
            current_context: Default::default(),
            context_data: Default::default(),
            returns_self: false,
            processed_parse_sites: Default::default(),
            req_transformer: Default::default(),
        })
    }

    pub fn finish_fun(mut self) -> Function {
        self.f.ret(ReturnStatus::Ok);
        self.f.fun
    }
}
