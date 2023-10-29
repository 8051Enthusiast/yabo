use std::{
    ops::{Deref, DerefMut},
    sync::Arc,
};

use super::Mirs;
use fxhash::FxHashMap;
use yaboc_ast::expr::{BtMarkKind, ConstraintBinOp, ConstraintUnOp, WiggleKind};
use yaboc_base::{error::SResult, interner::DefId};
use yaboc_dependents::{NeededBy, SubValue, SubValueKind};
use yaboc_expr::{ExprHead, ExprIdx, Expression, FetchKindData, IdxExpression, IndexExpr, ZipExpr};
use yaboc_hir::{BlockId, ExprId, HirConstraint};
use yaboc_hir_types::FullTypeId;
use yaboc_resolve::expr::{Resolved, ResolvedAtom, ValBinOp, ValUnOp, ValVarOp};
use yaboc_types::{PrimitiveType, Type, TypeId};

use crate::{
    BBRef, CallMeta, Comp, ExceptionRetreat, FunctionWriter, IntBinOp, IntUnOp, Place, PlaceInfo,
    PlaceOrigin, PlaceRef, ZstVal,
};

// anyone wanna play some type tetris?
type IndexTypeExpr = ZipExpr<
    Arc<IdxExpression<Resolved>>,
    <Resolved as FetchKindData<(ExprIdx<Resolved>, FullTypeId), ExprId, dyn Mirs>>::Data,
>;

pub struct ConvertExpr<'a> {
    db: &'a dyn Mirs,
    int: TypeId,
    pub f: FunctionWriter,
    pub retreat: ExceptionRetreat,
    places: FxHashMap<SubValue, PlaceRef>,
}

impl<'a> Deref for ConvertExpr<'a> {
    type Target = FunctionWriter;

    fn deref(&self) -> &Self::Target {
        &self.f
    }
}

impl<'a> DerefMut for ConvertExpr<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.f
    }
}

impl<'a> ConvertExpr<'a> {
    pub fn new(
        db: &'a dyn Mirs,
        f: FunctionWriter,
        retreat: ExceptionRetreat,
        places: FxHashMap<SubValue, PlaceRef>,
    ) -> Self {
        let int = db.intern_type(Type::Primitive(PrimitiveType::Int));
        ConvertExpr {
            db,
            int,
            f,
            retreat,
            places,
        }
    }

    pub fn register_place(&mut self, sub: SubValue, place: PlaceRef) {
        assert!(self.places.insert(sub, place).is_none());
    }

    pub fn val_place_at_def(&self, id: DefId) -> Option<PlaceRef> {
        self.places
            .get(&SubValue {
                kind: SubValueKind::Val,
                id,
            })
            .copied()
    }

    pub fn front_place_at_def(&self, id: DefId) -> Option<PlaceRef> {
        self.places
            .get(&SubValue {
                kind: SubValueKind::Front,
                id,
            })
            .copied()
    }

    pub fn back_place_at_def(&self, id: DefId) -> Option<PlaceRef> {
        self.places
            .get(&SubValue {
                kind: SubValueKind::Back,
                id,
            })
            .copied()
    }

    pub fn create_current_ins_retlen_place(&mut self, ty: TypeId, call_loc: DefId) -> PlaceRef {
        let place = self.f.add_place(PlaceInfo {
            place: Place::ModifiedBy(self.f.next_ins()),
            ty,
            remove_bt: false,
        });
        self.places.insert(SubValue::new_back(call_loc), place);
        place
    }

    pub fn unwrap_or_stack(
        &mut self,
        place: Option<PlaceRef>,
        ty: TypeId,
        origin: PlaceOrigin,
    ) -> PlaceRef {
        place.unwrap_or_else(|| self.f.new_stack_place(ty, origin))
    }

    pub fn copy(&mut self, origin: PlaceRef, target: PlaceRef) {
        self.f.copy(origin, target, self.retreat.error);
    }

    pub fn copy_if_different_levels(
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
    ) -> Option<PlaceRef> {
        let place_ref = self.val_place_at_def(var)?;
        if self.f.fun.place(place_ref).ty == ty && place.is_none() {
            return Some(place_ref);
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
    ) -> SResult<PlaceRef> {
        let cap_ty = self.db.parser_type_at(captured)?;
        let place_ref = self.f.add_place(PlaceInfo {
            place: Place::Captured(self.f.fun.cap(), captured),
            ty: cap_ty,
            remove_bt: false,
        });
        if self.db.deref_level(ty) == self.db.deref_level(cap_ty) && place.is_none() {
            return Ok(place_ref);
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
        self.f.load_int(n, place_ref);
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
        self.f.load_char(c, place_ref);
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
        self.f.load_bool(b, place_ref);
        place_ref
    }

    fn load_zst(
        &mut self,
        val: ZstVal,
        ty: TypeId,
        place: Option<PlaceRef>,
        origin: PlaceOrigin,
    ) -> PlaceRef {
        // loading a zst requires keeping the mono layout information
        // in order to get the vtable, which may be lost if it is
        // loaded directly into a multi-layout so we make a stack place
        // which is guaranteed to be mono
        let stack_place = self.new_stack_place(ty, origin);
        self.f.load_zst(val, stack_place);
        if let Some(place) = place {
            self.copy(stack_place, place);
            place
        } else {
            stack_place
        }
    }

    fn load_undef(&mut self, ty: TypeId, place: Option<PlaceRef>, origin: PlaceOrigin) -> PlaceRef {
        let place_ref = self.unwrap_or_stack(place, ty, origin);
        self.f.load_undef(place_ref);
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

    pub fn convert_constraint(
        &mut self,
        expr: &IdxExpression<HirConstraint>,
        idx: ExprIdx<HirConstraint>,
        val: PlaceRef,
        mut cont: BBRef,
    ) -> SResult<()> {
        match &expr[idx] {
            ExprHead::Niladic(n) => {
                self.f.assert_val(val, n.clone(), self.retreat.backtrack);
                self.f.branch(cont);
                Ok(())
            }
            ExprHead::Monadic(op, inner) => match op {
                ConstraintUnOp::Not => {
                    std::mem::swap(&mut cont, &mut self.retreat.backtrack);
                    self.convert_constraint(expr, *inner, val, cont)?;
                    self.retreat.backtrack = cont;
                    Ok(())
                }
                ConstraintUnOp::Dot(_) => todo!(),
            },
            ExprHead::Dyadic(op, [lhs, rhs]) => match op {
                ConstraintBinOp::And => {
                    let right_bb = self.f.new_bb();
                    self.convert_constraint(expr, *lhs, val, right_bb)?;
                    self.f.set_bb(right_bb);
                    self.convert_constraint(expr, *rhs, val, cont)?;
                    Ok(())
                }
                ConstraintBinOp::Or => {
                    let right_bb = self.f.new_bb();
                    let old_backtrack = self.retreat.backtrack;
                    self.retreat.backtrack = right_bb;
                    self.convert_constraint(expr, *lhs, val, cont)?;
                    self.f.set_bb(right_bb);
                    self.retreat.backtrack = old_backtrack;
                    self.convert_constraint(expr, *rhs, val, cont)?;
                    Ok(())
                }
            },
            ExprHead::Variadic(v, _) => match *v {},
        }
    }

    pub fn convert_expr(
        &mut self,
        expr_id: ExprId,
        expr: &IndexTypeExpr,
        place: Option<PlaceRef>,
        fun_dep: impl Fn(ExprIdx<Resolved>) -> bool,
    ) -> SResult<PlaceRef> {
        self.convert_expr_impl(expr_id, expr, expr.expr.root(), place, &fun_dep)
    }

    fn convert_expr_impl(
        &mut self,
        expr_id: ExprId,
        expr: &IndexTypeExpr,
        idx: ExprIdx<Resolved>,
        place: Option<PlaceRef>,
        fun_dep: &impl Fn(ExprIdx<Resolved>) -> bool,
    ) -> SResult<PlaceRef> {
        let (idx, &ty) = expr.data.index_expr(idx);
        let origin = PlaceOrigin::Expr(expr_id, idx);
        if !fun_dep(idx) {
            return Ok(self.load_undef(ty, place, origin));
        }
        Ok(match expr.expr.index_expr(idx) {
            ExprHead::Niladic(n) => match n {
                ResolvedAtom::Val(val) => self
                    .load_var(*val, ty, place, origin)
                    .expect("Invalid refernce to variable"),
                ResolvedAtom::Captured(cap) => self.load_captured(*cap, ty, place, origin)?,
                ResolvedAtom::Number(n) => self.load_int(*n, ty, place, origin),
                ResolvedAtom::Char(c) => self.load_char(*c, ty, place, origin),
                ResolvedAtom::Bool(b) => self.load_bool(*b, ty, place, origin),
                ResolvedAtom::ParserDef(pd) => {
                    self.load_zst(ZstVal::ParserDef(*pd), ty, place, origin)
                }
                ResolvedAtom::Single => self.load_zst(ZstVal::Single, ty, place, origin),
                ResolvedAtom::Nil => self.load_zst(ZstVal::Nil, ty, place, origin),
                ResolvedAtom::Array => self.load_zst(ZstVal::Array, ty, place, origin),
                ResolvedAtom::Regex(regex, _) => {
                    self.load_zst(ZstVal::Regex(*regex), ty, place, origin)
                }
                ResolvedAtom::Block(block) => {
                    self.create_block_parser(*block, ty, place, origin)?
                }
            },
            ExprHead::Monadic(op, inner) => {
                let (inner_idx, &inner_ty) = expr.data.index_expr(*inner);
                let inner_origin = PlaceOrigin::Expr(expr_id, inner_idx);
                let recurse = |ctx: &mut Self, plc| {
                    ctx.convert_expr_impl(expr_id, expr, *inner, plc, fun_dep)
                };
                match &op {
                    ValUnOp::Not | ValUnOp::Neg => {
                        let op: IntUnOp = op.try_into().unwrap();
                        let inner = self.copy_if_different_levels(
                            self.int,
                            inner_ty,
                            None,
                            inner_origin,
                            recurse,
                        )?;
                        let place_ref = self.unwrap_or_stack(place, ty, origin);
                        self.f.int_un_op(place_ref, op, inner);
                        place_ref
                    }
                    ValUnOp::Size => {
                        let inner_ldt = self.db.least_deref_type(inner_ty)?;
                        let inner = self.copy_if_different_levels(
                            inner_ldt,
                            inner_ty,
                            None,
                            inner_origin,
                            recurse,
                        )?;
                        let place_ref = self.unwrap_or_stack(place, ty, origin);
                        self.f.len_call(inner, place_ref, self.retreat);
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
                            let ldt_ref = self.f.new_stack_place(place_ldt, inner_origin);
                            self.copy(place_ref, ldt_ref);
                            let old_backtrack = self.retreat.backtrack;
                            self.retreat.backtrack = match kind {
                                WiggleKind::If => self.retreat.backtrack,
                                WiggleKind::Try => self.retreat.error,
                            };
                            let cont = self.f.new_bb();
                            let constr = self.db.lookup_intern_hir_constraint(*constr);
                            let root = constr.expr.root();
                            self.convert_constraint(&constr.expr, root, ldt_ref, cont)?;
                            self.f.set_bb(cont);
                            self.retreat.backtrack = old_backtrack;
                            place_ref
                        }
                    }
                    ValUnOp::Dot(field, acc) => {
                        let inner_ldt = self.db.least_deref_type(inner_ty)?;
                        let block_ref = self.copy_if_different_levels(
                            inner_ldt,
                            inner_ty,
                            None,
                            inner_origin,
                            recurse,
                        )?;
                        let place_ref = self.unwrap_or_stack(place, ty, origin);
                        let backtrack = if acc.can_backtrack() {
                            self.retreat.backtrack
                        } else {
                            self.retreat.error
                        };
                        self.f
                            .field(block_ref, *field, place_ref, self.retreat.error, backtrack);
                        place_ref
                    }
                    ValUnOp::BtMark(BtMarkKind::KeepBt) => recurse(self, place)?,
                    ValUnOp::BtMark(BtMarkKind::RemoveBt) => {
                        let remove_bt = self.new_remove_bt_stack_place(ty, origin);
                        let val_without_bt = recurse(self, Some(remove_bt))?;
                        if let Some(place) = place {
                            self.copy(val_without_bt, place);
                            place
                        } else {
                            val_without_bt
                        }
                    }
                    ValUnOp::EvalFun => {
                        let inner_ldt = self.db.least_deref_type(inner_ty)?;
                        let fun_ref = self.copy_if_different_levels(
                            inner_ldt,
                            inner_ty,
                            None,
                            inner_origin,
                            recurse,
                        )?;
                        let place_ref = self.unwrap_or_stack(place, ty, origin);
                        self.f.eval_fun(fun_ref, place_ref, self.retreat);
                        place_ref
                    }
                    ValUnOp::GetAddr => {
                        let inner_ldt = self.db.least_deref_type(inner_ty)?;
                        let inner = self.copy_if_different_levels(
                            inner_ldt,
                            inner_ty,
                            None,
                            inner_origin,
                            recurse,
                        )?;
                        let place_ref = self.unwrap_or_stack(place, ty, origin);
                        self.f.get_addr(inner, place_ref, self.retreat);
                        place_ref
                    }
                }
            }
            ExprHead::Dyadic(op, [left, right]) => {
                let (left_idx, &left_ty) = expr.data.index_expr(*left);
                let (right_idx, &right_ty) = expr.data.index_expr(*right);
                let lrecurse =
                    |ctx: &mut Self, plc| ctx.convert_expr_impl(expr_id, expr, *left, plc, fun_dep);
                let rrecurse = |ctx: &mut Self, plc| {
                    ctx.convert_expr_impl(expr_id, expr, *right, plc, fun_dep)
                };
                let lorigin = PlaceOrigin::Expr(expr_id, left_idx);
                let rorigin = PlaceOrigin::Expr(expr_id, right_idx);
                match &op {
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
                        let op: IntBinOp = op.try_into().unwrap();
                        let left = self
                            .copy_if_different_levels(self.int, left_ty, None, lorigin, lrecurse)?;
                        let right = self.copy_if_different_levels(
                            self.int, right_ty, None, rorigin, rrecurse,
                        )?;
                        let place_ref = self.unwrap_or_stack(place, ty, origin);
                        self.f.int_bin_op(place_ref, op, left, right);
                        place_ref
                    }
                    ValBinOp::LesserEq
                    | ValBinOp::Lesser
                    | ValBinOp::GreaterEq
                    | ValBinOp::Greater
                    | ValBinOp::Uneq
                    | ValBinOp::Equals => {
                        let op: Comp = op.try_into().unwrap();
                        let left = self
                            .copy_if_different_levels(self.int, left_ty, None, lorigin, lrecurse)?;
                        let right = self.copy_if_different_levels(
                            self.int, right_ty, None, rorigin, rrecurse,
                        )?;
                        let place_ref = self.unwrap_or_stack(place, ty, origin);
                        self.f.comp(place_ref, op, left, right);
                        place_ref
                    }
                    ValBinOp::ParserApply => {
                        let left_ldt = self.db.least_deref_type(left_ty)?;
                        let left_plc = self.new_stack_place(left_ldt, lorigin);
                        let left_plc = lrecurse(self, Some(left_plc))?;
                        let right_ldt = self.db.least_deref_type(right_ty)?;
                        let right = self.copy_if_different_levels(
                            right_ldt, right_ty, None, rorigin, rrecurse,
                        )?;
                        let place_ref = self.unwrap_or_stack(place, ty, origin);
                        self.f.parse_call(
                            CallMeta {
                                req: NeededBy::Val | NeededBy::Backtrack,
                                tail: false,
                            },
                            left_plc,
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
                    ValBinOp::Then => {
                        let _ = lrecurse(self, None)?;
                        let place_ref = self.unwrap_or_stack(place, ty, origin);
                        rrecurse(self, Some(place_ref))?
                    }
                }
            }
            ExprHead::Variadic(ValVarOp::PartialApply, inner) => {
                let (fun_idx, &fun_ty) = expr.data.index_expr(inner[0]);
                let fun_ldt = self.db.least_deref_type(fun_ty)?;
                let fun_origin = PlaceOrigin::Expr(expr_id, fun_idx);
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
                    |ctx, plc| ctx.convert_expr_impl(expr_id, expr, inner[0], plc, fun_dep),
                )?;
                let place_ref = self.unwrap_or_stack(place, ty, origin);
                let inner_results = inner[1..]
                    .iter()
                    .map(|inner| self.convert_expr_impl(expr_id, expr, *inner, None, fun_dep))
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
}
