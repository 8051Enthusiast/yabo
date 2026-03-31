use std::{
    collections::BTreeSet,
    convert::Infallible,
    ops::{Deref, DerefMut},
    sync::Arc,
};

use super::Mirs;
use fxhash::FxHashMap;
use yaboc_ast::expr::{BtMarkKind, ConstraintBinOp, ConstraintUnOp, WiggleKind};
use yaboc_base::{
    error::SResult,
    interner::{DefId, FieldName},
};
use yaboc_dependents::{SubValue, SubValueKind, requirements::ExprDepData};
use yaboc_expr::{ExprHead, ExprIdx, Expression, FetchKindData, IdxExpression, IndexExpr, ZipExpr};
use yaboc_hir::{
    BlockId, ExprId, HirConstraint, HirConstraintId, HirIdWrapper, HirNode, LambdaId,
    ParseStatement, ParserDefId, ParserPredecessor,
};
use yaboc_hir_types::FullTypeId;
use yaboc_req::{NeededBy, RequirementSet};
use yaboc_resolve::expr::{EvalKind, Resolved, ResolvedAtom, ValBinOp, ValUnOp, ValVarOp};
use yaboc_types::{Type, TypeId};

use crate::{
    BBRef, CallMeta, Comp, ExceptionRetreat, FunctionWriter, IntBinOp, IntUnOp, Place, PlaceInfo,
    PlaceOrigin, PlaceRef, UninitVal,
};

// anyone wanna play some type tetris?
type IndexTypeExpr =
    ZipExpr<
        Arc<IdxExpression<Resolved>>,
        <Resolved as FetchKindData<
            ((ExprIdx<Resolved>, FullTypeId), ExprDepData),
            ExprId,
            dyn Mirs,
        >>::Data,
    >;

pub struct ExprInfo<'a> {
    id: ExprId,
    content: &'a IndexTypeExpr,
    reqs: &'a [RequirementSet],
}

impl Clone for ExprInfo<'_> {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for ExprInfo<'_> {}

impl ExprInfo<'_> {
    fn req(&self, idx: ExprIdx<Resolved>) -> RequirementSet {
        self.reqs[idx.as_usize()]
    }
}

#[derive(Clone, Copy)]
pub struct ExpressionLoc {
    pub eval: bool,
    pub place: Option<PlaceRef>,
    pub origin: PlaceOrigin,
}

pub struct ConvertExpr<'a> {
    db: &'a dyn Mirs,
    pub f: FunctionWriter,
    pub retreat: ExceptionRetreat,
    places: FxHashMap<SubValue, PlaceRef>,
}

impl Deref for ConvertExpr<'_> {
    type Target = FunctionWriter;

    fn deref(&self) -> &Self::Target {
        &self.f
    }
}

impl DerefMut for ConvertExpr<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.f
    }
}

fn modify_reqs(reqs: RequirementSet, inner_can_bt: bool, eval_kind: EvalKind) -> RequirementSet {
    match (inner_can_bt, eval_kind) {
        (true, EvalKind::IfInnerBlock) | (_, EvalKind::Backtrack(BtMarkKind::KeepBt)) => reqs,
        _ => reqs & !NeededBy::Backtrack,
    }
}

impl<'a> ConvertExpr<'a> {
    pub fn new(
        db: &'a dyn Mirs,
        f: FunctionWriter,
        retreat: ExceptionRetreat,
        places: FxHashMap<SubValue, PlaceRef>,
    ) -> Self {
        ConvertExpr {
            db,
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

    pub fn create_current_ins_retlen_place(&mut self, call_loc: DefId) -> PlaceRef {
        let place = self.f.add_place(PlaceInfo {
            place: Place::ModifiedBy(self.f.next_ins()),
            eval: true,
        });
        self.places.insert(SubValue::new_back(call_loc), place);
        place
    }

    pub fn unwrap_or_stack(&mut self, loc: ExpressionLoc) -> PlaceRef {
        loc.place
            .unwrap_or_else(|| self.f.new_stack_place(loc.origin, loc.eval))
    }

    pub fn unwrap_or_undef(&mut self, loc: ExpressionLoc) -> PlaceRef {
        if let Some(place) = loc.place {
            return place;
        }
        let place_info = PlaceInfo {
            place: Place::Undefined,
            eval: false,
        };
        self.f.add_place(place_info)
    }

    pub fn copy(&mut self, origin: PlaceRef, target: PlaceRef) {
        self.f.copy(origin, target, self.retreat.error);
    }

    pub fn copy_if_eval(
        &mut self,
        target_eval: bool,
        loc: ExpressionLoc,
        cont: impl FnOnce(&mut Self, Option<PlaceRef>) -> SResult<PlaceRef>,
    ) -> SResult<PlaceRef> {
        if loc.eval || !target_eval {
            return cont(self, loc.place);
        }
        let inner = cont(self, None)?;
        let loc = ExpressionLoc {
            eval: target_eval,
            ..loc
        };
        let target = self.unwrap_or_stack(loc);
        self.copy(inner, target);
        Ok(target)
    }

    pub fn copy_if_deref(
        &mut self,
        loc: ExpressionLoc,
        cont: impl FnOnce(&mut Self, Option<PlaceRef>) -> SResult<PlaceRef>,
    ) -> SResult<PlaceRef> {
        self.copy_if_eval(true, loc, cont)
    }

    fn load_var(&mut self, var: DefId, loc: ExpressionLoc) -> Option<PlaceRef> {
        let place_ref = self.val_place_at_def(var)?;
        if self.f.fun.place(place_ref).eval == loc.eval && loc.place.is_none() {
            return Some(place_ref);
        }
        let new_place = self.unwrap_or_stack(loc);
        self.copy(place_ref, new_place);
        Some(new_place)
    }

    fn load_global(&mut self, pd: ParserDefId, loc: ExpressionLoc) -> SResult<PlaceRef> {
        let place_info = PlaceInfo {
            place: Place::Global(pd),
            eval: true,
        };
        let place_ref = self.f.add_place(place_info);
        if loc.place.is_none() {
            return Ok(place_ref);
        }
        let new_place = self.unwrap_or_stack(loc);
        self.copy(place_ref, new_place);
        Ok(new_place)
    }

    fn load_captured(&mut self, captured: DefId, loc: ExpressionLoc) -> SResult<PlaceRef> {
        let place_ref = self.f.add_place(PlaceInfo {
            place: Place::Captured(self.f.fun.cap(), captured),
            eval: false,
        });
        if !loc.eval && loc.place.is_none() {
            return Ok(place_ref);
        }
        let new_place = self.unwrap_or_stack(loc);
        self.copy(place_ref, new_place);
        Ok(new_place)
    }

    fn load_int(&mut self, n: i64, loc: ExpressionLoc) -> PlaceRef {
        let place_ref = self.unwrap_or_stack(loc);
        self.f.load_int(n, place_ref);
        place_ref
    }

    fn load_char(&mut self, c: u32, loc: ExpressionLoc) -> PlaceRef {
        let place_ref = self.unwrap_or_stack(loc);
        self.f.load_char(c, place_ref);
        place_ref
    }

    fn load_bool(&mut self, b: bool, loc: ExpressionLoc) -> PlaceRef {
        let place_ref = self.unwrap_or_stack(loc);
        self.f.load_bool(b, place_ref);
        place_ref
    }

    fn try_with_own_place<T, E>(
        &mut self,
        loc: ExpressionLoc,
        f: impl FnOnce(&mut Self, PlaceRef) -> Result<T, E>,
    ) -> Result<PlaceRef, E> {
        let stack_place = self.new_stack_place(loc.origin, loc.eval);
        f(self, stack_place)?;
        if let Some(place) = loc.place {
            self.copy(stack_place, place);
            Ok(place)
        } else {
            Ok(stack_place)
        }
    }

    fn with_own_place<T>(
        &mut self,
        loc: ExpressionLoc,
        f: impl FnOnce(&mut Self, PlaceRef) -> T,
    ) -> PlaceRef {
        let Ok(place) =
            self.try_with_own_place::<T, Infallible>(loc, |this, place| Ok(f(this, place)));
        place
    }

    fn load_uninit(&mut self, val: UninitVal, loc: ExpressionLoc) -> PlaceRef {
        // loading a zst requires keeping the mono layout information
        // in order to get the vtable, which may be lost if it is
        // loaded directly into a multi-layout so we make a stack place
        // which is guaranteed to be mono
        self.with_own_place(loc, |this, l| this.f.load_uninit(val, l))
    }

    fn load_string(&mut self, loc: ExpressionLoc, str: &str) -> PlaceRef {
        self.with_own_place(loc, |this, l| {
            this.f.load_bytes(str.as_bytes().to_owned(), l)
        })
    }

    fn load_undef(&mut self, loc: ExpressionLoc) -> PlaceRef {
        let has_place = loc.place.is_some();
        let place_ref = self.unwrap_or_undef(loc);
        if has_place {
            self.f.load_undef(place_ref);
        }
        place_ref
    }

    fn init_captures(
        &mut self,
        captures: &BTreeSet<DefId>,
        place_ref: PlaceRef,
    ) -> SResult<PlaceRef> {
        for captured in captures.iter() {
            let bp_ref = |ctx: &mut Self, block| {
                ctx.f.add_place(PlaceInfo {
                    place: Place::Captured(block, *captured),
                    eval: false,
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

    fn load_block(&mut self, block: BlockId, loc: ExpressionLoc) -> SResult<PlaceRef> {
        self.try_with_own_place(loc, |this, place| {
            this.f.load_uninit(UninitVal::Block(block), place);
            let captures = this.db.captures(block);
            this.init_captures(&captures, place)
        })
    }

    fn load_lambda(&mut self, lambda: LambdaId, loc: ExpressionLoc) -> SResult<PlaceRef> {
        self.try_with_own_place(loc, |this, place| {
            this.f.load_uninit(UninitVal::Lambda(lambda), place);
            let captures = this.db.lambda_captures(lambda);
            this.init_captures(&captures, place)
        })
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

    fn convert_wiggle(
        &mut self,
        loc: ExpressionLoc,
        mut inner_loc: ExpressionLoc,
        kind: WiggleKind,
        constr: HirConstraintId,
        recurse: impl FnOnce(&mut Self, Option<PlaceRef>) -> SResult<PlaceRef>,
    ) -> SResult<PlaceRef> {
        let res = if kind == WiggleKind::Is {
            let place_ref = self.unwrap_or_stack(loc);
            let inner_place = self.f.add_place(PlaceInfo {
                place: Place::Front(place_ref),
                eval: true,
            });
            inner_loc.place = Some(inner_place);
            self.copy_if_eval(loc.eval, inner_loc, recurse)?;
            place_ref
        } else {
            let place_ref = self.copy_if_eval(
                loc.eval,
                ExpressionLoc {
                    eval: inner_loc.eval,
                    ..loc
                },
                recurse,
            )?;
            let ldt_ref = self.f.new_stack_place(loc.origin, true);
            self.copy(place_ref, ldt_ref);
            let old_backtrack = self.retreat.backtrack;
            self.retreat.backtrack = match kind {
                WiggleKind::If => self.retreat.backtrack,
                WiggleKind::Expect => self.retreat.error,
                _ => unreachable!(),
            };
            let cont = self.f.new_bb();
            let constr = self.db.lookup_intern_hir_constraint(constr);
            let root = constr.expr.root();
            self.convert_constraint(&constr.expr, root, ldt_ref, cont)?;
            self.f.set_bb(cont);
            self.retreat.backtrack = old_backtrack;
            place_ref
        };
        Ok(res)
    }

    fn convert_dot(
        &mut self,
        inner_loc: ExpressionLoc,
        loc: ExpressionLoc,
        acc: BtMarkKind,
        field: FieldName,
        recurse: impl FnOnce(&mut Self, Option<PlaceRef>) -> SResult<PlaceRef>,
    ) -> SResult<PlaceRef> {
        let block_ref = self.copy_if_deref(inner_loc, recurse)?;
        let place_ref = self.unwrap_or_stack(loc);
        let backtrack = if acc.can_backtrack() {
            self.retreat.backtrack
        } else {
            self.retreat.error
        };
        self.f
            .field(block_ref, field, place_ref, self.retreat.error, backtrack);
        Ok(place_ref)
    }

    fn convert_eval_fun(
        &mut self,
        inner_loc: ExpressionLoc,
        loc: ExpressionLoc,
        recurse: impl FnOnce(&mut Self, Option<PlaceRef>) -> SResult<PlaceRef>,
        req: RequirementSet,
    ) -> SResult<PlaceRef> {
        let fun_ref = self.copy_if_deref(inner_loc, recurse)?;
        let place_ref = self.unwrap_or_stack(loc);
        self.f
            .eval_fun(fun_ref, place_ref, req & !NeededBy::Len, self.retreat);
        Ok(place_ref)
    }

    fn convert_parser_apply(
        &mut self,
        loc: ExpressionLoc,
        [lloc, rloc]: [ExpressionLoc; 2],
        lrecurse: impl FnOnce(&mut Self, Option<PlaceRef>) -> SResult<PlaceRef>,
        rrecurse: impl FnOnce(&mut Self, Option<PlaceRef>) -> SResult<PlaceRef>,
        req: RequirementSet,
    ) -> SResult<PlaceRef> {
        let left_plc = self.new_stack_place(lloc.origin, true);
        let left_plc = lrecurse(self, Some(left_plc))?;
        let right = self.copy_if_deref(rloc, rrecurse)?;
        let place_ref = self.unwrap_or_stack(loc);
        self.f.parse_call(
            CallMeta { req, tail: false },
            left_plc,
            right,
            Some(place_ref),
            None,
            self.retreat,
        );
        Ok(place_ref)
    }

    fn convert_span(&mut self, loc: ExpressionLoc, start: DefId, end: DefId) -> SResult<PlaceRef> {
        // the front typically gets modified by parser, so we have to get the back value of the
        // previous parse statement, or the block/choice front if we are at the start of a ctx
        let HirNode::Parse(ParseStatement { front, .. }) = self.db.hir_node(start)? else {
            panic!("Span does not start with parse statement")
        };
        let start_place = match front {
            ParserPredecessor::After(p) => self.back_place_at_def(p).unwrap(),
            ParserPredecessor::ChildOf(ctx) => {
                let ctx = ctx.lookup(self.db)?;
                if let Some(choice) = ctx.parent_choice {
                    self.front_place_at_def(choice.0).unwrap()
                } else {
                    self.front_place_at_def(ctx.block_id.0).unwrap()
                }
            }
        };
        let end_place = self.back_place_at_def(end).unwrap();
        let place_ref = self.unwrap_or_stack(loc);
        self.f
            .span(place_ref, start_place, end_place, self.retreat.error);
        Ok(place_ref)
    }

    fn convert_niladic_no_val(&mut self, loc: ExpressionLoc) -> SResult<PlaceRef> {
        Ok(self.load_undef(loc))
    }

    fn convert_niladic(&mut self, atom: &ResolvedAtom, loc: ExpressionLoc) -> SResult<PlaceRef> {
        Ok(match atom {
            ResolvedAtom::Val(val) => self
                .load_var(*val, loc)
                .expect("Invalid refernce to variable"),
            ResolvedAtom::Captured(cap) => self.load_captured(*cap, loc)?,
            ResolvedAtom::Number(n) => self.load_int(*n, loc),
            ResolvedAtom::Char(c) => self.load_char(*c, loc),
            ResolvedAtom::Bool(b) => self.load_bool(*b, loc),
            ResolvedAtom::ParserDef(pd) => self.load_uninit(UninitVal::ParserDef(*pd), loc),
            ResolvedAtom::Global(pd) => self.load_global(*pd, loc)?,
            ResolvedAtom::Single => self.load_uninit(UninitVal::Single, loc),
            ResolvedAtom::Array => self.load_uninit(UninitVal::Array, loc),
            ResolvedAtom::ArrayFill => self.load_uninit(UninitVal::ArrayFill, loc),
            ResolvedAtom::Span(start, end) => self.convert_span(loc, *start, *end)?,
            ResolvedAtom::String(str) => self.load_string(loc, str),
            ResolvedAtom::Regex(regex) => self.load_uninit(UninitVal::Regex(*regex), loc),
            ResolvedAtom::Block(block, _) => self.load_block(*block, loc)?,
            ResolvedAtom::Lambda(lambda) => self.load_lambda(*lambda, loc)?,
        })
    }

    fn convert_monadic_no_val(
        &mut self,
        op: &ValUnOp<HirConstraintId>,
        loc: ExpressionLoc,
        inner_loc: ExpressionLoc,
        inner_can_bt: bool,
        req: RequirementSet,
        recurse: impl FnOnce(&mut Self, Option<PlaceRef>) -> SResult<PlaceRef>,
    ) -> SResult<PlaceRef> {
        match &op {
            ValUnOp::Wiggle(_, WiggleKind::Is) => {
                recurse(self, None)?;
                Ok(self.load_undef(loc))
            }
            ValUnOp::Wiggle(constr, WiggleKind::If) => {
                self.convert_wiggle(loc, inner_loc, WiggleKind::If, *constr, recurse)
            }
            ValUnOp::Dot(field, Some(BtMarkKind::KeepBt)) => {
                self.convert_dot(inner_loc, loc, BtMarkKind::KeepBt, *field, recurse)
            }
            ValUnOp::EvalFun(eval_kind) => self.convert_eval_fun(
                inner_loc,
                loc,
                recurse,
                modify_reqs(req, inner_can_bt, *eval_kind),
            ),
            ValUnOp::Wiggle(_, WiggleKind::Expect)
            | ValUnOp::Dot(_, None | Some(BtMarkKind::RemoveBt))
            | ValUnOp::Size
            | ValUnOp::GetAddr
            | ValUnOp::Not
            | ValUnOp::Neg
            | ValUnOp::Reverse
            | ValUnOp::Popcount => {
                recurse(self, None)?;
                Ok(self.load_undef(loc))
            }
        }
    }

    fn convert_monadic(
        &mut self,
        op: &ValUnOp<HirConstraintId>,
        loc: ExpressionLoc,
        inner_loc: ExpressionLoc,
        inner_can_bt: bool,
        inner_ty: TypeId,
        req: RequirementSet,
        recurse: impl FnOnce(&mut Self, Option<PlaceRef>) -> SResult<PlaceRef>,
    ) -> SResult<PlaceRef> {
        Ok(match &op {
            ValUnOp::Not | ValUnOp::Neg | ValUnOp::Reverse | ValUnOp::Popcount => {
                let op: IntUnOp = op.try_into().unwrap();
                let inner = self.copy_if_deref(inner_loc, recurse)?;
                let place_ref = self.unwrap_or_stack(loc);
                self.f.int_un_op(place_ref, op, inner);
                place_ref
            }
            ValUnOp::Size => {
                let inner = self.copy_if_deref(inner_loc, recurse)?;
                let place_ref = self.unwrap_or_stack(loc);
                if let Type::Loop(..) = self.db.lookup_intern_type(inner_ty) {
                    self.f.array_len_call(inner, place_ref, self.retreat);
                } else {
                    self.f.len_call(inner, place_ref, self.retreat);
                }
                place_ref
            }
            ValUnOp::Wiggle(constr, kind) => {
                self.convert_wiggle(loc, inner_loc, *kind, *constr, recurse)?
            }
            ValUnOp::Dot(field, acc) => self.convert_dot(
                inner_loc,
                loc,
                acc.unwrap_or(BtMarkKind::RemoveBt),
                *field,
                recurse,
            )?,
            ValUnOp::EvalFun(eval_kind) => self.convert_eval_fun(
                inner_loc,
                loc,
                recurse,
                modify_reqs(req, inner_can_bt, *eval_kind),
            )?,
            ValUnOp::GetAddr => {
                let inner = self.copy_if_deref(inner_loc, recurse)?;
                let place_ref = self.unwrap_or_stack(loc);
                self.f.get_addr(inner, place_ref, self.retreat);
                place_ref
            }
        })
    }

    fn convert_dyadic_no_value(
        &mut self,
        op: &ValBinOp,
        loc: ExpressionLoc,
        [lloc, rloc]: [ExpressionLoc; 2],
        (lrecurse, rrecurse): (
            impl FnOnce(&mut Self, Option<PlaceRef>) -> SResult<PlaceRef>,
            impl FnOnce(&mut Self, Option<PlaceRef>) -> SResult<PlaceRef>,
        ),
        req: RequirementSet,
    ) -> SResult<PlaceRef> {
        match op {
            ValBinOp::ParserApply(Some(BtMarkKind::KeepBt)) => {
                self.convert_parser_apply(loc, [lloc, rloc], lrecurse, rrecurse, req)
            }
            ValBinOp::Else => {
                let right_bb = self.f.new_bb();
                let continue_bb = self.f.new_bb();
                let old_backtrack = self.retreat.backtrack;
                self.retreat.backtrack = right_bb;
                lrecurse(self, None)?;
                self.f.branch(continue_bb);
                self.f.set_bb(right_bb);
                self.retreat.backtrack = old_backtrack;
                rrecurse(self, None)?;
                self.f.branch(continue_bb);
                self.f.set_bb(continue_bb);
                Ok(self.load_undef(loc))
            }
            ValBinOp::Then
            | ValBinOp::ParserApply(None | Some(BtMarkKind::RemoveBt))
            | ValBinOp::Range
            | ValBinOp::And
            | ValBinOp::Xor
            | ValBinOp::Or
            | ValBinOp::LesserEq
            | ValBinOp::Lesser
            | ValBinOp::GreaterEq
            | ValBinOp::Greater
            | ValBinOp::Uneq
            | ValBinOp::Equals
            | ValBinOp::ShiftR
            | ValBinOp::ShiftL
            | ValBinOp::Minus
            | ValBinOp::Plus
            | ValBinOp::Div
            | ValBinOp::Modulo
            | ValBinOp::Mul
            | ValBinOp::ClMul => {
                lrecurse(self, None)?;
                rrecurse(self, None)?;
                Ok(self.load_undef(loc))
            }
        }
    }

    fn convert_dyadic(
        &mut self,
        op: &ValBinOp,
        loc: ExpressionLoc,
        [mut lloc, mut rloc]: [ExpressionLoc; 2],
        (lrecurse, rrecurse): (
            impl FnOnce(&mut Self, Option<PlaceRef>) -> SResult<PlaceRef>,
            impl FnOnce(&mut Self, Option<PlaceRef>) -> SResult<PlaceRef>,
        ),
        req: RequirementSet,
    ) -> SResult<PlaceRef> {
        Ok(match op {
            ValBinOp::And
            | ValBinOp::Xor
            | ValBinOp::Or
            | ValBinOp::ShiftR
            | ValBinOp::ShiftL
            | ValBinOp::Minus
            | ValBinOp::Plus
            | ValBinOp::Div
            | ValBinOp::Modulo
            | ValBinOp::Mul
            | ValBinOp::ClMul => {
                let op: IntBinOp = op.try_into().unwrap();
                let left = self.copy_if_deref(lloc, lrecurse)?;
                let right = self.copy_if_deref(rloc, rrecurse)?;
                let place_ref = self.unwrap_or_stack(loc);
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
                let left = self.copy_if_deref(lloc, lrecurse)?;
                let right = self.copy_if_deref(rloc, rrecurse)?;
                let place_ref = self.unwrap_or_stack(loc);
                self.f.comp(place_ref, op, left, right);
                place_ref
            }
            ValBinOp::ParserApply(_) => {
                self.convert_parser_apply(loc, [lloc, rloc], lrecurse, rrecurse, req)?
            }
            ValBinOp::Else => {
                let place_ref = self.unwrap_or_stack(loc);
                lloc.place = Some(place_ref);
                rloc.place = Some(place_ref);
                let right_bb = self.f.new_bb();
                let continue_bb = self.f.new_bb();
                let old_backtrack = self.retreat.backtrack;
                self.retreat.backtrack = right_bb;
                self.copy_if_eval(loc.eval, lloc, lrecurse)?;
                self.f.branch(continue_bb);
                self.f.set_bb(right_bb);
                self.retreat.backtrack = old_backtrack;
                self.copy_if_eval(loc.eval, rloc, rrecurse)?;
                self.f.branch(continue_bb);
                self.f.set_bb(continue_bb);
                place_ref
            }
            ValBinOp::Then => {
                let _ = lrecurse(self, None)?;
                let place_ref = self.unwrap_or_stack(loc);
                rrecurse(self, Some(place_ref))?
            }
            ValBinOp::Range => {
                let left = self.copy_if_deref(lloc, lrecurse)?;
                let right = self.copy_if_deref(rloc, rrecurse)?;
                let place_ref = self.unwrap_or_stack(loc);
                self.f.range(place_ref, left, right, self.retreat.error);
                place_ref
            }
        })
    }

    fn convert_variadic(
        &mut self,
        arg_count: usize,
        loc: ExpressionLoc,
        inner_locs: &[ExpressionLoc],
        mut recurse: impl FnMut(&mut Self, Option<PlaceRef>, usize) -> SResult<PlaceRef>,
    ) -> SResult<PlaceRef> {
        let fun_place = self.copy_if_deref(inner_locs[0], |ctx, plc| recurse(ctx, plc, 0))?;
        let place_ref = self.unwrap_or_stack(loc);
        // if the place is already there, it means that argument is not used
        let inner_results = (1..=arg_count)
            .map(|n| {
                Ok((
                    recurse(self, inner_locs[n].place, n)?,
                    inner_locs[n].place.is_none(),
                ))
            })
            .collect::<SResult<Vec<_>>>()?;
        if inner_results.is_empty() {
            // no arguments given, so the input type must be the same
            // as the output type (remember that evaluation is done separately,
            // so we are just applying zero arguments here)
            // we also need to do this because in the case of a function like `fun test() = 1`,
            // the `first_arg_index` below would end up negative which is not valid
            self.copy(fun_place, place_ref);
        } else {
            self.f
                .apply_args(fun_place, inner_results, place_ref, self.retreat.error);
        }
        Ok(place_ref)
    }

    fn convert_expr_impl(
        &mut self,
        expr: ExprInfo,
        idx: ExprIdx<Resolved>,
        place: Option<PlaceRef>,
    ) -> SResult<PlaceRef> {
        let idx = expr.content.data.index_expr(idx).0.0;
        let req = expr.req(idx);
        let origin = PlaceOrigin::Expr(expr.id, idx);
        let loc = ExpressionLoc {
            eval: false,
            place,
            origin,
        };
        if req.is_empty() {
            return Ok(self.load_undef(loc));
        }
        Ok(match expr.content.expr.index_expr(idx) {
            ExprHead::Niladic(n) => {
                if req.contains(NeededBy::Val) {
                    self.convert_niladic(n, loc)?
                } else {
                    self.convert_niladic_no_val(loc)?
                }
            }
            ExprHead::Monadic(op, inner) => {
                let recurse = |ctx: &mut Self, plc| ctx.convert_expr_impl(expr, *inner, plc);
                let ((inner_idx, inner_ty), inner_bt) = expr.content.data.index_expr(*inner);
                let inner_origin = PlaceOrigin::Expr(expr.id, inner_idx);
                let inner_loc = ExpressionLoc {
                    eval: false,
                    place: None,
                    origin: inner_origin,
                };
                if req.contains(NeededBy::Val) {
                    self.convert_monadic(
                        op,
                        loc,
                        inner_loc,
                        inner_bt.bt.can_backtrack(),
                        *inner_ty,
                        req,
                        recurse,
                    )?
                } else {
                    self.convert_monadic_no_val(
                        op,
                        loc,
                        inner_loc,
                        inner_bt.bt.can_backtrack(),
                        req,
                        recurse,
                    )?
                }
            }
            ExprHead::Dyadic(op, [left, right]) => {
                let lrecurse = |ctx: &mut Self, plc| ctx.convert_expr_impl(expr, *left, plc);
                let rrecurse = |ctx: &mut Self, plc| ctx.convert_expr_impl(expr, *right, plc);
                let ((left_idx, _), _) = expr.content.data.index_expr(*left);
                let ((right_idx, _), _) = expr.content.data.index_expr(*right);
                let lorigin = PlaceOrigin::Expr(expr.id, left_idx);
                let rorigin = PlaceOrigin::Expr(expr.id, right_idx);
                let lloc = ExpressionLoc {
                    eval: false,
                    place: None,
                    origin: lorigin,
                };
                let rloc = ExpressionLoc {
                    eval: false,
                    place: None,
                    origin: rorigin,
                };
                if req.contains(NeededBy::Val) {
                    self.convert_dyadic(op, loc, [lloc, rloc], (lrecurse, rrecurse), req)?
                } else {
                    self.convert_dyadic_no_value(op, loc, [lloc, rloc], (lrecurse, rrecurse), req)?
                }
            }
            ExprHead::Variadic(ValVarOp::PartialApply(_), inner) => {
                let recurse = |ctx: &mut Self, plc, n| ctx.convert_expr_impl(expr, inner[n], plc);
                let mut inner_locs = Vec::with_capacity(inner.len());
                for idx in inner.iter() {
                    let ((idx, _), _) = expr.content.data.index_expr(*idx);
                    let origin = PlaceOrigin::Expr(expr.id, idx);
                    let mut loc = ExpressionLoc {
                        eval: false,
                        place: None,
                        origin,
                    };
                    // in order to know what slot to call during llvm lowering, we have to know
                    // the layouts of all the arguments
                    // therefore, if some arguments are unused, we cannot just leave them out
                    // or leave them as undef places
                    // the subexpressions will just write undef to the place if it is present
                    // and they don't calculate the value
                    if !expr.req(idx).contains(NeededBy::Val) {
                        loc.place = Some(self.new_stack_place(loc.origin, false));
                    }
                    inner_locs.push(loc);
                }
                if req.contains(NeededBy::Val) {
                    self.convert_variadic(inner.len() - 1, loc, &inner_locs, recurse)?
                } else {
                    for i in 0..inner.len() {
                        recurse(self, None, i)?;
                    }
                    self.unwrap_or_undef(loc)
                }
            }
        })
    }

    pub fn convert_expr(
        &mut self,
        expr_id: ExprId,
        expr: &IndexTypeExpr,
        place: Option<PlaceRef>,
        reqs: &[RequirementSet],
    ) -> SResult<PlaceRef> {
        let info = ExprInfo {
            id: expr_id,
            content: expr,
            reqs,
        };
        self.convert_expr_impl(info, expr.expr.root(), place)
    }
}
