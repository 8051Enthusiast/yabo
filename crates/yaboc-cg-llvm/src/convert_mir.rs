use std::rc::Rc;

use inkwell::{
    basic_block::BasicBlock,
    values::{FunctionValue, IntValue, PointerValue},
    AddressSpace, IntPredicate,
};

use mir::{CallMeta, ControlFlow, Place, Strictness};
use yaboc_absint::AbstractDomain;
use yaboc_ast::expr::Atom;
use yaboc_ast::ConstraintAtom;
use yaboc_base::{dbpanic, interner::FieldName};
use yaboc_hir::BlockId;
use yaboc_hir_types::{NominalId, TyHirs, NOBACKTRACK_BIT, VTABLE_BIT};
use yaboc_layout::{mir_subst::FunctionSubstitute, ILayout, Layout, MonoLayout};
use yaboc_mir::{
    self as mir, BBRef, Comp, IntBinOp, IntUnOp, MirInstr, PlaceRef, ReturnStatus, Val,
};
use yaboc_types::{Type, TypeInterner};

use crate::{
    val::{CgMonoValue, CgReturnValue, CgValue},
    IResult,
};

use super::CodeGenCtx;

pub struct MirTranslator<'llvm, 'comp, 'r> {
    cg: &'r mut CodeGenCtx<'llvm, 'comp>,
    mir_fun: Rc<FunctionSubstitute<'comp>>,
    llvm_fun: FunctionValue<'llvm>,
    blocks: Vec<BasicBlock<'llvm>>,
    stack: Vec<PointerValue<'llvm>>,
    fun: CgMonoValue<'comp, 'llvm>,
    arg: CgValue<'comp, 'llvm>,
    ret: Option<CgReturnValue<'llvm>>,
    undefined: BasicBlock<'llvm>,
}

impl<'llvm, 'comp, 'r> MirTranslator<'llvm, 'comp, 'r> {
    pub fn new(
        cg: &'r mut CodeGenCtx<'llvm, 'comp>,
        mir_fun: Rc<FunctionSubstitute<'comp>>,
        llvm_fun: FunctionValue<'llvm>,
        fun: CgMonoValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
    ) -> IResult<Self> {
        cg.add_entry_block(llvm_fun);
        let mut stack = Vec::new();
        for (idx, layout) in mir_fun.stack_layouts.iter().enumerate() {
            stack.push(cg.build_layout_alloca(*layout, &format!("stack_{idx}"))?);
        }
        let mut blocks = Vec::new();
        for (bbref, _) in mir_fun.f.iter_bb() {
            let block = cg
                .llvm
                .append_basic_block(llvm_fun, &format!("mirblock_{}", bbref.as_index()));
            blocks.push(block);
        }
        let mir_entry = blocks[mir_fun.f.entry().as_index()];
        cg.builder.build_unconditional_branch(mir_entry)?;
        let undefined = cg.llvm.append_basic_block(llvm_fun, "undefined");
        cg.builder.position_at_end(undefined);
        // cg.builder.build_unreachable();
        // is easier to use with disassemblers for now
        cg.builder.build_return(Some(&cg.const_i64(4)))?;
        cg.builder.position_at_end(mir_entry);
        Ok(Self {
            cg,
            mir_fun,
            llvm_fun,
            blocks,
            stack,
            fun,
            arg,
            ret: None,
            undefined,
        })
    }

    pub fn with_ret_val(mut self, ret: CgReturnValue<'llvm>) -> Self {
        self.ret = Some(ret);
        self
    }

    fn bb(&self, bbref: BBRef) -> BasicBlock<'llvm> {
        self.blocks[bbref.as_index()]
    }

    fn is_ret_place(&self, placeref: mir::PlaceRef) -> bool {
        self.mir_fun.place_strictness(placeref) == Strictness::Return
    }

    fn place_ptr(&mut self, placeref: mir::PlaceRef) -> IResult<PointerValue<'llvm>> {
        let place = self.mir_fun.f.place(placeref).place;
        if self.is_ret_place(placeref) {
            return Ok(self
                .ret
                .expect("referenced return of non-returning function")
                .ptr);
        }
        Ok(match place {
            mir::Place::Arg | mir::Place::ReturnLen => self.arg.ptr,
            mir::Place::Captures => self.fun.ptr,
            mir::Place::Return => {
                self.ret
                    .expect("referenced return of non-returning function")
                    .ptr
            }
            mir::Place::Front(outer) => {
                let inner = self.place_ptr(outer)?;
                self.cg
                    .build_center_gep(inner, self.mir_fun.place(placeref))?
            }
            mir::Place::Stack(stack_ref) => self.stack[stack_ref.as_index()],
            mir::Place::Field(outer, a) | mir::Place::Captured(outer, a) => {
                let outer = self.place_val(outer)?;
                let inner_layout = self.mir_fun.place(placeref);
                self.cg.build_field_gep(a, outer, inner_layout)?.ptr
            }
            mir::Place::ModifiedBy(ins_ref) => {
                let MirInstr::ParseCall(_, _, _, front, _, _) = self.mir_fun.f.ins_at(ins_ref)
                else {
                    unreachable!()
                };
                self.place_ptr(front)?
            }
            mir::Place::Global(pd) => self.cg.global_constant(pd),
            mir::Place::Undefined => self.cg.invalid_ptr(),
        })
    }

    fn place_val(&mut self, placeref: PlaceRef) -> IResult<CgValue<'comp, 'llvm>> {
        let ptr = self.place_ptr(placeref)?;
        let layout = self.mir_fun.place(placeref);
        Ok(CgValue::new(layout, ptr))
    }

    fn deref_level(&mut self, place: PlaceRef) -> IntValue<'llvm> {
        let remove_bt = self.mir_fun.f.place(place).remove_bt;
        let place_layout = self.mir_fun.place(place);
        if self.mir_fun.f.place(place).place == mir::Place::Return {
            return self.ret.unwrap().head;
        }
        let place_strictness = self.mir_fun.place_strictness(place);
        let deref_level = match place_strictness {
            Strictness::Return => {
                return self.ret.unwrap().head;
            }
            Strictness::Static(level) => level,
        };
        let mut level = deref_level.into_shifted_runtime_value();
        level |= (place_layout.is_multi() as u64) << VTABLE_BIT;
        level |= (remove_bt as u64) << NOBACKTRACK_BIT;
        self.cg.const_i64(level as i64)
    }

    fn return_val(&mut self, place: PlaceRef) -> IResult<CgReturnValue<'llvm>> {
        let ptr = self.place_ptr(place)?;
        let head = self.deref_level(place);
        Ok(CgReturnValue::new(head, ptr))
    }

    fn controlflow_case(&mut self, ret: IntValue<'llvm>, ctrl: ControlFlow) -> IResult<()> {
        let mut cases = [self.undefined; 4];
        cases[0] = self.bb(ctrl.next);
        if let Some(error) = ctrl.error {
            cases[1] = self.bb(error);
        }
        if let Some(eof) = ctrl.eof {
            cases[2] = self.bb(eof);
        }
        if let Some(backtrack) = ctrl.backtrack {
            cases[3] = self.bb(backtrack);
        }
        self.cg.builder.build_switch(
            ret,
            self.undefined,
            &[
                (self.cg.const_i64(ReturnStatus::Ok as i64), cases[0]),
                (self.cg.const_i64(ReturnStatus::Error as i64), cases[1]),
                (self.cg.const_i64(ReturnStatus::Eof as i64), cases[2]),
                (self.cg.const_i64(ReturnStatus::Backtrack as i64), cases[3]),
            ],
        )?;
        Ok(())
    }

    fn copy(&mut self, to: PlaceRef, from: PlaceRef, ctrl: ControlFlow) -> IResult<()> {
        if self.is_ret_place(to) && self.is_ret_place(from) {
            let block = self.bb(ctrl.next);
            self.cg.builder.build_unconditional_branch(block)?;
            return Ok(());
        }
        let to = self.return_val(to)?;
        let from = self.place_val(from)?;
        if let Layout::None = from.layout.layout.1 {
            self.cg.builder.build_unreachable()?;
            return Ok(());
        }
        let ret = self.cg.call_typecast_fun(to, from)?;
        self.controlflow_case(ret, ctrl)
    }

    fn eval_fun(&mut self, to: PlaceRef, from: PlaceRef, ctrl: ControlFlow) -> IResult<()> {
        let to = self.return_val(to)?;
        let from = self.place_val(from)?;
        if let Layout::None = from.layout.layout.1 {
            self.cg.builder.build_unreachable()?;
            return Ok(());
        }
        let ret = self.cg.call_eval_fun_fun_wrapper(to, from)?;
        self.controlflow_case(ret, ctrl)
    }

    fn unwrap_block_id(&mut self, layout: ILayout<'comp>) -> BlockId {
        match layout
            .maybe_mono()
            .expect("set_discrimant can only be called on mono layouts")
            .mono_layout()
            .0
        {
            MonoLayout::Block(id, _) => *id,
            _ => panic!("set_discriminant can only be called on block layouts"),
        }
    }

    fn discriminant_info(
        &mut self,
        block: PlaceRef,
        field: FieldName,
    ) -> IResult<Option<(PointerValue<'llvm>, IntValue<'llvm>)>> {
        let block = self.place_val(block)?;
        let block_id = self.unwrap_block_id(block.layout);
        self.cg.build_discriminant_info(block_id, block, field)
    }

    fn set_discriminant(&mut self, block: PlaceRef, field: FieldName, val: bool) -> IResult<()> {
        let (byte_ptr, shifted_bit) = self
            .discriminant_info(block, field)?
            .expect("fields that are always present can not be set");
        let byte = self.cg.build_byte_load(byte_ptr, "lddisc")?;
        let modified_byte = if val {
            self.cg.builder.build_or(byte, shifted_bit, "setdisc")?
        } else {
            self.cg
                .builder
                .build_and(byte, shifted_bit.const_not(), "clrdisc")?
        };
        self.cg.builder.build_store(byte_ptr, modified_byte)?;
        Ok(())
    }

    fn assert_value(
        &mut self,
        place: PlaceRef,
        val: ConstraintAtom,
        ctrl: ControlFlow,
    ) -> IResult<()> {
        let cond = match val {
            ConstraintAtom::Atom(Atom::Field(field)) => {
                let (byte_ptr, shifted_bit) = match self.discriminant_info(place, field)? {
                    Some(x) => x,
                    None => return Ok(()),
                };
                self.cg.build_discriminant_check(byte_ptr, shifted_bit)?
            }
            ConstraintAtom::Atom(Atom::Number(num)) => {
                let num = self.cg.const_i64(num);
                let num_ptr = self.place_ptr(place)?;
                let num_actual = self.cg.build_i64_load(num_ptr, "ld_assert_num")?;
                self.cg.builder.build_int_compare(
                    IntPredicate::EQ,
                    num,
                    num_actual,
                    "cmp_assert_num",
                )?
            }
            ConstraintAtom::Atom(Atom::Char(num)) => {
                let num = self.cg.llvm.i32_type().const_int(num as u64, false);
                let num_ptr = self.place_ptr(place)?;
                let num_actual = self.cg.build_char_load(num_ptr, "ld_assert_char")?;
                self.cg.builder.build_int_compare(
                    IntPredicate::EQ,
                    num,
                    num_actual,
                    "cmp_assert_char",
                )?
            }
            ConstraintAtom::Atom(Atom::Bool(bool)) => {
                let bool = self.cg.llvm.i8_type().const_int(bool as u64, false);
                let bool_ptr = self.place_ptr(place)?;
                let num_actual = self.cg.build_byte_load(bool_ptr, "ld_assert_bool")?;
                self.cg.builder.build_int_compare(
                    IntPredicate::EQ,
                    bool,
                    num_actual,
                    "cmp_assert_bool",
                )?
            }
            ConstraintAtom::Range(start, end) => {
                let start = self.cg.const_i64(start);
                let end = self.cg.const_i64(end);
                let num_ptr = self.place_ptr(place)?;
                let num_actual = self.cg.build_i64_load(num_ptr, "ld_assert_range")?;
                let cmp_start = self.cg.builder.build_int_compare(
                    IntPredicate::SGE,
                    num_actual,
                    start,
                    "cmp_assert_range",
                )?;
                let cmp_end = self.cg.builder.build_int_compare(
                    IntPredicate::SLE,
                    num_actual,
                    end,
                    "cmp_assert_range",
                )?;
                self.cg
                    .builder
                    .build_and(cmp_start, cmp_end, "cmp_assert_range")?
            }
            ConstraintAtom::NotEof => {
                // always true since it is checked for the parser and not the returned value
                self.cg.llvm.bool_type().const_int(1, false)
            }
        };
        let next_block = self.bb(ctrl.next);
        let fallback_block = ctrl
            .backtrack
            .map(|bb| self.bb(bb))
            .unwrap_or(self.undefined);
        self.cg
            .builder
            .build_conditional_branch(cond, next_block, fallback_block)?;
        Ok(())
    }

    fn field(
        &mut self,
        ret: PlaceRef,
        place: PlaceRef,
        field: FieldName,
        ctrl: ControlFlow,
    ) -> IResult<()> {
        let field = match field {
            FieldName::Return => {
                return self.copy(place, ret, ctrl);
            }
            FieldName::Ident(ident) => ident,
        };
        let ty = self.mir_fun.f.place(place).ty;
        let ldt_ty = self.cg.compiler_database.db.least_deref_type(ty).unwrap();
        let block = match self.cg.compiler_database.db.lookup_intern_type(ldt_ty) {
            Type::Nominal(nom) => NominalId::from_nominal_head(&nom).unwrap_block(),
            _ => panic!("field called on non-block"),
        };
        let place_val = self.place_val(place)?;
        let ret_val = self.return_val(ret)?;
        let ret = self
            .cg
            .call_field_access_fun(ret_val, place_val, block, field)?;
        self.controlflow_case(ret, ctrl)
    }

    fn len_call(&mut self, ret: PlaceRef, fun: PlaceRef, ctrl: ControlFlow) -> IResult<()> {
        let fun_val = self.place_val(fun)?;
        let ret_val = self.return_val(ret)?.ptr;
        let ret = self.cg.call_len_fun(ret_val, fun_val)?;
        self.controlflow_case(ret, ctrl)
    }

    fn parse_call(
        &mut self,
        ret: Option<PlaceRef>,
        call_kind: CallMeta,
        fun: PlaceRef,
        arg: PlaceRef,
        ctrl: Option<ControlFlow>,
    ) -> IResult<()> {
        let fun_val = self.place_val(fun)?;
        let arg_val = self.place_val(arg)?;
        let ret_val = ret
            .map(|r| self.return_val(r))
            .unwrap_or_else(|| Ok(self.cg.undef_ret()))?;
        if let Some(ctrl) = ctrl {
            let ret = self
                .cg
                .call_parser_fun_wrapper(ret_val, fun_val, arg_val, call_kind.req)?;
            self.controlflow_case(ret, ctrl)?;
        } else {
            let parent_fun = if self.mir_fun.f.place(fun).place == Place::Captures {
                // don't copy in case it is already the arg since that would
                // result in issues with memcpy
                None
            } else {
                Some(self.fun)
            };
            let ret = self.cg.call_parser_fun_tail(
                ret_val,
                fun_val,
                arg_val,
                call_kind.req,
                parent_fun,
            )?;
            self.cg.builder.build_return(Some(&ret))?;
        }
        Ok(())
    }

    fn store_val(&mut self, ret: PlaceRef, val: Val) -> IResult<()> {
        let ret_ptr = self.place_ptr(ret)?;
        let llvm_val = match val {
            Val::Char(c) => self.cg.llvm.i32_type().const_int(c as u64, false),
            Val::Int(i) => self.cg.const_i64(i),
            Val::Bool(b) => self.cg.llvm.i8_type().const_int(b as u64, false),
            Val::Parser(_) | Val::Undefined => return Ok(()),
        };
        let casted_ret_ptr = self
            .cg
            .builder
            .build_bit_cast(
                ret_ptr,
                self.cg.llvm.ptr_type(AddressSpace::default()),
                "cast_store_val",
            )?
            .into_pointer_value();
        self.cg.builder.build_store(casted_ret_ptr, llvm_val)?;
        Ok(())
    }

    fn build_typed_place_ptr(&mut self, place: PlaceRef) -> IResult<PointerValue<'llvm>> {
        let ptr = self.place_ptr(place)?;
        Ok(self
            .cg
            .builder
            .build_bit_cast(ptr, self.cg.llvm.ptr_type(AddressSpace::default()), "int")?
            .into_pointer_value())
    }

    fn build_int_load(&mut self, place: PlaceRef) -> IResult<IntValue<'llvm>> {
        let ptr = self.place_ptr(place)?;
        self.cg.build_i64_load(ptr, "load_int")
    }

    fn comp(&mut self, ret: PlaceRef, op: Comp, left: PlaceRef, right: PlaceRef) -> IResult<()> {
        let lhs = self.build_int_load(left)?;
        let rhs = self.build_int_load(right)?;
        let op = match op {
            Comp::LesserEq => IntPredicate::SLE,
            Comp::Lesser => IntPredicate::SLT,
            Comp::GreaterEq => IntPredicate::SGE,
            Comp::Greater => IntPredicate::SGT,
            Comp::Uneq => IntPredicate::NE,
            Comp::Equals => IntPredicate::EQ,
        };
        let comp_res = self.cg.builder.build_int_compare(op, lhs, rhs, "comp")?;
        let zext_res = self.cg.builder.build_int_z_extend(
            comp_res,
            self.cg.llvm.i8_type(),
            "zext_bool_to_u8",
        )?;
        let ret_ptr = self.build_typed_place_ptr(ret)?;
        self.cg.builder.build_store(ret_ptr, zext_res)?;
        Ok(())
    }

    fn set_arg(
        &mut self,
        fun: PlaceRef,
        arg: PlaceRef,
        argnum: u64,
        error: BasicBlock<'llvm>,
    ) -> IResult<()> {
        let fun_val = self.place_val(fun)?;
        let arg_val = self.place_val(arg)?;
        let ret = self.cg.build_arg_set(fun_val, arg_val, argnum)?;
        let ret_is_err = self.cg.builder.build_int_compare(
            IntPredicate::EQ,
            ret,
            self.cg.const_i64(ReturnStatus::Error as i64),
            "set_arg_is_err",
        )?;
        let next_bb = self
            .cg
            .llvm
            .append_basic_block(self.llvm_fun, "set_arg_next");
        self.cg
            .builder
            .build_conditional_branch(ret_is_err, error, next_bb)?;
        self.cg.builder.position_at_end(next_bb);
        Ok(())
    }

    fn apply_args(
        &mut self,
        ret: PlaceRef,
        fun: PlaceRef,
        args: &[(PlaceRef, bool)],
        first_index: u64,
        ctrl: ControlFlow,
    ) -> IResult<()> {
        let error = ctrl.error.map(|x| self.bb(x)).unwrap_or(self.undefined);
        let ty = self.mir_fun.place_type(fun);
        let Type::FunctionArg(_, arg_tys) = self.cg.compiler_database.db.lookup_intern_type(ty)
        else {
            panic!("apply_args on non-function");
        };
        let arg_layout = args
            .iter()
            .zip(arg_tys.iter())
            .map(|((x, _), ty)| {
                self.mir_fun
                    .place(*x)
                    .typecast(self.cg.layouts, *ty)
                    .map(|x| x.0)
            })
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        let any_ty = self.cg.compiler_database.db.intern_type(Type::Any);
        let arg_layout_tuple = self
            .cg
            .layouts
            .dcx
            .intern(Layout::Mono(MonoLayout::Tuple(arg_layout), any_ty));
        let fun = self.place_val(fun)?;
        let Some(slot) = self
            .cg
            .collected_layouts
            .funcall_slots
            .layout_vtable_offsets
            .get(&(arg_layout_tuple, fun.layout))
            .copied()
        else {
            dbpanic!(
                &self.cg.compiler_database.db,
                "cannot find funcall slot for {} with args {}",
                &fun.layout,
                &arg_layout_tuple
            );
        };
        let ret_val = self.return_val(ret)?;
        self.cg.call_fun_create(ret_val, fun, slot)?;
        for (i, (arg, used)) in args.iter().enumerate() {
            if *used {
                self.set_arg(ret, *arg, first_index - i as u64, error)?;
            }
        }
        self.cg
            .builder
            .build_unconditional_branch(self.bb(ctrl.next))?;
        Ok(())
    }

    fn int_un(&mut self, ret: PlaceRef, op: IntUnOp, right: PlaceRef) -> IResult<()> {
        let rhs = self.build_int_load(right)?;
        let value = match op {
            IntUnOp::Not => self.cg.builder.build_not(rhs, "not")?,
            IntUnOp::Neg => self.cg.builder.build_int_neg(rhs, "neg")?,
        };
        let ret_ptr = self.build_typed_place_ptr(ret)?;
        self.cg.builder.build_store(ret_ptr, value)?;
        Ok(())
    }

    fn int_bin(
        &mut self,
        ret: PlaceRef,
        op: IntBinOp,
        left: PlaceRef,
        right: PlaceRef,
    ) -> IResult<()> {
        let lhs = self.build_int_load(left)?;
        let rhs = self.build_int_load(right)?;
        let b = &self.cg.builder;
        let value = match op {
            IntBinOp::And => b.build_and(lhs, rhs, "and"),
            IntBinOp::Xor => b.build_xor(lhs, rhs, "xor"),
            IntBinOp::Or => b.build_or(lhs, rhs, "or"),
            IntBinOp::ShiftR => b.build_right_shift(lhs, rhs, true, "shr"),
            IntBinOp::ShiftL => b.build_left_shift(lhs, rhs, "shl"),
            IntBinOp::Minus => b.build_int_sub(lhs, rhs, "minus"),
            IntBinOp::Plus => b.build_int_add(lhs, rhs, "plus"),
            IntBinOp::Div => b.build_int_signed_div(lhs, rhs, "div"),
            IntBinOp::Modulo => b.build_int_signed_rem(lhs, rhs, "mod"),
            IntBinOp::Mul => b.build_int_mul(lhs, rhs, "mul"),
        }?;
        let ret_ptr = self.build_typed_place_ptr(ret)?;
        self.cg.builder.build_store(ret_ptr, value)?;
        Ok(())
    }

    fn get_addr(&mut self, ret: PlaceRef, addr: PlaceRef, ctrl: ControlFlow) -> IResult<()> {
        let ret_val = self.place_val(ret)?;
        let global_array = self.cg.yabo_global_address();
        self.cg.build_copy_invariant(ret_val, global_array.into())?;
        let addr_ptr = self.place_ptr(addr)?;
        let addr_val = self.cg.build_i64_load(addr_ptr, "get_addr")?;
        let len = self.cg.call_array_len_fun(ret_val)?;
        // this is unsigned compare because negative numbers also need to error
        let cmp =
            self.cg
                .builder
                .build_int_compare(IntPredicate::ULT, addr_val, len, "get_addr_cmp")?;
        let next = self
            .cg
            .llvm
            .append_basic_block(self.llvm_fun, "get_addr_next");
        let err = self.bb(ctrl.error.unwrap());
        self.cg.builder.build_conditional_branch(cmp, next, err)?;
        self.cg.builder.position_at_end(next);
        let status = self.cg.call_skip_fun(ret_val, addr_val)?;
        self.controlflow_case(status, ctrl)
    }

    fn span(
        &mut self,
        ret: PlaceRef,
        start: PlaceRef,
        end: PlaceRef,
        ctrl: ControlFlow,
    ) -> IResult<()> {
        let ret_val = self.return_val(ret)?;
        let start_val = self.place_val(start)?;
        let end_val = self.place_val(end)?;
        let ret = self.cg.call_span_fun(ret_val, start_val, end_val)?;
        self.controlflow_case(ret, ctrl)
    }

    fn mir_ins(&mut self, ins: MirInstr) -> IResult<()> {
        match ins {
            MirInstr::IntBin(ret, op, left, right) => self.int_bin(ret, op, left, right),
            MirInstr::IntUn(ret, op, right) => self.int_un(ret, op, right),
            MirInstr::Comp(ret, op, left, right) => self.comp(ret, op, left, right),
            MirInstr::StoreVal(ret, val) => self.store_val(ret, val),
            MirInstr::ParseCall(ret, _, call_kind, arg, fun, ctrl) => {
                self.parse_call(ret, call_kind, fun, arg, ctrl)
            }
            MirInstr::LenCall(ret, fun, ctrl) => self.len_call(ret, fun, ctrl),
            MirInstr::Field(ret, place, field, ctrl) => self.field(ret, place, field, ctrl),
            MirInstr::AssertVal(place, val, ctrl) => self.assert_value(place, val, ctrl),
            MirInstr::SetDiscriminant(block, field, val) => {
                self.set_discriminant(block, field, val)
            }
            MirInstr::EvalFun(to, from, ctrl) => self.eval_fun(to, from, ctrl),
            MirInstr::Copy(to, from, ctrl) => self.copy(to, from, ctrl),
            MirInstr::GetAddr(ret, place, ctrl) => self.get_addr(ret, place, ctrl),
            MirInstr::Span(ret, start, end, ctrl) => self.span(ret, start, end, ctrl),
            MirInstr::ApplyArgs(ret, fun, args, first_index, ctrl) => {
                self.apply_args(ret, fun, &args, first_index, ctrl)
            }
            MirInstr::Branch(target) => {
                let target_block = self.bb(target);
                self.cg.builder.build_unconditional_branch(target_block)?;
                Ok(())
            }
            MirInstr::Return(status) => {
                let llvm_int = self.cg.llvm.i64_type().const_int(status as u64, false);
                self.cg.builder.build_return(Some(&llvm_int))?;
                Ok(())
            }
        }
    }

    fn build_bb(&mut self, bb: &mir::BasicBlock) -> IResult<()> {
        for ins in bb.ins() {
            self.mir_ins(ins)?
        }
        Ok(())
    }

    pub fn build(mut self) -> IResult<FunctionValue<'llvm>> {
        let mir_fun = self.mir_fun.clone();
        for (bbref, bb) in mir_fun.f.iter_bb() {
            let block = self.bb(bbref);
            self.cg.builder.position_at_end(block);
            self.build_bb(bb)?;
        }
        Ok(self.llvm_fun)
    }
}
