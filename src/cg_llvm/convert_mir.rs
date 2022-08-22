use std::rc::Rc;

use inkwell::{
    basic_block::BasicBlock,
    types::IntType,
    values::{BasicMetadataValueEnum, CallableValue, FunctionValue, IntValue, PointerValue},
    AddressSpace, IntPredicate,
};

use crate::{
    expr::Atom,
    hir::BlockId,
    hir_types::{NominalId, TyHirs},
    interner::FieldName,
    layout::{mir_subst::FunctionSubstitute, ILayout, Layout, MonoLayout},
    mir::{
        self, BBRef, CallKind, Comp, ExceptionRetreat, IntBinOp, IntUnOp, MirInstr, PlaceRef,
        ReturnStatus, Val,
    },
    types::{Type, TypeInterner},
};

use super::CodeGenCtx;

pub struct MirTranslator<'llvm, 'comp, 'r> {
    cg: &'r mut CodeGenCtx<'llvm, 'comp>,
    mir_fun: Rc<FunctionSubstitute<'comp>>,
    llvm_fun: FunctionValue<'llvm>,
    blocks: Vec<BasicBlock<'llvm>>,
    stack: Vec<PointerValue<'llvm>>,
    fun: PointerValue<'llvm>,
    arg: PointerValue<'llvm>,
    ret: PointerValue<'llvm>,
    undefined: BasicBlock<'llvm>,
}

impl<'llvm, 'comp, 'r> MirTranslator<'llvm, 'comp, 'r> {
    pub fn new(
        cg: &'r mut CodeGenCtx<'llvm, 'comp>,
        mir_fun: Rc<FunctionSubstitute<'comp>>,
        llvm_fun: FunctionValue<'llvm>,
        fun: PointerValue<'llvm>,
        arg: PointerValue<'llvm>,
        ret: PointerValue<'llvm>,
    ) -> Self {
        let entry = cg.llvm.append_basic_block(llvm_fun, "entry");
        cg.builder.position_at_end(entry);
        let mut stack = Vec::new();
        for layout in mir_fun.stack_layouts.iter() {
            stack.push(cg.build_layout_alloca(layout));
        }
        let mut blocks = Vec::new();
        for (bbref, _) in mir_fun.f.iter_bb() {
            let block = cg
                .llvm
                .append_basic_block(llvm_fun, &format!("mirblock_{}", bbref.as_index()));
            blocks.push(block);
        }
        let mir_entry = blocks[mir_fun.f.entry().as_index()];
        cg.builder.build_unconditional_branch(mir_entry);
        let undefined = cg.llvm.append_basic_block(llvm_fun, "undefined");
        cg.builder.position_at_end(undefined);
        // cg.builder.build_unreachable();
        // is easier to use with disassemblers for now
        cg.builder.build_return(Some(&cg.const_i64(4)));
        cg.builder.position_at_end(mir_entry);
        Self {
            cg,
            mir_fun,
            llvm_fun,
            blocks,
            stack,
            fun,
            arg,
            ret,
            undefined,
        }
    }

    fn bb(&self, bbref: BBRef) -> BasicBlock<'llvm> {
        self.blocks[bbref.as_index()]
    }

    fn place_ptr(&mut self, place: mir::PlaceRef) -> PointerValue<'llvm> {
        let place = self.mir_fun.f.place(place).place;
        match place {
            mir::Place::Arg => self.arg,
            mir::Place::Return => self.ret,
            mir::Place::Captures => self.fun,
            mir::Place::From(outer) => self.place_ptr(outer),
            mir::Place::Stack(stack_ref) => self.stack[stack_ref.as_index()],
            mir::Place::Field(outer, a) => {
                let outer_layout = self.mir_fun.place(outer);
                let outer_ptr = self.place_ptr(outer);
                self.cg.build_field_gep(outer_layout, a, outer_ptr)
            }
            mir::Place::DupleField(outer, field) => {
                let outer_layout = self.mir_fun.place(outer);
                let outer_ptr = self.place_ptr(outer);
                self.cg.build_duple_gep(outer_layout, field, outer_ptr)
            }
        }
    }

    fn head_disc(&mut self, place: PlaceRef) -> IntValue<'llvm> {
        let place_ty = self.mir_fun.f.place(place).ty;
        let place_layout = self.mir_fun.place(place);
        let mut disc = self.cg.compiler_database.db.head_discriminant(place_ty);
        if let Layout::Multi(_) = place_layout.layout {
            disc |= 1;
        }
        self.cg.llvm.i64_type().const_int(disc as u64, false)
    }

    fn fallible_call(
        &mut self,
        fun: CallableValue<'llvm>,
        args: &[BasicMetadataValueEnum<'llvm>],
        retreat: [BasicBlock<'llvm>; 3],
    ) {
        let ret = self.cg.build_call_with_int_ret(fun, args);
        let next_block = self.cg.llvm.append_basic_block(self.llvm_fun, "");
        self.cg.builder.build_switch(
            ret,
            self.undefined,
            &[
                (self.cg.const_i64(ReturnStatus::Ok as i64), next_block),
                (
                    self.cg.const_i64(ReturnStatus::Backtrack as i64),
                    retreat[0],
                ),
                (self.cg.const_i64(ReturnStatus::Eof as i64), retreat[1]),
                (self.cg.const_i64(ReturnStatus::Error as i64), retreat[2]),
            ],
        );
        self.cg.builder.position_at_end(next_block);
    }

    fn copy(&mut self, to: PlaceRef, from: PlaceRef, error: BBRef) {
        let from_layout = self.mir_fun.place(from);
        let to_disc = self.head_disc(to);
        if let Layout::None = from_layout.layout {
            self.cg.builder.build_unreachable();
            return;
        }
        let maybe_mono = from_layout.maybe_mono();
        let from_ptr = self.place_ptr(from);
        let fun = self.cg.build_typecast_fun_get(maybe_mono, from_ptr);
        let to_ptr = self.place_ptr(to);
        self.fallible_call(
            fun,
            &[from_ptr.into(), to_disc.into(), to_ptr.into()],
            [self.undefined, self.undefined, self.bb(error)],
        )
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
    ) -> Option<(PointerValue<'llvm>, IntValue<'llvm>)> {
        let layout = self.mir_fun.place(block);
        let block_id = self.unwrap_block_id(layout);
        let block_ptr = self.place_ptr(block);
        self.cg
            .build_discriminant_info(block_id, block_ptr, layout, field)
    }

    fn set_discriminant(&mut self, block: PlaceRef, field: FieldName, val: bool) {
        let (byte_ptr, shifted_bit) = self
            .discriminant_info(block, field)
            .expect("fields that are always present can not be set");
        let byte = self
            .cg
            .builder
            .build_load(byte_ptr, "lddisc")
            .into_int_value();
        let modified_byte = if val {
            self.cg.builder.build_or(byte, shifted_bit, "setdisc")
        } else {
            self.cg
                .builder
                .build_and(byte, shifted_bit.const_not(), "clrdisc")
        };
        self.cg.builder.build_store(byte_ptr, modified_byte);
    }

    fn assert_value(&mut self, place: PlaceRef, val: Atom, fallback: BBRef) {
        let cond = match val {
            Atom::Field(field) => {
                let (byte_ptr, shifted_bit) = match self.discriminant_info(place, field) {
                    Some(x) => x,
                    None => return,
                };
                self.cg.build_discriminant_check(byte_ptr, shifted_bit)
            }
            Atom::Number(num) => {
                let num = self.cg.const_i64(num);
                let num_ptr = self.place_ptr(place);
                let cast_num_ptr = self
                    .cg
                    .builder
                    .build_bitcast(
                        num_ptr,
                        self.cg.llvm.i64_type().ptr_type(AddressSpace::Generic),
                        "cast_assert_num",
                    )
                    .into_pointer_value();
                let num_actual = self
                    .cg
                    .builder
                    .build_load(cast_num_ptr, "ld_assert_num")
                    .into_int_value();
                self.cg.builder.build_int_compare(
                    IntPredicate::EQ,
                    num,
                    num_actual,
                    "cmp_assert_num",
                )
            }
            Atom::Char(num) => {
                let num = self.cg.llvm.i32_type().const_int(num as u64, false);
                let num_ptr = self.place_ptr(place);
                let cast_num_ptr = self
                    .cg
                    .builder
                    .build_bitcast(
                        num_ptr,
                        self.cg.llvm.i32_type().ptr_type(AddressSpace::Generic),
                        "cast_assert_char",
                    )
                    .into_pointer_value();
                let num_actual = self
                    .cg
                    .builder
                    .build_load(cast_num_ptr, "ld_assert_char")
                    .into_int_value();
                self.cg.builder.build_int_compare(
                    IntPredicate::EQ,
                    num,
                    num_actual,
                    "cmp_assert_char",
                )
            }
        };
        let next_block = self.cg.llvm.append_basic_block(self.llvm_fun, "");
        let fallback_block = self.bb(fallback);
        self.cg
            .builder
            .build_conditional_branch(cond, next_block, fallback_block);
        self.cg.builder.position_at_end(next_block);
    }

    fn field(&mut self, ret: PlaceRef, place: PlaceRef, field: FieldName, error: BBRef) {
        let place_ptr = self.place_ptr(place);
        let layout = self.mir_fun.place(place);
        let shifted_place_ptr = self.cg.build_mono_ptr(place_ptr, layout);
        let field = match field {
            FieldName::Return => {
                return self.copy(place, ret, error);
            }
            FieldName::Ident(ident) => ident,
        };
        let ty = self.mir_fun.f.place(place).ty;
        let block = match self.cg.compiler_database.db.lookup_intern_type(ty) {
            Type::Nominal(nom) => {
                let id = NominalId::from_nominal_head(&nom);
                match id {
                    NominalId::Def(_) => panic!("field called on non-block"),
                    NominalId::Block(f) => f,
                }
            }
            _ => panic!("field called on non-block"),
        };
        let place_ptr = self.place_ptr(place);
        let fun = self
            .cg
            .build_field_access_fun_get(block, layout.maybe_mono(), place_ptr, field);
        let target_head = self.head_disc(ret);
        let ret = self.place_ptr(ret);
        self.fallible_call(
            fun,
            &[ret.into(), target_head.into(), shifted_place_ptr.into()],
            [self.undefined, self.undefined, self.bb(error)],
        )
    }

    fn call(
        &mut self,
        ret: PlaceRef,
        call_kind: CallKind,
        fun: PlaceRef,
        arg: PlaceRef,
        retreat: ExceptionRetreat,
    ) {
        let fun_layout = self.mir_fun.place(fun);
        let fun_ptr = self.place_ptr(fun);
        let mono_fun_ptr = self.cg.build_mono_ptr(fun_ptr, fun_layout);
        let arg_layout = self.mir_fun.place(arg);
        let slot = match self
            .cg
            .collected_layouts
            .call_slots
            .get(&(arg_layout, fun_layout))
        {
            Some(slot) => *slot,
            None => {
                // TODO(8051): should be turned into an llvm unreachable in the future,
                // but for now this is a panic to catch more errors
                panic!("call slot not available")
            }
        };
        let call_ptr =
            self.cg
                .build_parser_fun_get(fun_layout.maybe_mono(), fun_ptr, slot, call_kind);
        let retreat = [retreat.backtrack, retreat.eof, retreat.error].map(|x| self.bb(x));
        let ret_ptr = self.place_ptr(ret);
        let arg_ptr = self.place_ptr(arg);
        match call_kind {
            CallKind::Len => self.fallible_call(
                call_ptr,
                &[mono_fun_ptr.into(), arg_ptr.into(), ret_ptr.into()],
                retreat,
            ),
            CallKind::Val => {
                let head_disc = self.head_disc(ret);
                self.fallible_call(
                    call_ptr,
                    &[
                        mono_fun_ptr.into(),
                        arg_ptr.into(),
                        head_disc.into(),
                        ret_ptr.into(),
                    ],
                    retreat,
                )
            }
        };
    }

    fn store_val(&mut self, ret: PlaceRef, val: Val) {
        let ret_ptr = self.place_ptr(ret);
        let llvm_val = match val {
            Val::Char(c) => self.cg.llvm.i32_type().const_int(c as u64, false),
            Val::Int(i) => self.cg.const_i64(i),
        };
        let casted_ret_ptr = self
            .cg
            .builder
            .build_bitcast(
                ret_ptr,
                llvm_val.get_type().ptr_type(AddressSpace::Generic),
                "cast_store_val",
            )
            .into_pointer_value();
        self.cg.builder.build_store(casted_ret_ptr, llvm_val);
    }

    fn build_typed_place_ptr(
        &mut self,
        place: PlaceRef,
        ty: IntType<'llvm>,
    ) -> PointerValue<'llvm> {
        let ptr = self.place_ptr(place);
        self.cg
            .builder
            .build_bitcast(ptr, ty.ptr_type(AddressSpace::Generic), "int")
            .into_pointer_value()
    }

    fn build_int_load(&mut self, place: PlaceRef) -> IntValue<'llvm> {
        let ptr = self.build_typed_place_ptr(place, self.cg.llvm.i64_type());
        self.cg.builder.build_load(ptr, "load_int").into_int_value()
    }

    fn comp(&mut self, ret: PlaceRef, op: Comp, left: PlaceRef, right: PlaceRef) {
        let lhs = self.build_int_load(left);
        let rhs = self.build_int_load(right);
        let op = match op {
            Comp::LesserEq => IntPredicate::SLE,
            Comp::Lesser => IntPredicate::SLT,
            Comp::GreaterEq => IntPredicate::SGE,
            Comp::Greater => IntPredicate::SGT,
            Comp::Uneq => IntPredicate::NE,
            Comp::Equals => IntPredicate::EQ,
        };
        let comp_res = self.cg.builder.build_int_compare(op, lhs, rhs, "comp");
        let ret_ptr = self.build_typed_place_ptr(ret, self.cg.llvm.bool_type());
        self.cg.builder.build_store(ret_ptr, comp_res);
    }

    fn int_un(&mut self, ret: PlaceRef, op: IntUnOp, right: PlaceRef) {
        let rhs = self.build_int_load(right);
        let value = match op {
            IntUnOp::Not => self.cg.builder.build_not(rhs, "not"),
            IntUnOp::Neg => self.cg.builder.build_int_neg(rhs, "neg"),
        };
        let ret_ptr = self.build_typed_place_ptr(ret, self.cg.llvm.i64_type());
        self.cg.builder.build_store(ret_ptr, value);
    }

    fn int_bin(&mut self, ret: PlaceRef, op: IntBinOp, left: PlaceRef, right: PlaceRef) {
        let lhs = self.build_int_load(left);
        let rhs = self.build_int_load(right);
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
        };
        let ret_ptr = self.build_typed_place_ptr(ret, self.cg.llvm.i64_type());
        self.cg.builder.build_store(ret_ptr, value);
    }

    fn mir_ins(&mut self, ins: MirInstr) {
        match ins {
            MirInstr::IntBin(ret, op, left, right) => self.int_bin(ret, op, left, right),
            MirInstr::IntUn(ret, op, right) => self.int_un(ret, op, right),
            MirInstr::Comp(ret, op, left, right) => self.comp(ret, op, left, right),
            MirInstr::StoreVal(ret, val) => self.store_val(ret, val),
            MirInstr::Call(ret, call_kind, fun, arg, retreat) => {
                self.call(ret, call_kind, fun, arg, retreat)
            }
            MirInstr::Field(ret, place, field, error) => self.field(ret, place, field, error),
            MirInstr::AssertVal(place, val, fallback) => self.assert_value(place, val, fallback),
            MirInstr::SetDiscriminant(block, field, val) => {
                self.set_discriminant(block, field, val)
            }
            MirInstr::Copy(to, from, error) => self.copy(to, from, error),
        }
    }

    fn build_bb(&mut self, bb: &mir::BasicBlock) {
        for ins in bb.ins() {
            self.mir_ins(ins)
        }
        match bb.exit() {
            mir::BlockExit::BlockInProgress => panic!("build_bb called on incomplete block"),
            mir::BlockExit::Jump(target) => {
                let target_block = self.bb(target);
                self.cg.builder.build_unconditional_branch(target_block);
            }
            mir::BlockExit::Return(status) => {
                let llvm_int = self.cg.llvm.i64_type().const_int(status as u64, false);
                self.cg.builder.build_return(Some(&llvm_int));
            }
        }
    }

    pub fn build(mut self) -> FunctionValue<'llvm> {
        let mir_fun = self.mir_fun.clone();
        for (bbref, bb) in mir_fun.f.iter_bb() {
            let block = self.bb(bbref);
            self.cg.builder.position_at_end(block);
            self.build_bb(bb);
        }
        self.llvm_fun
    }
}
