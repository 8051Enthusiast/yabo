use yaboc_layout::represent::ParserFunKind;
use yaboc_types::TypeId;

use crate::{defs::TAILCC, val::CgReturnValue};

use super::*;

impl<'llvm, 'comp> CodeGenCtx<'llvm, 'comp> {
    fn vtable_get<T: TargetSized>(
        &mut self,
        value_ptr: PointerValue<'llvm>,
        field_path: &[u64],
    ) -> BasicValueEnum<'llvm> {
        let ty = <&&T>::codegen_ty(self).into_pointer_type();
        let actual_ptr_ptr = self
            .builder
            .build_bitcast(value_ptr, ty, "casted_ptr_ptr")
            .into_pointer_value();
        let before_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                actual_ptr_ptr,
                &[self.const_i64(-1)],
                "vtable_ptr_ptr",
            )
        };
        let vtable_ptr = self
            .builder
            .build_load(before_ptr, "vtable_ptr")
            .into_pointer_value();
        let mut path = Vec::with_capacity(field_path.len() + 1);
        path.push(self.const_i64(0));
        path.extend(
            field_path
                .iter()
                .map(|x| self.llvm.i32_type().const_int(*x, false)),
        );
        let target = unsafe { self.builder.build_in_bounds_gep(vtable_ptr, &path, "") };
        self.builder.build_load(target, "")
    }

    fn vtable_callable<T: TargetSized>(
        &mut self,
        value_ptr: PointerValue<'llvm>,
        field_path: &[u64],
    ) -> CallableValue<'llvm> {
        self.vtable_get::<T>(value_ptr, field_path)
            .into_pointer_value()
            .try_into()
            .unwrap()
    }

    pub(super) fn get_object_start(&mut self, val: CgValue<'comp, 'llvm>) -> PointerValue<'llvm> {
        match val.layout.maybe_mono() {
            Some(_) => val.ptr,
            None => {
                self.build_byte_gep(val.ptr, self.const_i64(-(self.word_size() as i64)), "start")
            }
        }
    }

    pub(super) fn call_typecast_fun(
        &mut self,
        ret: CgReturnValue<'llvm>,
        arg: CgValue<'comp, 'llvm>,
    ) -> IntValue<'llvm> {
        let typecast = match arg.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::Typecast),
            None => self.vtable_callable::<vtable::VTableHeader>(
                arg.ptr,
                &[VTableHeaderFields::typecast_impl as u64],
            ),
        };
        self.build_call_with_int_ret(typecast, &[ret.ptr.into(), arg.ptr.into(), ret.head.into()])
    }

    pub(super) fn call_field_access_fun(
        &mut self,
        ret: CgReturnValue<'llvm>,
        arg: CgValue<'comp, 'llvm>,
        block: BlockId,
        field: Identifier,
    ) -> IntValue<'llvm> {
        let access = match arg.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::Field(field)),
            None => {
                let index = self
                    .compiler_database
                    .db
                    .sorted_field_index(block, FieldName::Ident(field), false)
                    .expect("failed to lookup field index")
                    .expect("could not find field");
                self.vtable_callable::<vtable::BlockVTable>(
                    arg.ptr,
                    &[BlockVTableFields::access_impl as u64, index as u64],
                )
            }
        };
        self.build_call_with_int_ret(access, &[ret.ptr.into(), arg.ptr.into(), ret.head.into()])
    }

    fn call_parser_fun(
        &mut self,
        ret: CgReturnValue<'llvm>,
        fun: CgValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
        slot: u64,
        req: RequirementSet,
        fun_kind: ParserFunKind,
    ) -> IntValue<'llvm> {
        let parser = match fun.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::Parse(slot, req, fun_kind)),
            None => self.vtable_callable::<vtable::ParserVTable>(
                fun.ptr,
                &[ParserVTableFields::apply_table as u64, slot],
            ),
        };
        self.build_call_with_int_ret(
            parser,
            &[
                ret.ptr.into(),
                fun.ptr.into(),
                ret.head.into(),
                arg.ptr.into(),
            ],
        )
    }

    pub(super) fn call_parser_fun_impl(
        &mut self,
        ret: CgReturnValue<'llvm>,
        fun: CgMonoValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
        slot: u64,
        call_kind: RequirementSet,
        tail: bool,
    ) -> IntValue<'llvm> {
        let parser = self.sym_callable(
            fun.layout,
            LayoutPart::Parse(slot, call_kind, ParserFunKind::Worker),
        );
        let call_ret = self.builder.build_call(
            parser,
            &[
                ret.ptr.into(),
                fun.ptr.into(),
                ret.head.into(),
                arg.ptr.into(),
            ],
            "impl_tail_call",
        );
        call_ret.set_tail_call(tail);
        call_ret.set_call_convention(TAILCC);
        let ret = call_ret
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value();
        ret
    }

    pub(super) fn call_parser_fun_wrapper(
        &mut self,
        ret: CgReturnValue<'llvm>,
        fun: CgValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
        slot: u64,
        call_kind: RequirementSet,
    ) -> IntValue<'llvm> {
        self.call_parser_fun(ret, fun, arg, slot, call_kind, ParserFunKind::Wrapper)
    }

    pub(super) fn call_parser_fun_tail(
        &mut self,
        ret: CgReturnValue<'llvm>,
        fun: CgValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
        slot: u64,
        call_kind: RequirementSet,
        parent_fun: Option<CgValue<'comp, 'llvm>>,
    ) -> IntValue<'llvm> {
        let parser = match fun.layout.maybe_mono() {
            Some(mono) => self.sym_callable(
                mono,
                LayoutPart::Parse(slot, call_kind, ParserFunKind::TailWrapper),
            ),
            None => self.vtable_callable::<vtable::ParserVTable>(
                fun.ptr,
                &[ParserVTableFields::apply_table as u64, slot],
            ),
        };
        let sa = fun.layout.size_align_without_vtable(self.layouts).unwrap();
        let size = self.const_i64(sa.size as i64);
        let fun = if let Some(parent_fun) = parent_fun {
            self.builder
                .build_memcpy(
                    parent_fun.ptr,
                    sa.align() as u32,
                    fun.ptr,
                    sa.align() as u32,
                    size,
                )
                .unwrap();
            parent_fun
        } else {
            fun
        };
        let call_ret = self.builder.build_call(
            parser,
            &[
                ret.ptr.into(),
                fun.ptr.into(),
                ret.head.into(),
                arg.ptr.into(),
            ],
            "tail_call",
        );
        call_ret.set_tail_call(true);
        call_ret.set_call_convention(TAILCC);
        let ret = call_ret
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value();
        ret
    }

    pub(super) fn call_fun_create(
        &mut self,
        ret: CgReturnValue<'llvm>,
        fun: CgValue<'comp, 'llvm>,
        slot: PSize,
    ) -> IntValue<'llvm> {
        let create = match fun.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::CreateArgs(slot)),
            None => self.vtable_callable::<vtable::FunctionVTable>(
                fun.ptr,
                &[FunctionVTableFields::apply_table as u64, slot],
            ),
        };
        self.build_call_with_int_ret(create, &[ret.ptr.into(), fun.ptr.into(), ret.head.into()])
    }

    pub(super) fn call_current_element_fun(
        &mut self,
        ret: CgReturnValue<'llvm>,
        arg: CgValue<'comp, 'llvm>,
    ) -> IntValue<'llvm> {
        let current = match arg.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::CurrentElement),
            None => self.vtable_callable::<vtable::ArrayVTable>(
                arg.ptr,
                &[ArrayVTableFields::current_element_impl as u64],
            ),
        };
        self.build_call_with_int_ret(current, &[ret.ptr.into(), arg.ptr.into(), ret.head.into()])
    }

    pub(super) fn call_single_forward_fun(
        &mut self,
        arg: CgValue<'comp, 'llvm>,
    ) -> IntValue<'llvm> {
        let single_forward = match arg.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::SingleForward),
            None => self.vtable_callable::<vtable::ArrayVTable>(
                arg.ptr,
                &[ArrayVTableFields::single_forward_impl as u64],
            ),
        };
        self.build_call_with_int_ret(single_forward, &[arg.ptr.into()])
    }

    pub(super) fn call_array_len_fun(
        &mut self,
        arg: CgValue<'comp, 'llvm>,
    ) -> IntValue<'llvm> {
        let len = match arg.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::ArrayLen),
            None => self.vtable_callable::<vtable::ArrayVTable>(
                arg.ptr,
                &[ArrayVTableFields::len_impl as u64],
            ),
        };
        self.build_call_with_int_ret(len, &[arg.ptr.into()])
    }

    pub(super) fn call_skip_fun(
        &mut self,
        arg: CgValue<'comp, 'llvm>,
        count: IntValue<'llvm>,
    ) -> IntValue<'llvm> {
        let skip = match arg.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::Skip),
            None => self.vtable_callable::<vtable::ArrayVTable>(
                arg.ptr,
                &[ArrayVTableFields::skip_impl as u64],
            ),
        };
        self.build_call_with_int_ret(skip, &[arg.ptr.into(), count.into()])
    }

    pub(super) fn call_span_fun(
        &mut self,
        ret: CgReturnValue<'llvm>,
        start: CgValue<'comp, 'llvm>,
        end: CgValue<'comp, 'llvm>,
    ) -> IntValue<'llvm> {
        let span = match start.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::Span),
            None => self.vtable_callable::<vtable::ArrayVTable>(
                start.ptr,
                &[ArrayVTableFields::span_impl as u64],
            ),
        };
        self.build_call_with_int_ret(
            span,
            &[
                ret.ptr.into(),
                start.ptr.into(),
                ret.head.into(),
                end.ptr.into(),
            ],
        )
    }

    pub(super) fn deref_level(&mut self, ty: TypeId) -> IntValue<'llvm> {
        let level = self.compiler_database.db.deref_level(ty).unwrap();
        self.const_i64(level.into_shifted_runtime_value() as i64)
    }

    pub(super) fn build_deref_level_get(
        &mut self,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
    ) -> IntValue<'llvm> {
        match layout {
            Some(mono) => {
                let ty = mono.mono_layout().1;
                self.deref_level(ty)
            }
            None => self
                .vtable_get::<vtable::VTableHeader>(ptr, &[VTableHeaderFields::deref_level as u64])
                .into_int_value(),
        }
    }

    pub(super) fn build_check_i64_bit_set(
        &mut self,
        val: IntValue<'llvm>,
        bit: u8,
    ) -> IntValue<'llvm> {
        let set_bit = self.const_i64(1 << bit);
        let and = self.builder.build_and(set_bit, val, "");
        self.builder
            .build_int_compare(IntPredicate::NE, and, self.const_i64(0), "")
    }

    pub(super) fn build_vtable_arg_set_info_get(
        &mut self,
        fun: PointerValue<'llvm>,
        argnum: PSize,
    ) -> (IntValue<'llvm>, IntValue<'llvm>) {
        let arginfo_ptr_ptr = self.build_cast::<*const *const vtable::ArgDescriptor, _>(fun);
        let vtable_ptr_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                arginfo_ptr_ptr,
                &[self.const_i64(-1)],
                "vtable_ptr_ptr",
            )
        };
        let vtable_ptr = self
            .builder
            .build_load(vtable_ptr_ptr, "vtable_ptr")
            .into_pointer_value();
        let head_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                vtable_ptr,
                &[
                    self.const_i64(-1 - argnum as i64),
                    self.llvm
                        .i32_type()
                        .const_int(vtable::ArgDescriptorFields::head as u64, false),
                ],
                "",
            )
        };
        let offset_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                vtable_ptr,
                &[
                    self.const_i64(-1 - argnum as i64),
                    self.llvm
                        .i32_type()
                        .const_int(vtable::ArgDescriptorFields::offset as u64, false),
                ],
                "",
            )
        };
        let head = self.builder.build_load(head_ptr, "").into_int_value();
        let offset = self.builder.build_load(offset_ptr, "").into_int_value();
        (head, offset)
    }

    pub(super) fn build_arg_set(
        &mut self,
        fun: CgValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
        argnum: PSize,
    ) -> IntValue<'llvm> {
        let (head, offset) = match fun.layout.maybe_mono() {
            Some(mono) => {
                let (head, offset) = self.arg_level_and_offset(mono, argnum);
                (self.const_i64(head), self.const_size_t(offset as i64))
            }
            None => self.build_vtable_arg_set_info_get(fun.ptr, argnum),
        };
        let fun_any_ptr = self.build_cast::<*mut u8, _>(fun.ptr);
        let fun_arg_ptr = self.build_byte_gep(fun_any_ptr, offset, "");
        let fun_arg = CgReturnValue::new(head, fun_arg_ptr);
        self.call_typecast_fun(fun_arg, arg)
    }

    pub(super) fn call_len_fun(
        &mut self,
        ret: PointerValue<'llvm>,
        fun: CgValue<'comp, 'llvm>,
    ) -> IntValue<'llvm> {
        let create = match fun.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::Len),
            None => self.vtable_callable::<vtable::ParserVTable>(
                fun.ptr,
                &[ParserVTableFields::len_impl as u64],
            ),
        };
        self.build_call_with_int_ret(create, &[ret.into(), fun.ptr.into()])
    }

}
