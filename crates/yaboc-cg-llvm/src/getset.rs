use inkwell::types::FunctionType;
use yaboc_hir_types::VTABLE_BIT;
use yaboc_layout::represent::ParserFunKind;
use yaboc_types::TypeId;

use super::*;

pub(crate) enum Callable<'llvm> {
    Pointer(PointerValue<'llvm>, FunctionType<'llvm>),
    Function(FunctionValue<'llvm>),
}

impl<'llvm> From<FunctionValue<'llvm>> for Callable<'llvm> {
    fn from(fun: FunctionValue<'llvm>) -> Self {
        Callable::Function(fun)
    }
}

trait FunctionTy: TargetSized {
    fn fun_ty<'llvm>(ctx: &mut CodeGenCtx<'llvm, '_>) -> FunctionType<'llvm>;
}

macro_rules! function_ty_impl {
    (fn($($n:ident: $t:ident),*) -> $ret:ident) => {
        impl<$ret: TargetSized, $($t: TargetSized),*> FunctionTy for fn($($t),*) -> $ret {
            fn fun_ty<'llvm>(ctx: &mut CodeGenCtx<'llvm, '_>) -> FunctionType<'llvm> {
                $(
                    let $n = <$t>::codegen_ty(ctx).into();
                )*
                $ret::codegen_ty(ctx).fn_type(&[$($n),*], false)
            }
        }
    };
}

function_ty_impl!(fn(a: A) -> R);
function_ty_impl!(fn(a: A, b: B) -> R);
function_ty_impl!(fn(a: A, b: B, c: C) -> R);
function_ty_impl!(fn(a: A, b: B, c: C, d: D) -> R);

impl<'llvm, 'comp> CodeGenCtx<'llvm, 'comp> {
    fn vtable_get<T: TargetSized, Target: TargetSized>(
        &mut self,
        value_ptr: PointerValue<'llvm>,
        field_path: &[u64],
    ) -> IResult<BasicValueEnum<'llvm>> {
        let ptr_ty = <&T>::codegen_ty(self).into_pointer_type();
        let actual_ptr_ptr = self.build_cast::<&&T, _>(value_ptr)?;
        let before_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                ptr_ty,
                actual_ptr_ptr,
                &[self.const_i64(-1)],
                "vtable_ptr_ptr",
            )?
        };
        let vtable_ptr = self
            .builder
            .build_load(ptr_ty, before_ptr, "vtable_ptr")?
            .into_pointer_value();
        let mut path = Vec::with_capacity(field_path.len() + 1);
        path.push(self.const_i64(0));
        path.extend(
            field_path
                .iter()
                .map(|x| self.llvm.i32_type().const_int(*x, false)),
        );
        let t_ty = T::codegen_ty(self);
        let target = unsafe {
            self.builder
                .build_in_bounds_gep(t_ty, vtable_ptr, &path, "")?
        };
        let target_ty = Target::codegen_ty(self);
        let vtable = self.builder.build_load(target_ty, target, "")?;
        Ok(vtable)
    }

    fn vtable_callable<T: TargetSized, Fun: FunctionTy>(
        &mut self,
        value_ptr: PointerValue<'llvm>,
        field_path: &[u64],
    ) -> IResult<Callable<'llvm>> {
        let ptr = self
            .vtable_get::<T, Fun>(value_ptr, field_path)?
            .into_pointer_value();
        let ty = Fun::fun_ty(self);
        Ok(Callable::Pointer(ptr, ty))
    }

    pub(super) fn get_object_start(
        &mut self,
        val: CgValue<'comp, 'llvm>,
    ) -> IResult<PointerValue<'llvm>> {
        let sa = val.layout.size_align(self.layouts).unwrap();
        if sa.before == 0 {
            return Ok(val.ptr);
        }
        self.build_byte_gep(val.ptr, self.const_i64(-(sa.before as i64)), "start")
    }

    pub(super) fn call_typecast_fun(
        &mut self,
        ret: CgReturnValue<'llvm>,
        arg: CgValue<'comp, 'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        let typecast = match arg.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::Typecast).into(),
            None => self.vtable_callable::<vtable::VTableHeader, vtable::TypecastFun>(
                arg.ptr,
                &[VTableHeaderFields::typecast_impl as u64],
            )?,
        };
        self.build_call_with_int_ret(typecast, &[ret.ptr.into(), arg.ptr.into(), ret.head.into()])
    }

    pub(super) fn call_field_access_fun(
        &mut self,
        ret: CgReturnValue<'llvm>,
        arg: CgValue<'comp, 'llvm>,
        block: BlockId,
        field: Identifier,
    ) -> IResult<IntValue<'llvm>> {
        let access = match arg.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::Field(field)).into(),
            None => {
                let index = self
                    .compiler_database
                    .db
                    .sorted_field_index(block, FieldName::Ident(field), false)
                    .expect("failed to lookup field index")
                    .expect("could not find field");
                self.vtable_callable::<vtable::BlockVTable, vtable::BlockFieldFun>(
                    arg.ptr,
                    &[BlockVTableFields::access_impl as u64, index as u64],
                )?
            }
        };
        self.build_call_with_int_ret(access, &[ret.ptr.into(), arg.ptr.into(), ret.head.into()])
    }

    fn call_parser_fun(
        &mut self,
        ret: CgReturnValue<'llvm>,
        fun: CgValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
        req: RequirementSet,
        fun_kind: ParserFunKind,
    ) -> IResult<IntValue<'llvm>> {
        let parser = match fun.layout.maybe_mono() {
            Some(mono) => {
                let part = self.parser_layout_part(arg.layout, req, fun_kind);
                self.sym_callable(mono, part).into()
            }
            None => {
                if ParserFunKind::Worker == fun_kind {
                    panic!("worker function must be monomorphized")
                }
                let meta = CallMeta {
                    req,
                    tail: fun_kind == ParserFunKind::TailWrapper,
                };
                let slot = self.collected_layouts.parser_slots.layout_vtable_offsets
                    [&((arg.layout, meta), fun.layout)];
                self.vtable_callable::<vtable::ParserVTable, vtable::ParserFun>(
                    fun.ptr,
                    &[ParserVTableFields::apply_table as u64, slot],
                )?
            }
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
        call_kind: RequirementSet,
        tail: bool,
    ) -> IResult<IntValue<'llvm>> {
        let part = self.parser_layout_part(arg.layout, call_kind, ParserFunKind::Worker);
        let parser = self.sym_callable(fun.layout, part);
        let call_ret = self.builder.build_call(
            parser,
            &[
                ret.ptr.into(),
                fun.ptr.into(),
                ret.head.into(),
                arg.ptr.into(),
            ],
            "impl_tail_call",
        )?;
        call_ret.set_tail_call(tail);
        call_ret.set_call_convention(self.tailcc());
        let ret = call_ret
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value();
        Ok(ret)
    }

    pub(super) fn call_parser_fun_wrapper(
        &mut self,
        ret: CgReturnValue<'llvm>,
        fun: CgValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
        call_kind: RequirementSet,
    ) -> IResult<IntValue<'llvm>> {
        self.call_parser_fun(ret, fun, arg, call_kind, ParserFunKind::Wrapper)
    }

    pub(super) fn call_parser_fun_tail(
        &mut self,
        ret: CgReturnValue<'llvm>,
        fun: CgValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
        call_kind: RequirementSet,
        parent_fun: Option<CgMonoValue<'comp, 'llvm>>,
    ) -> IResult<IntValue<'llvm>> {
        let sa = fun.layout.size_align_without_vtable(self.layouts).unwrap();
        let size = self.const_i64(sa.after as i64);
        let parser = match fun.layout.maybe_mono() {
            Some(mono) => {
                let part =
                    self.parser_layout_part(arg.layout, call_kind, ParserFunKind::TailWrapper);
                self.sym_callable(mono, part).into()
            }
            None => {
                let meta = CallMeta {
                    req: call_kind,
                    tail: true,
                };
                let slot = self.collected_layouts.parser_slots.layout_vtable_offsets
                    [&((arg.layout, meta), fun.layout)];
                self.vtable_callable::<vtable::ParserVTable, vtable::ParserFun>(
                    fun.ptr,
                    &[ParserVTableFields::apply_table as u64, slot],
                )?
            }
        };
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
            parent_fun.into()
        } else {
            fun
        };
        let call_ret = match parser {
            Callable::Pointer(ptr, ty) => self.builder.build_indirect_call(
                ty,
                ptr,
                &[
                    ret.ptr.into(),
                    fun.ptr.into(),
                    ret.head.into(),
                    arg.ptr.into(),
                ],
                "tail_call",
            ),
            Callable::Function(func_val) => self.builder.build_call(
                func_val,
                &[
                    ret.ptr.into(),
                    fun.ptr.into(),
                    ret.head.into(),
                    arg.ptr.into(),
                ],
                "tail_call",
            ),
        }?;
        call_ret.set_tail_call(true);
        call_ret.set_call_convention(self.tailcc());
        let ret = call_ret
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value();
        Ok(ret)
    }

    pub(super) fn call_fun_create(
        &mut self,
        ret: CgReturnValue<'llvm>,
        fun: CgValue<'comp, 'llvm>,
        slot: PSize,
    ) -> IResult<IntValue<'llvm>> {
        let create = match fun.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::CreateArgs(slot)).into(),
            None => self.vtable_callable::<vtable::FunctionVTable, vtable::CreateArgFun>(
                fun.ptr,
                &[FunctionVTableFields::apply_table as u64, slot],
            )?,
        };
        self.build_call_with_int_ret(create, &[ret.ptr.into(), fun.ptr.into(), ret.head.into()])
    }

    pub(super) fn call_current_element_fun(
        &mut self,
        ret: CgReturnValue<'llvm>,
        arg: CgValue<'comp, 'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        let current = match arg.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::CurrentElement).into(),
            None => self.vtable_callable::<vtable::ArrayVTable, vtable::CurrentElementFun>(
                arg.ptr,
                &[ArrayVTableFields::current_element_impl as u64],
            )?,
        };
        self.build_call_with_int_ret(current, &[ret.ptr.into(), arg.ptr.into(), ret.head.into()])
    }

    pub(super) fn call_single_forward_fun(
        &mut self,
        arg: CgValue<'comp, 'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        let single_forward = match arg.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::SingleForward).into(),
            None => self.vtable_callable::<vtable::ArrayVTable, vtable::SingleForwardFun>(
                arg.ptr,
                &[ArrayVTableFields::single_forward_impl as u64],
            )?,
        };
        self.build_call_with_int_ret(single_forward, &[arg.ptr.into()])
    }

    pub(super) fn call_array_len_fun(
        &mut self,
        arg: CgValue<'comp, 'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        let len = match arg.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::ArrayLen).into(),
            None => self.vtable_callable::<vtable::ArrayVTable, vtable::ArrayLenFun>(
                arg.ptr,
                &[ArrayVTableFields::len_impl as u64],
            )?,
        };
        self.build_call_with_int_ret(len, &[arg.ptr.into()])
    }

    pub(super) fn call_eval_fun_fun(
        &mut self,
        ret: CgReturnValue<'llvm>,
        arg: CgValue<'comp, 'llvm>,
        kind: ParserFunKind,
    ) -> IResult<IntValue<'llvm>> {
        let eval_fun = match arg.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::EvalFun(kind)).into(),
            None => self.vtable_callable::<vtable::FunctionVTable, vtable::EvalFunFun>(
                arg.ptr,
                &[FunctionVTableFields::eval_fun_impl as u64],
            )?,
        };
        self.build_call_with_int_ret(eval_fun, &[ret.ptr.into(), arg.ptr.into(), ret.head.into()])
    }

    pub(super) fn call_eval_fun_fun_wrapper(
        &mut self,
        ret: CgReturnValue<'llvm>,
        arg: CgValue<'comp, 'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        self.call_eval_fun_fun(ret, arg, ParserFunKind::Wrapper)
    }

    pub(super) fn call_eval_fun_fun_impl(
        &mut self,
        ret: CgReturnValue<'llvm>,
        arg: CgValue<'comp, 'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        self.call_eval_fun_fun(ret, arg, ParserFunKind::Worker)
    }

    pub(super) fn call_mask_fun(&mut self, arg: CgValue<'comp, 'llvm>) -> IResult<()> {
        let len = match arg.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::Mask).into(),
            None => self.vtable_callable::<vtable::VTableHeader, vtable::MaskFun>(
                arg.ptr,
                &[VTableHeaderFields::mask_impl as u64],
            )?,
        };
        let size = self.build_call_with_int_ret(len, &[arg.ptr.into()])?;
        // since we have a union of multiple types, we need to mask the leftover
        // padding after the current inhabitant
        if arg.layout.is_multi() {
            let mask_offset = self.build_byte_gep(arg.ptr, size, "mask_offset")?;
            let whole_size = arg
                .layout
                .size_align_without_vtable(self.layouts)
                .unwrap()
                .after;
            let whole_size = self.const_size_t(whole_size as i64);
            let mask_size = self.builder.build_int_sub(whole_size, size, "mask_size")?;
            let zero = self.llvm.i8_type().const_int(0, false);
            self.builder.build_memset(mask_offset, 1, zero, mask_size)?;
        }
        Ok(())
    }

    pub(super) fn call_skip_fun(
        &mut self,
        arg: CgValue<'comp, 'llvm>,
        count: IntValue<'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        let skip = match arg.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::Skip).into(),
            None => self.vtable_callable::<vtable::ArrayVTable, vtable::SkipFun>(
                arg.ptr,
                &[ArrayVTableFields::skip_impl as u64],
            )?,
        };
        self.build_call_with_int_ret(skip, &[arg.ptr.into(), count.into()])
    }

    pub(super) fn call_span_fun(
        &mut self,
        ret: CgReturnValue<'llvm>,
        start: CgValue<'comp, 'llvm>,
        end: CgValue<'comp, 'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        let span = match start.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::Span).into(),
            None => self.vtable_callable::<vtable::ArrayVTable, vtable::SpanFun>(
                start.ptr,
                &[ArrayVTableFields::span_impl as u64],
            )?,
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
    ) -> IResult<IntValue<'llvm>> {
        Ok(match layout {
            Some(mono) => {
                let ty = mono.mono_layout().1;
                self.deref_level(ty)
            }
            None => self
                .vtable_get::<vtable::VTableHeader, i64>(
                    ptr,
                    &[VTableHeaderFields::deref_level as u64],
                )?
                .into_int_value(),
        })
    }

    pub(super) fn build_return_value(
        &mut self,
        val: CgValue<'comp, 'llvm>,
    ) -> IResult<CgReturnValue<'llvm>> {
        let mut head = self.build_deref_level_get(val.layout.maybe_mono(), val.ptr)?;
        if val.layout.is_multi() {
            let tag = self.const_i64(1 << VTABLE_BIT);
            head = self.builder.build_or(head, tag, "vtable_tag")?;
        }
        Ok(CgReturnValue::new(head, val.ptr))
    }

    pub(super) fn build_check_i64_bit_set(
        &mut self,
        val: IntValue<'llvm>,
        bit: u8,
    ) -> IResult<IntValue<'llvm>> {
        let set_bit = self.const_i64(1 << bit);
        let and = self.builder.build_and(set_bit, val, "")?;
        let comp = self
            .builder
            .build_int_compare(IntPredicate::NE, and, self.const_i64(0), "")?;
        Ok(comp)
    }

    pub(super) fn build_vtable_arg_set_info_get(
        &mut self,
        fun: PointerValue<'llvm>,
        argnum: PSize,
    ) -> IResult<(IntValue<'llvm>, IntValue<'llvm>)> {
        let arginfo_ptr_ptr = self.build_cast::<*const *const vtable::ArgDescriptor, _>(fun)?;
        let arginfo_ptr_ty = <*const vtable::ArgDescriptor>::codegen_ty(self);
        let arginfo_ty = vtable::ArgDescriptor::codegen_ty(self);
        let vtable_ptr_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                arginfo_ptr_ty,
                arginfo_ptr_ptr,
                &[self.const_i64(-1)],
                "vtable_ptr_ptr",
            )?
        };
        let vtable_ptr = self
            .builder
            .build_load(arginfo_ptr_ty, vtable_ptr_ptr, "vtable_ptr")?
            .into_pointer_value();
        let head_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                arginfo_ty,
                vtable_ptr,
                &[
                    self.const_i64(-1 - argnum as i64),
                    self.llvm
                        .i32_type()
                        .const_int(vtable::ArgDescriptorFields::head as u64, false),
                ],
                "",
            )?
        };
        let offset_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                arginfo_ty,
                vtable_ptr,
                &[
                    self.const_i64(-1 - argnum as i64),
                    self.llvm
                        .i32_type()
                        .const_int(vtable::ArgDescriptorFields::offset as u64, false),
                ],
                "",
            )?
        };
        let head = self.build_i64_load(head_ptr, "")?;
        let offset = self.build_size_load(offset_ptr, "")?;
        Ok((head, offset))
    }

    pub(super) fn build_arg_set(
        &mut self,
        fun: CgValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
        argnum: PSize,
    ) -> IResult<IntValue<'llvm>> {
        let (head, offset) = match fun.layout.maybe_mono() {
            Some(mono) => {
                let (head, offset) = self.arg_level_and_offset(mono, argnum);
                (self.const_i64(head), self.const_size_t(offset as i64))
            }
            None => self.build_vtable_arg_set_info_get(fun.ptr, argnum)?,
        };
        let fun_any_ptr = self.build_cast::<*mut u8, _>(fun.ptr)?;
        let fun_arg_ptr = self.build_byte_gep(fun_any_ptr, offset, "")?;
        let fun_arg = CgReturnValue::new(head, fun_arg_ptr);
        self.call_typecast_fun(fun_arg, arg)
    }

    pub(super) fn call_len_fun(
        &mut self,
        ret: PointerValue<'llvm>,
        fun: CgValue<'comp, 'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        let create = match fun.layout.maybe_mono() {
            Some(mono) => self.sym_callable(mono, LayoutPart::Len).into(),
            None => self.vtable_callable::<vtable::ParserVTable, vtable::LenFun>(
                fun.ptr,
                &[ParserVTableFields::len_impl as u64],
            )?,
        };
        self.build_call_with_int_ret(create, &[ret.into(), fun.ptr.into()])
    }

    pub(super) fn build_array_parser_get(
        &mut self,
        array: CgMonoValue<'comp, 'llvm>,
    ) -> IResult<CgValue<'comp, 'llvm>> {
        let (MonoLayout::Array { parser, .. } | MonoLayout::ArrayParser(Some((parser, _)))) =
            array.layout.mono_layout().0
        else {
            panic!("array_parser_field called on non-array");
        };
        let ptr = self.build_center_gep(array.ptr, *parser)?;
        Ok(CgValue::new(*parser, ptr))
    }

    pub(super) fn build_array_slice_get(
        &mut self,
        array: CgMonoValue<'comp, 'llvm>,
    ) -> IResult<CgValue<'comp, 'llvm>> {
        let MonoLayout::Array { slice, .. } = array.layout.mono_layout().0 else {
            panic!("array_slice_field called on non-array");
        };
        let offset = array
            .layout
            .inner()
            .size_align_without_vtable(self.layouts)
            .unwrap()
            .after
            - slice.size_align_without_vtable(self.layouts).unwrap().after;
        let ptr = self.build_byte_gep(array.ptr, self.const_i64(offset as i64), "slice_ptr")?;
        Ok(CgValue::new(*slice, ptr))
    }
}
