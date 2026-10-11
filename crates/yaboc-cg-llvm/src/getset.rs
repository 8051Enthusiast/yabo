use inkwell::{types::FunctionType, values::CallSiteValue};
use yaboc_layout::{
    LayoutSlice,
    collect::LCallReq,
    part::ParserFunKind::{self, TailWrapper},
    vtable::FunctionInfoFields,
};

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

pub(crate) trait FunctionTy {
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

fn return_status(call: CallSiteValue) -> IntValue {
    call.try_as_basic_value()
        .basic()
        .expect("function should not return void")
        .into_int_value()
}

function_ty_impl!(fn(a: A) -> R);
function_ty_impl!(fn(a: A, b: B) -> R);
function_ty_impl!(fn(a: A, b: B, c: C) -> R);
function_ty_impl!(fn(a: A, b: B, c: C, d: D) -> R);

impl<'llvm, 'comp> CodeGenCtx<'llvm, 'comp> {
    fn read_vtable_pointer(
        &mut self,
        value_ptr: PointerValue<'llvm>,
    ) -> IResult<PointerValue<'llvm>> {
        let ptr_ty = self.any_ptr();
        let before_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                ptr_ty,
                value_ptr,
                &[self.const_i64(-1)],
                "vtable_ptr_ptr",
            )?
        };
        let vtable_ptr = self
            .builder
            .build_load(ptr_ty, before_ptr, "vtable_ptr")?
            .into_pointer_value();
        Ok(vtable_ptr)
    }

    fn llvm_load_relative(&mut self) -> FunctionValue<'llvm> {
        self.get_intrinsic::<fn(ptr: *const u8, offset: RelativeVPtr<()>) -> *const u8>(
            "llvm.load.relative.%1",
        )
    }

    fn vtable_get_val_at_offset<Target: TargetSized>(
        &mut self,
        vtable_ptr: PointerValue<'llvm>,
        offset: i64,
    ) -> IResult<BasicValueEnum<'llvm>> {
        let target = self.build_byte_gep(vtable_ptr, self.const_i64(offset), "vtable_field")?;
        let target_ty = Target::codegen_ty(self);
        self.builder.build_load(target_ty, target, "vtable_val")
    }

    fn vtable_get_rel_ptr_at_offset(
        &mut self,
        vtable_ptr: PointerValue<'llvm>,
        offset: i64,
    ) -> IResult<PointerValue<'llvm>> {
        let load_relative = self.llvm_load_relative();
        let llvm_offset = self.llvm.i32_type().const_int(offset as u64, true);
        let res = self
            .builder
            .build_direct_call(
                load_relative,
                &[vtable_ptr.into(), llvm_offset.into()],
                "get_rel",
            )?
            .try_as_basic_value()
            .basic()
            .unwrap()
            .into_pointer_value();
        Ok(res)
    }

    fn vtable_get_ptr_from_offset(
        &mut self,
        value_ptr: PointerValue<'llvm>,
        offset: i64,
    ) -> IResult<PointerValue<'llvm>> {
        let vtable_ptr = self.read_vtable_pointer(value_ptr)?;
        if self.options.target.relative_vptrs {
            self.vtable_get_rel_ptr_at_offset(vtable_ptr, offset)
        } else {
            self.vtable_get_val_at_offset::<*const u8>(vtable_ptr, offset)
                .map(|x| x.into_pointer_value())
        }
    }

    fn vtable_offset_callable<Fun: FunctionTy>(
        &mut self,
        value_ptr: PointerValue<'llvm>,
        offset: i64,
    ) -> IResult<Callable<'llvm>> {
        let ptr = if self.options.target.relative_vptrs {
            self.vtable_get_ptr_from_offset(value_ptr, offset)?
        } else {
            self.vtable_get_ptr_from_offset(value_ptr, offset)?
        };
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

    fn dispatch<Fun: FunctionTy>(
        &mut self,
        dispatchee: CgValue<'comp, 'llvm>,
        part: LayoutPart<'comp>,
        args: &[BasicMetadataValueEnum<'llvm>],
    ) -> IResult<CallSiteValue<'llvm>> {
        let fun = match dispatchee.layout.maybe_mono() {
            Some(mono) => self.layout_part_fun_val(mono, part).into(),
            None => {
                let offset = if self.options.target.relative_vptrs {
                    self.layout_part_vtable_offset::<RelPtr>(dispatchee.layout, part)
                } else {
                    self.layout_part_vtable_offset::<AbsPtr>(dispatchee.layout, part)
                };
                self.vtable_offset_callable::<Fun>(dispatchee.ptr, offset)?
            }
        };
        match fun {
            Callable::Pointer(ptr, ty) => self.builder.build_indirect_call(ty, ptr, args, "call"),
            Callable::Function(fun) => self.builder.build_call(fun, args, "call"),
        }
    }

    fn dispatch_int_ret<Fun: FunctionTy>(
        &mut self,
        dispatchee: CgValue<'comp, 'llvm>,
        part: LayoutPart<'comp>,
        args: &[BasicMetadataValueEnum<'llvm>],
    ) -> IResult<IntValue<'llvm>> {
        Ok(return_status(self.dispatch::<Fun>(dispatchee, part, args)?))
    }

    fn handle_vtable_copy(
        &mut self,
        ret: CgReturnValue<'comp, 'llvm>,
        dispatchee: CgValue<'comp, 'llvm>,
    ) -> IResult<()> {
        if ret.layout.is_multi() {
            let vtable_pointer = self.build_vtable_get(dispatchee)?;
            self.build_vtable_store(ret.ptr, vtable_pointer)?;
        }
        Ok(())
    }

    fn get_return_vtable(
        &mut self,
        dispatchee: CgValue<'comp, 'llvm>,
        part: LayoutPart<'comp>,
    ) -> IResult<Option<PointerValue<'llvm>>> {
        match dispatchee.layout.maybe_mono() {
            Some(mono) => match mono.return_layout(part, self.layouts).unwrap().maybe_mono() {
                Some(mono_ret) => Ok(Some(self.get_vtable_tag(mono_ret))),
                None => Ok(None),
            },
            None => {
                let data = &self.options.target.data;
                let offset = if self.options.target.relative_vptrs {
                    self.layout_part_vtable_offset::<RelPtr>(dispatchee.layout, part)
                        + <FunctionInfo<RelPtr, fn(i64) -> i64>>::path_offset(
                            data,
                            &[FunctionInfoFields::vtable as i64],
                        )
                } else {
                    self.layout_part_vtable_offset::<AbsPtr>(dispatchee.layout, part)
                        + <FunctionInfo<AbsPtr, fn(i64) -> i64>>::path_offset(
                            data,
                            &[FunctionInfoFields::vtable as i64],
                        )
                };
                self.vtable_get_ptr_from_offset(dispatchee.ptr, offset)
                    .map(Some)
            }
        }
    }

    fn handle_vtable_return_write(
        &mut self,
        ret: CgReturnValue<'comp, 'llvm>,
        dispatchee: CgValue<'comp, 'llvm>,
        part: LayoutPart<'comp>,
    ) -> IResult<()> {
        if ret.layout.is_multi() {
            if let Some(vtable_pointer) = self.get_return_vtable(dispatchee, part)? {
                self.build_vtable_store(ret.ptr, vtable_pointer)?;
            }
        }
        Ok(())
    }

    pub(super) fn call_deref_fun(
        &mut self,
        ret: CgReturnValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        self.handle_vtable_return_write(ret, arg, LayoutPart::Deref)?;
        self.dispatch_int_ret::<vtable::DerefFun>(
            arg,
            LayoutPart::Deref,
            &[ret.ptr, arg.ptr, ret.head].map(|x| x.into()),
        )
    }

    pub(super) fn call_field_access_fun(
        &mut self,
        ret: CgReturnValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
        field: Identifier,
    ) -> IResult<IntValue<'llvm>> {
        self.handle_vtable_return_write(ret, arg, LayoutPart::Field(field))?;
        self.dispatch_int_ret::<vtable::BlockFieldFun>(
            arg,
            LayoutPart::Field(field),
            &[ret.ptr, arg.ptr, ret.head].map(|x| x.into()),
        )
    }

    pub(super) fn consume_const_length(
        &mut self,
        arg: CgValue<'comp, 'llvm>,
        fun: CgMonoValue<'comp, 'llvm>,
        globals: PointerValue<'llvm>,
        length: u64,
    ) -> IResult<CgValue<'comp, 'llvm>> {
        let llvm_fun = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        let fail_bb = self.llvm.append_basic_block(llvm_fun, "eof");
        let succ_bb = self.llvm.append_basic_block(llvm_fun, "succ");

        let arg_size = arg
            .layout
            .size_align_without_vtable(self.layouts)
            .unwrap()
            .after_aligned()
            .total_size();
        let old_copy = self.build_const_offset_byte_gep(fun.ptr, -(arg_size as i64), "old_copy")?;
        let old_copy = CgValue::new(arg.layout, old_copy);
        self.build_copy_invariant(old_copy, arg)?;

        let ptr_diff = self.call_array_len_fun(arg, globals)?;
        let limit = self.const_i64(length as i64);
        let eof_check =
            self.builder
                .build_int_compare(IntPredicate::UGE, ptr_diff, limit, "eof_check")?;
        self.builder
            .build_conditional_branch(eof_check, succ_bb, fail_bb)?;
        self.builder.position_at_end(fail_bb);
        let eof = self.const_i64(ReturnStatus::Eof as i64);
        self.builder.build_return(Some(&eof))?;
        self.builder.position_at_end(succ_bb);
        self.call_skip_fun(arg, limit, globals)?;
        Ok(old_copy)
    }

    pub(super) fn call_parser_fun_impl_without_ret(
        &mut self,
        ret: CgReturnValue<'comp, 'llvm>,
        fun: CgMonoValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
        call_kind: LCallReq,
    ) -> IResult<CallSiteValue<'llvm>> {
        let info = &self.collected_layouts.layout_info.info[&fun.layout.inner()];
        let (call_kind, precheck) = info.modify_reqs(call_kind);
        let parser = self.parser_impl_fun_val(fun.layout, arg.layout, call_kind);
        let passed_arg = if let Some(len) = precheck {
            self.consume_const_length(arg, fun, ret.head, len)?
        } else {
            arg
        };
        let call = self.builder.build_call(
            parser,
            &[
                ret.ptr.into(),
                fun.ptr.into(),
                ret.head.into(),
                passed_arg.ptr.into(),
            ],
            "impl_tail_call",
        )?;
        call.set_call_convention(self.tailcc());
        Ok(call)
    }

    pub(super) fn call_parser_fun_impl(
        &mut self,
        ret: CgReturnValue<'comp, 'llvm>,
        fun: CgMonoValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
        call_kind: LCallReq,
    ) -> IResult<()> {
        let call_ret = self.call_parser_fun_impl_without_ret(ret, fun, arg, call_kind)?;
        self.set_tail_call(call_ret, true);
        let ret = return_status(call_ret);
        self.builder.build_return(Some(&ret))?;
        Ok(())
    }

    pub(super) fn call_parser_fun_wrapper(
        &mut self,
        ret: CgReturnValue<'comp, 'llvm>,
        fun: CgValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
        call_kind: LCallReq,
    ) -> IResult<IntValue<'llvm>> {
        let part = LayoutPart::Parse(call_kind, ParserFunKind::Wrapper, arg.layout);
        self.handle_vtable_return_write(ret, fun, part)?;
        self.dispatch_int_ret::<vtable::ParserFun>(
            fun,
            part,
            &[ret.ptr, fun.ptr, ret.head, arg.ptr].map(|x| x.into()),
        )
    }

    pub(super) fn call_parser_fun_tail(
        &mut self,
        ret: CgReturnValue<'comp, 'llvm>,
        fun: CgValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
        call_kind: LCallReq,
        parent_fun: Option<CgMonoValue<'comp, 'llvm>>,
    ) -> IResult<IntValue<'llvm>> {
        let sa = fun.layout.size_align_without_vtable(self.layouts).unwrap();
        let size = self.const_i64(sa.after as i64);
        let part = LayoutPart::Parse(call_kind, ParserFunKind::TailWrapper, arg.layout);
        self.handle_vtable_return_write(ret, fun, part)?;
        let fun_copy = if let Some(parent_fun) = parent_fun {
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
        let call_ret = self.dispatch::<vtable::ParserFun>(
            fun,
            part,
            &[ret.ptr, fun_copy.ptr, ret.head, arg.ptr].map(|x| x.into()),
        )?;
        self.set_tail_call(call_ret, true);
        let ret = return_status(call_ret);
        Ok(ret)
    }

    pub(super) fn call_fun_create(
        &mut self,
        ret: CgReturnValue<'comp, 'llvm>,
        fun: CgValue<'comp, 'llvm>,
        args: LayoutSlice<'comp>,
    ) -> IResult<IntValue<'llvm>> {
        let part = LayoutPart::CreateArgs(args);
        self.handle_vtable_return_write(ret, fun, part)?;
        self.dispatch_int_ret::<vtable::CreateArgFun>(
            fun,
            part,
            &[ret.ptr, fun.ptr, ret.head].map(|x| x.into()),
        )
    }

    pub(super) fn call_current_element_fun(
        &mut self,
        ret: CgReturnValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        let part = LayoutPart::CurrentElement;
        self.handle_vtable_return_write(ret, arg, part)?;
        self.dispatch_int_ret::<vtable::CurrentElementFun>(
            arg,
            part,
            &[ret.ptr, arg.ptr, ret.head].map(|x| x.into()),
        )
    }

    pub(super) fn call_single_forward_fun(
        &mut self,
        arg: CgValue<'comp, 'llvm>,
        globals: PointerValue<'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        self.dispatch_int_ret::<vtable::SingleForwardFun>(
            arg,
            LayoutPart::SingleForward,
            &[arg.ptr.into(), globals.into()],
        )
    }

    pub(super) fn call_array_len_fun(
        &mut self,
        arg: CgValue<'comp, 'llvm>,
        globals: PointerValue<'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        self.dispatch_int_ret::<vtable::ArrayLenFun>(
            arg,
            LayoutPart::ArrayLen,
            &[arg.ptr.into(), globals.into()],
        )
    }

    pub(super) fn call_eval_fun_fun(
        &mut self,
        ret: CgReturnValue<'comp, 'llvm>,
        fun: CgValue<'comp, 'llvm>,
        arg: Option<CgValue<'comp, 'llvm>>,
        kind: ParserFunKind,
        req: LCallReq,
    ) -> IResult<CallSiteValue<'llvm>> {
        let part = LayoutPart::EvalFun(req, kind);
        self.handle_vtable_return_write(ret, fun, part)?;
        let call = if let Some(arg) = arg {
            assert_ne!(kind, ParserFunKind::Wrapper);
            self.dispatch::<vtable::EvalFunFunInternal>(
                fun,
                part,
                &[ret.ptr, fun.ptr, ret.head, arg.ptr].map(|x| x.into()),
            )?
        } else {
            self.dispatch::<vtable::EvalFunFun>(
                fun,
                part,
                &[ret.ptr, fun.ptr, ret.head].map(|x| x.into()),
            )?
        };
        if let ParserFunKind::TailWrapper | ParserFunKind::Worker = kind {
            self.set_tail_call(call, false);
        }
        Ok(call)
    }

    pub(super) fn call_eval_fun_fun_wrapper(
        &mut self,
        ret: CgReturnValue<'comp, 'llvm>,
        fun: CgValue<'comp, 'llvm>,
        req: LCallReq,
    ) -> IResult<IntValue<'llvm>> {
        self.call_eval_fun_fun(ret, fun, None, ParserFunKind::Wrapper, req)
            .map(return_status)
    }

    pub(super) fn call_eval_fun_fun_tail(
        &mut self,
        ret: CgReturnValue<'comp, 'llvm>,
        fun: CgValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
        call_kind: LCallReq,
        parent_fun: Option<CgMonoValue<'comp, 'llvm>>,
    ) -> IResult<IntValue<'llvm>> {
        let sa = fun.layout.size_align_without_vtable(self.layouts).unwrap();
        let size = self.const_i64(sa.after as i64);
        let fun_copy = if let Some(parent_fun) = parent_fun {
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
        let part = LayoutPart::EvalFun(call_kind, TailWrapper);
        self.handle_vtable_return_write(ret, fun, part)?;
        let call_ret = self.dispatch::<vtable::EvalFunFunInternal>(
            fun,
            part,
            &[ret.ptr, fun_copy.ptr, ret.head, arg.ptr].map(|x| x.into()),
        )?;
        self.set_tail_call(call_ret, true);
        let ret = return_status(call_ret);
        Ok(ret)
    }

    pub(super) fn call_eval_fun_fun_impl(
        &mut self,
        ret: CgReturnValue<'comp, 'llvm>,
        fun: CgValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
        req: LCallReq,
    ) -> IResult<()> {
        let info = &self.collected_layouts.layout_info.info[&fun.layout];
        let (req, _) = info.modify_reqs(req);
        let ret = return_status(self.call_eval_fun_fun(
            ret,
            fun,
            Some(arg),
            ParserFunKind::Worker,
            req,
        )?);
        self.builder.build_return(Some(&ret))?;
        Ok(())
    }

    pub(super) fn call_mask_fun(&mut self, arg: CgValue<'comp, 'llvm>) -> IResult<()> {
        let size =
            self.dispatch_int_ret::<vtable::MaskFun>(arg, LayoutPart::Mask, &[arg.ptr.into()])?;
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
        globals: PointerValue<'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        self.dispatch_int_ret::<vtable::SkipFun>(
            arg,
            LayoutPart::Skip,
            &[arg.ptr.into(), count.into(), globals.into()],
        )
    }

    pub(super) fn call_span_fun(
        &mut self,
        ret: CgReturnValue<'comp, 'llvm>,
        start: CgValue<'comp, 'llvm>,
        end: CgValue<'comp, 'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        self.handle_vtable_copy(ret, start)?;
        self.dispatch_int_ret::<vtable::SpanFun>(
            start,
            LayoutPart::Span,
            &[
                ret.ptr.into(),
                start.ptr.into(),
                ret.head.into(),
                end.ptr.into(),
            ],
        )
    }

    pub(super) fn build_return_value(
        &mut self,
        val: CgValue<'comp, 'llvm>,
        globals: PointerValue<'llvm>,
    ) -> IResult<CgReturnValue<'comp, 'llvm>> {
        Ok(CgReturnValue::new(globals, val.ptr, val.layout))
    }

    pub(super) fn build_vtable_arg_set_offset_get(
        &mut self,
        fun: PointerValue<'llvm>,
        argnum: PSize,
    ) -> IResult<IntValue<'llvm>> {
        let vtable_ptr = self.read_vtable_pointer(fun)?;
        let arginfo_ty = vtable::ArgDescriptor::codegen_ty(self);
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
        let offset = self.build_i32_load(offset_ptr, "")?;
        Ok(offset)
    }

    pub(super) fn build_arg_set(
        &mut self,
        fun: CgValue<'comp, 'llvm>,
        arg: CgValue<'comp, 'llvm>,
        argnum: PSize,
        globals: PointerValue<'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        let offset = match fun.layout.maybe_mono() {
            Some(mono) => {
                let offset = self.arg_offset(mono, argnum);
                self.const_u32(offset)
            }
            None => self.build_vtable_arg_set_offset_get(fun.ptr, argnum)?,
        };
        let fun_any_ptr = self.build_cast::<*mut u8, _>(fun.ptr)?;
        let fun_arg_ptr = self.build_byte_gep(fun_any_ptr, offset, "")?;
        let fun_arg = CgReturnValue::new(
            globals,
            fun_arg_ptr,
            arg.layout.evaluate(self.layouts).unwrap().0,
        );
        self.call_deref_fun(fun_arg, arg)
    }

    pub(super) fn call_len_fun(
        &mut self,
        ret: CgReturnValue<'comp, 'llvm>,
        fun: CgValue<'comp, 'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        if ret.layout.is_multi() {
            let int_layout = self.layouts.dcx.int().maybe_mono().unwrap();
            let int_vtable = self.get_vtable_tag(int_layout);
            self.build_vtable_store(ret.ptr, int_vtable)?;
        }
        self.dispatch_int_ret::<vtable::LenFun>(
            fun,
            LayoutPart::Len,
            &[ret.ptr.into(), fun.ptr.into(), ret.head.into()],
        )
    }

    pub(super) fn build_vtable_get(
        &mut self,
        val: CgValue<'comp, 'llvm>,
    ) -> IResult<PointerValue<'llvm>> {
        match val.layout.maybe_mono() {
            Some(mono) => Ok(self.get_vtable_tag(mono)),
            None => self.read_vtable_pointer(val.ptr),
        }
    }

    pub(super) fn build_copy(
        &mut self,
        dest: CgReturnValue<'comp, 'llvm>,
        src: CgValue<'comp, 'llvm>,
    ) -> IResult<PointerValue<'llvm>> {
        self.handle_vtable_copy(dest, src)?;
        let sa = src.layout.size_align_without_vtable(self.layouts).unwrap();
        let size = self.const_i64(sa.total_size() as i64);
        let align = sa.start_alignment() as u32;
        self.builder
            .build_memcpy(dest.ptr, align, src.ptr, align, size)?;
        self.write_vtable_if_tagged(dest, src)?;
        Ok(dest.ptr)
    }

    pub(super) fn build_array_parser_get(
        &mut self,
        array: CgMonoValue<'comp, 'llvm>,
    ) -> IResult<CgValue<'comp, 'llvm>> {
        let (MonoLayout::Array { parser, .. }
        | MonoLayout::ArrayParser(Some((parser, _)))
        | MonoLayout::ArrayFillParser(Some((parser, _)))) = array.layout.mono_layout()
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
        let MonoLayout::Array { slice, .. } = array.layout.mono_layout() else {
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
