use inkwell::debug_info::AsDIScope;
use inkwell::values::{CallSiteValue, LLVMTailCallKind};
use yaboc_base::dbformat;
use yaboc_layout::collect::LCallReq;
use yaboc_layout::part::ParserFunKind;
use yaboc_layout::vtable::{
    ArrayVTable, BlockVTable, FunctionVTable, NominalVTable, NominalVTableFields, ParserVTable,
    VTableHeader,
};
use yaboc_layout::{LayoutSlice, vtable};
use yaboc_target::layout::VtablePointer;

use crate::debug::SubroutineDebugType;

use super::*;

pub const YABO_LIBRARY_INFO: &str = "_yabo_info";
pub const YABO_GLOBAL_INIT: &str = "_yabo_global_init";

#[derive(Clone, Copy)]
pub enum ArgLayout<'comp> {
    Arg(ILayout<'comp>),
    Return,
    Undef,
}

use ArgLayout::*;

impl<'llvm, 'comp> CodeGenCtx<'llvm, 'comp> {
    pub(crate) fn tailcc(&self) -> u32 {
        if self.options.target.use_tailcc {
            18
        } else {
            0
        }
    }
    pub(crate) fn set_tail_call(&self, call: CallSiteValue<'llvm>, tail: bool) {
        call.set_call_convention(self.tailcc());
        if tail && self.options.target.use_musttail {
            call.set_tail_call_kind(LLVMTailCallKind::LLVMTailCallKindMustTail);
        }
    }

    fn fun_val<const N: usize, F: FunctionTy + SubroutineDebugType<N>>(
        &mut self,
        layout: IMonoLayout<'comp>,
        part: LayoutPart<'comp>,
        arg_layouts: [ArgLayout<'comp>; N],
    ) -> FunctionValue<'llvm> {
        let sf_sym = self.sym(layout, part);
        if let Some(x) = self.module.get_function(&sf_sym) {
            return x;
        }
        let sf_type = F::fun_ty(self);
        let fun = self
            .module
            .add_function(&sf_sym, sf_type, Some(Linkage::Internal));
        fun.add_attribute(
            AttributeLoc::Function,
            self.llvm
                .create_enum_attribute(Attribute::get_named_enum_kind_id("nounwind"), 0),
        );
        let noalias = Attribute::get_named_enum_kind_id("noalias");
        let noalias_attr = self.llvm.create_enum_attribute(noalias, 0);
        for (i, ty) in sf_type.get_param_types().iter().enumerate() {
            if ty.is_pointer_type() {
                fun.add_attribute(AttributeLoc::Param(i as u32), noalias_attr);
            }
        }
        fun.as_global_value()
            .set_unnamed_address(UnnamedAddress::Global);
        if self.options.debug {
            let debug_info =
                self.debug
                    .location_data_or_default(layout, &self.compiler_database.db, self.llvm);
            let mut param_types = F::subroutine_ty(self).unwrap();
            let mut fundef = SmallVec::new();
            for (i, arg_layout) in arg_layouts.iter().enumerate() {
                let maybe_layout = match arg_layout {
                    ArgLayout::Arg(layout) => Some(*layout),
                    ArgLayout::Return => Some(self.return_layout(layout, part)),
                    ArgLayout::Undef => None,
                };
                fundef.push(maybe_layout);
                if let Some(layout) = maybe_layout {
                    let ty = self
                        .debug
                        .debug_type(layout, &self.compiler_database.db, self.llvm, self.layouts)
                        .unwrap();
                    let ptr_ty = self.debug.builder.create_pointer_type(
                        &dbformat!(&self.compiler_database.db, "ptr to {}", &layout),
                        ty,
                        self.options.target.data.pointer_sa.after_bits(),
                        self.options.target.data.pointer_sa.align_bits(),
                        AddressSpace::default(),
                    );
                    param_types.args[i] = ptr_ty.as_type();
                }
            }
            let subprogram_ty = param_types
                .as_subprogram_type(self, debug_info.file)
                .unwrap();
            let pretty_name = dbformat!(&self.compiler_database.db, "{} of {}", &part, &layout);
            let debuginfo = self.debug.builder.create_function(
                debug_info.file.as_debug_info_scope(),
                &pretty_name,
                Some(&sf_sym),
                debug_info.file,
                debug_info.line,
                subprogram_ty,
                true,
                true,
                debug_info.line,
                0,
                true,
            );
            self.fundefs.insert(fun, fundef);
            fun.set_subprogram(debuginfo);
        }
        fun
    }

    pub(super) fn single_forward_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> FunctionValue<'llvm> {
        self.fun_val::<_, vtable::SingleForwardFun>(
            layout,
            LayoutPart::SingleForward,
            [Arg(layout.inner()), Undef],
        )
    }

    pub(super) fn array_len_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.fun_val::<_, vtable::ArrayLenFun>(
            layout,
            LayoutPart::ArrayLen,
            [Arg(layout.inner()), Undef],
        )
    }

    pub(super) fn span_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        let arg = Arg(layout.inner());
        self.fun_val::<_, vtable::SpanFun>(layout, LayoutPart::Span, [arg, arg, Undef, arg])
    }

    pub(super) fn inner_array_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> FunctionValue<'llvm> {
        self.fun_val::<_, vtable::InnerArrayFun>(
            layout,
            LayoutPart::InnerArray,
            [Return, Arg(layout.inner()), Undef],
        )
    }

    pub(super) fn start_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.fun_val::<_, vtable::StartFun>(
            layout,
            LayoutPart::Start,
            [Return, Arg(layout.inner()), Undef],
        )
    }

    pub(super) fn end_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.fun_val::<_, vtable::EndFun>(
            layout,
            LayoutPart::End,
            [Return, Arg(layout.inner()), Undef],
        )
    }

    pub(super) fn current_element_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> FunctionValue<'llvm> {
        self.fun_val::<_, vtable::CurrentElementFun>(
            layout,
            LayoutPart::CurrentElement,
            [Return, Arg(layout.inner()), Undef],
        )
    }

    pub(super) fn skip_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.fun_val::<_, vtable::SkipFun>(
            layout,
            LayoutPart::Skip,
            [Arg(layout.inner()), Undef, Undef],
        )
    }

    pub(super) fn access_field_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        name: Identifier,
    ) -> FunctionValue<'llvm> {
        let f = self.fun_val::<_, vtable::BlockFieldFun>(
            layout,
            LayoutPart::Field(name),
            [Return, Arg(layout.inner()), Undef],
        );
        self.set_always_inline(f);
        f
    }

    pub(super) fn deref_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        let f = self.fun_val::<_, vtable::DerefFun>(
            layout,
            LayoutPart::Deref,
            [Return, Arg(layout.inner()), Undef],
        );
        self.set_always_inline(f);
        f
    }

    pub(super) fn parser_len_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> FunctionValue<'llvm> {
        self.fun_val::<_, vtable::LenFun>(
            layout,
            LayoutPart::Len,
            [Return, Arg(layout.inner()), Undef],
        )
    }

    pub(super) fn mask_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        // this function returns a usize instead of i64, so it's not a p_fun_val function
        self.fun_val::<_, vtable::MaskFun>(layout, LayoutPart::Mask, [Arg(layout.inner())])
    }

    pub(super) fn parser_layout_part(
        &mut self,
        from: ILayout<'comp>,
        req: LCallReq,
        kind: ParserFunKind,
    ) -> LayoutPart<'comp> {
        LayoutPart::Parse(req, kind, from)
    }

    pub(super) fn create_args_part(&mut self, from: LayoutSlice<'comp>) -> LayoutPart<'comp> {
        LayoutPart::CreateArgs(from)
    }

    pub(super) fn parser_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        from: ILayout<'comp>,
        req: LCallReq,
        kind: ParserFunKind,
    ) -> FunctionValue<'llvm> {
        let part = self.parser_layout_part(from, req, kind);
        let ret = self.fun_val::<_, vtable::ParserFun>(
            layout,
            part,
            [Return, Arg(layout.inner()), Undef, Arg(from)],
        );
        if kind != ParserFunKind::Wrapper {
            ret.set_call_conventions(self.tailcc());
        }
        ret
    }

    pub(super) fn parser_fun_val_wrapper(
        &mut self,
        layout: IMonoLayout<'comp>,
        from: ILayout<'comp>,
        req: LCallReq,
    ) -> FunctionValue<'llvm> {
        self.parser_fun_val(layout, from, req, ParserFunKind::Wrapper)
    }

    pub(super) fn parser_fun_val_tail(
        &mut self,
        layout: IMonoLayout<'comp>,
        from: ILayout<'comp>,
        req: LCallReq,
    ) -> FunctionValue<'llvm> {
        self.parser_fun_val(layout, from, req, ParserFunKind::TailWrapper)
    }

    pub(super) fn parser_impl_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        from: ILayout<'comp>,
        req: LCallReq,
    ) -> FunctionValue<'llvm> {
        self.parser_fun_val(layout, from, req, ParserFunKind::Worker)
    }

    pub(super) fn eval_fun_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        req: LCallReq,
        kind: ParserFunKind,
    ) -> FunctionValue<'llvm> {
        let is_tail = kind != ParserFunKind::Wrapper;
        let prototype = if is_tail {
            self.fun_val::<_, vtable::EvalFunFunInternal>(
                layout,
                LayoutPart::EvalFun(req, kind),
                [Return, Arg(layout.inner()), Undef, Undef],
            )
        } else {
            self.fun_val::<_, vtable::EvalFunFun>(
                layout,
                LayoutPart::EvalFun(req, kind),
                [Return, Arg(layout.inner()), Undef],
            )
        };
        if kind != ParserFunKind::Wrapper {
            prototype.set_call_conventions(self.tailcc());
        }
        prototype
    }

    pub(super) fn eval_fun_fun_val_wrapper(
        &mut self,
        layout: IMonoLayout<'comp>,
        req: LCallReq,
    ) -> FunctionValue<'llvm> {
        self.eval_fun_fun_val(layout, req, ParserFunKind::Wrapper)
    }

    pub(super) fn eval_fun_fun_val_tail(
        &mut self,
        layout: IMonoLayout<'comp>,
        req: LCallReq,
    ) -> FunctionValue<'llvm> {
        self.eval_fun_fun_val(layout, req, ParserFunKind::TailWrapper)
    }

    pub(super) fn eval_fun_fun_val_worker(
        &mut self,
        layout: IMonoLayout<'comp>,
        req: LCallReq,
    ) -> FunctionValue<'llvm> {
        self.eval_fun_fun_val(layout, req, ParserFunKind::Worker)
    }

    fn array_arg_level_and_offset(
        &mut self,
        layout: IMonoLayout<'comp>,
        argnum: u64,
    ) -> Option<u32> {
        match (layout.mono_layout(), argnum) {
            (MonoLayout::ArrayParser(Some((parser, _))), 1)
            | (MonoLayout::ArrayFillParser(Some((parser, _))), 0) => {
                let offset = parser.size_align(self.layouts).unwrap().next_offset(0);
                Some(offset as u32)
            }
            (MonoLayout::ArrayParser(Some((_, Some((len_layout, _))))), 0) => {
                let int_size = len_layout.size_align(self.layouts).unwrap().after;
                let whole_size = layout.inner().size_align(self.layouts).unwrap().after;
                Some((whole_size - int_size) as u32)
            }
            _ => None,
        }
    }

    pub(super) fn arg_offset(&mut self, layout: IMonoLayout<'comp>, argnum: PSize) -> u32 {
        if let Some(value) = self.array_arg_level_and_offset(layout, argnum) {
            return value;
        }
        let arg_defs = match layout.mono_layout() {
            MonoLayout::NominalParser(pd, _, _) => {
                let pd = pd.lookup(&self.compiler_database.db).unwrap();
                let arg_defs = pd.args.unwrap();
                arg_defs
            }
            MonoLayout::Lambda(lambda_id, _, _, ..) => {
                let lambda = lambda_id.lookup(&self.compiler_database.db).unwrap();
                let arg_defs = lambda.args;
                arg_defs
            }
            _ => {
                dbpanic!(
                    &self.compiler_database.db,
                    "trying to get parser set arg struct for non-nominal-parser layout {}",
                    &layout.inner()
                );
            }
        };
        let arg_index = arg_defs.len() - argnum as usize - 1;
        // needed so that the manifestation exists
        let _ = layout.inner().size_align(self.layouts).unwrap();
        let manifestation = self.layouts.dcx.manifestation(layout.inner());
        let arg_defid = arg_defs[arg_index];
        let field_offset = manifestation.field_offsets[&arg_defid.0] as u32;
        field_offset
    }

    pub(super) fn parser_set_arg_struct_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        argnum: PSize,
    ) -> StructValue<'llvm> {
        let offset = self.arg_offset(layout, argnum);
        let head = self.const_u32(0);
        let offset = self.const_u32(offset);
        self.llvm.const_struct(&[head.into(), offset.into()], false)
    }

    pub(super) fn function_create_args_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        from: LayoutSlice<'comp>,
    ) -> FunctionValue<'llvm> {
        let part = self.create_args_part(from);
        let f = self.fun_val::<_, vtable::CreateArgFun>(
            layout,
            part,
            [Return, Arg(layout.inner()), Undef],
        );
        self.set_always_inline(f);
        f
    }

    pub(super) fn layout_part_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        part: LayoutPart<'comp>,
    ) -> FunctionValue<'llvm> {
        match part {
            LayoutPart::Parse(req, kind, from) => self.parser_fun_val(layout, from, req, kind),
            LayoutPart::Field(name) => self.access_field_fun_val(layout, name),
            LayoutPart::Start => self.start_fun_val(layout),
            LayoutPart::End => self.end_fun_val(layout),
            LayoutPart::Deref => self.deref_fun_val(layout),
            LayoutPart::SingleForward => self.single_forward_fun_val(layout),
            LayoutPart::CurrentElement => self.current_element_fun_val(layout),
            LayoutPart::ArrayLen => self.array_len_fun_val(layout),
            LayoutPart::Skip => self.skip_fun_val(layout),
            LayoutPart::Span => self.span_fun_val(layout),
            LayoutPart::InnerArray => self.inner_array_fun_val(layout),
            LayoutPart::CreateArgs(from) => self.function_create_args_fun_val(layout, from),
            LayoutPart::Len => self.parser_len_fun_val(layout),
            LayoutPart::Mask => self.mask_fun_val(layout),
            LayoutPart::EvalFun(req, kind) => self.eval_fun_fun_val(layout, req, kind),
            LayoutPart::VTable => {
                panic!("{:?} is not a function part", part)
            }
        }
    }

    pub(super) fn layout_part_vtable_offset<Ptr: VtablePointer>(
        &self,
        layout: ILayout<'comp>,
        part: LayoutPart<'comp>,
    ) -> i64
    where
        ParserVTable<Ptr>: TargetSized,
        ArrayVTable<Ptr>: TargetSized,
        BlockVTable<Ptr>: TargetSized,
        NominalVTable<Ptr>: TargetSized,
        FunctionVTable<Ptr>: TargetSized,
        VTableHeader<Ptr>: TargetSized,
    {
        let data = &self.options.target.data;
        match part {
            LayoutPart::Parse(..) => {
                let slot = self.collected_layouts.slots.layout_vtable_offsets[&(part, layout)];
                <ParserVTable<Ptr>>::path_offset(
                    data,
                    &[ParserVTableFields::apply_table as i64, slot as i64],
                )
            }
            LayoutPart::Field(field) => {
                let Some(example_layout) = layout.into_iter().next() else {
                    dbpanic!(&self.compiler_database.db, "layout {} empty", &layout)
                };
                let MonoLayout::Block(block, _) = example_layout.mono_layout() else {
                    dbpanic!(&self.compiler_database.db, "layout {} not a block", &layout)
                };
                let index = self
                    .compiler_database
                    .db
                    .sorted_field_index(*block, FieldName::Ident(field), false)
                    .expect("failed to lookup field index")
                    .expect("could not find field");
                <BlockVTable<Ptr>>::path_offset(
                    data,
                    &[BlockVTableFields::access_impl as i64, index as i64],
                )
            }
            LayoutPart::VTable => 0,
            LayoutPart::Start => {
                <NominalVTable<Ptr>>::path_offset(data, &[NominalVTableFields::start_impl as i64])
            }
            LayoutPart::End => {
                <NominalVTable<Ptr>>::path_offset(data, &[NominalVTableFields::end_impl as i64])
            }
            LayoutPart::Deref => {
                <VTableHeader<Ptr>>::path_offset(data, &[VTableHeaderFields::deref_impl as i64])
            }
            LayoutPart::SingleForward => <ArrayVTable<Ptr>>::path_offset(
                data,
                &[ArrayVTableFields::single_forward_impl as i64],
            ),
            LayoutPart::CurrentElement => <ArrayVTable<Ptr>>::path_offset(
                data,
                &[ArrayVTableFields::current_element_impl as i64],
            ),
            LayoutPart::ArrayLen => {
                <ArrayVTable<Ptr>>::path_offset(data, &[ArrayVTableFields::len_impl as i64])
            }
            LayoutPart::Skip => {
                <ArrayVTable<Ptr>>::path_offset(data, &[ArrayVTableFields::skip_impl as i64])
            }
            LayoutPart::Span => {
                <ArrayVTable<Ptr>>::path_offset(data, &[ArrayVTableFields::span_impl as i64])
            }
            LayoutPart::InnerArray => {
                <ArrayVTable<Ptr>>::path_offset(data, &[ArrayVTableFields::inner_array_impl as i64])
            }
            LayoutPart::EvalFun(..) | LayoutPart::CreateArgs(_) => {
                let slot = self.collected_layouts.slots.layout_vtable_offsets[&(part, layout)];
                <FunctionVTable<Ptr>>::path_offset(
                    data,
                    &[FunctionVTableFields::apply_table as i64, slot as i64],
                )
            }
            LayoutPart::Len => {
                <ParserVTable<Ptr>>::path_offset(data, &[ParserVTableFields::len_impl as i64])
            }
            LayoutPart::Mask => {
                <VTableHeader<Ptr>>::path_offset(data, &[VTableHeaderFields::mask_impl as i64])
            }
        }
    }

    pub(super) fn global_constant_init_fun_val(&mut self) -> FunctionValue<'llvm> {
        if let Some(x) = self.module.get_function(YABO_GLOBAL_INIT) {
            return x;
        }
        let fun_type = self
            .llvm
            .i64_type()
            .fn_type(&[self.any_ptr().into(); 3], false);
        let f = self
            .module
            .add_function(YABO_GLOBAL_INIT, fun_type, Some(Linkage::Internal));
        f
    }
}
