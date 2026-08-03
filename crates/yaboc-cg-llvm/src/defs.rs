use inkwell::debug_info::AsDIScope;
use inkwell::values::{CallSiteValue, LLVMTailCallKind};
use yaboc_base::dbformat;
use yaboc_hir_types::VTABLE_BIT;
use yaboc_layout::represent::ParserFunKind;
use yaboc_layout::vtable;

use crate::debug::SubroutineDebugType;

use super::*;

pub const YABO_GLOBAL_INIT: &str = "yabo_global_init";
pub const YABO_GLOBAL_SIZE: &str = "yabo_global_size";
pub const YABO_MAX_BUF_SIZE: &str = "yabo_max_buf_size";

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
        part: LayoutPart,
        arg_layouts: [Option<ILayout<'comp>>; N],
    ) -> FunctionValue<'llvm> {
        let sf_sym = self.sym(layout, part);
        if let Some(x) = self.module.get_function(&sf_sym) {
            return x;
        }
        let sf_type = F::fun_ty(self);
        let fun = self
            .module
            .add_function(&sf_sym, sf_type, Some(Linkage::Internal));
        let noalias = Attribute::get_named_enum_kind_id("noalias");
        let noalias_attr = self.llvm.create_enum_attribute(noalias, 0);
        for (i, ty) in sf_type.get_param_types().iter().enumerate() {
            if ty.is_pointer_type() {
                fun.add_attribute(AttributeLoc::Param(i as u32), noalias_attr);
            }
        }
        if self.options.asan {
            let sanitize_address = Attribute::get_named_enum_kind_id("sanitize_address");
            let sanitize_address_attr = self.llvm.create_enum_attribute(sanitize_address, 1);
            fun.add_attribute(AttributeLoc::Function, sanitize_address_attr);
        }
        if self.options.msan {
            let sanitize_memory = Attribute::get_named_enum_kind_id("sanitize_memory");
            let sanitize_memory_attr = self.llvm.create_enum_attribute(sanitize_memory, 1);
            fun.add_attribute(AttributeLoc::Function, sanitize_memory_attr);
        }
        fun.as_global_value()
            .set_unnamed_address(UnnamedAddress::Global);
        if self.options.debug {
            let debug_info =
                self.debug
                    .location_data_or_default(layout, &self.compiler_database.db, self.llvm);
            let mut param_types = F::subroutine_ty(self).unwrap();
            for (i, layout) in arg_layouts.iter().enumerate() {
                if let Some(layout) = layout {
                    let ty = self
                        .debug
                        .debug_type(*layout, &self.compiler_database.db, self.llvm, self.layouts)
                        .unwrap();
                    let ptr_ty = self.debug.builder.create_pointer_type(
                        &dbformat!(&self.compiler_database.db, "ptr to {}", layout),
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
            let debuginfo = self.debug.builder.create_function(
                debug_info.file.as_debug_info_scope(),
                &sf_sym,
                None,
                debug_info.file,
                debug_info.line,
                subprogram_ty,
                true,
                true,
                debug_info.line,
                0,
                true,
            );
            fun.set_subprogram(debuginfo);
        }
        self.fundefs.insert(fun, SmallVec::from_slice(&arg_layouts));
        fun
    }

    pub(super) fn single_forward_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> FunctionValue<'llvm> {
        self.fun_val::<_, vtable::SingleForwardFun>(
            layout,
            LayoutPart::SingleForward,
            [Some(layout.inner()), None],
        )
    }

    pub(super) fn array_len_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.fun_val::<_, vtable::ArrayLenFun>(
            layout,
            LayoutPart::ArrayLen,
            [Some(layout.inner()), None],
        )
    }

    pub(super) fn span_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        let arg = Some(layout.inner());
        self.fun_val::<_, vtable::SpanFun>(layout, LayoutPart::Span, [arg, arg, None, arg])
    }

    pub(super) fn inner_array_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> FunctionValue<'llvm> {
        self.fun_val::<_, vtable::InnerArrayFun>(
            layout,
            LayoutPart::InnerArray,
            [None, Some(layout.inner()), None],
        )
    }

    pub(super) fn start_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.fun_val::<_, vtable::StartFun>(
            layout,
            LayoutPart::Start,
            [None, Some(layout.inner()), None],
        )
    }

    pub(super) fn end_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.fun_val::<_, vtable::EndFun>(
            layout,
            LayoutPart::End,
            [None, Some(layout.inner()), None],
        )
    }

    pub(super) fn current_element_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> FunctionValue<'llvm> {
        self.fun_val::<_, vtable::CurrentElementFun>(
            layout,
            LayoutPart::CurrentElement,
            [None, Some(layout.inner()), None],
        )
    }

    pub(super) fn skip_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.fun_val::<_, vtable::SkipFun>(
            layout,
            LayoutPart::Skip,
            [Some(layout.inner()), None, None],
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
            [None, Some(layout.inner()), None],
        );
        self.set_always_inline(f);
        f
    }

    pub(super) fn typecast_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        let f = self.fun_val::<_, vtable::TypecastFun>(
            layout,
            LayoutPart::Typecast,
            [None, Some(layout.inner()), None],
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
            [None, Some(layout.inner()), None],
        )
    }

    pub(super) fn mask_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        // this function returns a usize instead of i64, so it's not a p_fun_val function
        self.fun_val::<_, vtable::MaskFun>(layout, LayoutPart::Mask, [Some(layout.inner())])
    }

    pub(super) fn parser_layout_part(
        &mut self,
        from: ILayout<'comp>,
        req: RequirementSet,
        kind: ParserFunKind,
    ) -> LayoutPart {
        let hash = self.layouts.dcx.layout_hash(self.layouts.db, from);
        LayoutPart::Parse(req, kind, hash)
    }

    pub(super) fn create_args_part(&mut self, from: &[ILayout<'comp>]) -> LayoutPart {
        let hash = self.layouts.dcx.layout_slice_hash(self.layouts.db, from);
        LayoutPart::CreateArgs(hash)
    }

    pub(super) fn parser_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        from: ILayout<'comp>,
        req: RequirementSet,
        kind: ParserFunKind,
    ) -> FunctionValue<'llvm> {
        let part = self.parser_layout_part(from, req, kind);
        let ret = self.fun_val::<_, vtable::ParserFun>(
            layout,
            part,
            [None, Some(layout.inner()), None, Some(from)],
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
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        self.parser_fun_val(layout, from, req, ParserFunKind::Wrapper)
    }

    pub(super) fn parser_fun_val_tail(
        &mut self,
        layout: IMonoLayout<'comp>,
        from: ILayout<'comp>,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        self.parser_fun_val(layout, from, req, ParserFunKind::TailWrapper)
    }

    pub(super) fn parser_impl_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        from: ILayout<'comp>,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        self.parser_fun_val(layout, from, req, ParserFunKind::Worker)
    }

    pub(super) fn eval_fun_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        req: RequirementSet,
        kind: ParserFunKind,
    ) -> FunctionValue<'llvm> {
        let is_tail = kind != ParserFunKind::Wrapper;
        let prototype = if is_tail {
            self.fun_val::<_, vtable::EvalFunFunInternal>(
                layout,
                LayoutPart::EvalFun(req, kind),
                [None, Some(layout.inner()), None, None],
            )
        } else {
            self.fun_val::<_, vtable::EvalFunFun>(
                layout,
                LayoutPart::EvalFun(req, kind),
                [None, Some(layout.inner()), None],
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
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        self.eval_fun_fun_val(layout, req, ParserFunKind::Wrapper)
    }

    pub(super) fn eval_fun_fun_val_tail(
        &mut self,
        layout: IMonoLayout<'comp>,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        self.eval_fun_fun_val(layout, req, ParserFunKind::TailWrapper)
    }

    pub(super) fn eval_fun_fun_val_worker(
        &mut self,
        layout: IMonoLayout<'comp>,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        self.eval_fun_fun_val(layout, req, ParserFunKind::Worker)
    }

    fn array_arg_level_and_offset(
        &mut self,
        layout: IMonoLayout<'comp>,
        argnum: u64,
    ) -> Option<(i64, u64)> {
        match (layout.mono_layout(), argnum) {
            (MonoLayout::ArrayParser(Some((parser, _))), 1)
            | (MonoLayout::ArrayFillParser(Some((parser, _))), 0) => {
                let offset = parser.size_align(self.layouts).unwrap().next_offset(0);
                Some(((parser.is_multi() as i64) << VTABLE_BIT, offset))
            }
            (MonoLayout::ArrayParser(Some((_, Some((len_layout, _))))), 0) => {
                let int_size = len_layout.size_align(self.layouts).unwrap().after;
                let whole_size = layout.inner().size_align(self.layouts).unwrap().after;
                Some((0, whole_size - int_size))
            }
            _ => None,
        }
    }

    pub(super) fn arg_level_and_offset(
        &mut self,
        layout: IMonoLayout<'comp>,
        argnum: PSize,
    ) -> (i64, PSize) {
        if let Some(value) = self.array_arg_level_and_offset(layout, argnum) {
            return value;
        }
        let (arg_defs, args) = match layout.mono_layout() {
            MonoLayout::NominalParser(pd, args, _) => {
                let pd = pd.lookup(&self.compiler_database.db).unwrap();
                let arg_defs = pd.args.unwrap();
                (arg_defs, args)
            }
            MonoLayout::Lambda(lambda_id, _, args, ..) => {
                let lambda = lambda_id.lookup(&self.compiler_database.db).unwrap();
                let arg_defs = lambda.args;
                (arg_defs, args)
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
        let arg_layout = args[arg_index];
        let is_multi = arg_layout.is_multi();
        let head = is_multi as i64;
        // needed so that the manifestation exists
        let _ = layout.inner().size_align(self.layouts).unwrap();
        let manifestation = self.layouts.dcx.manifestation(layout.inner());
        let arg_defid = arg_defs[arg_index];
        let mut field_offset = manifestation.field_offsets[&arg_defid.0];
        if is_multi {
            field_offset += arg_layout
                .size_align(self.layouts)
                .unwrap()
                .allocation_center_offset();
        }
        (head, field_offset)
    }

    pub(super) fn parser_set_arg_struct_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        argnum: PSize,
    ) -> StructValue<'llvm> {
        let (head, offset) = self.arg_level_and_offset(layout, argnum);
        let head = self.const_i64(head);
        let offset = self.const_size_t(offset as i64);
        self.llvm.const_struct(&[head.into(), offset.into()], false)
    }

    pub(super) fn parser_impl_struct_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        from: ILayout<'comp>,
        info: CallMeta,
    ) -> PointerValue<'llvm> {
        if info.tail {
            self.parser_fun_val_tail(layout, from, info.req)
                .as_global_value()
                .as_pointer_value()
        } else {
            self.parser_fun_val_wrapper(layout, from, info.req)
                .as_global_value()
                .as_pointer_value()
        }
    }

    pub(super) fn function_create_args_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        from: &[ILayout<'comp>],
    ) -> FunctionValue<'llvm> {
        let part = self.create_args_part(from);
        let f = self.fun_val::<_, vtable::CreateArgFun>(
            layout,
            part,
            [None, Some(layout.inner()), None],
        );
        self.set_always_inline(f);
        f
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
            .add_function(YABO_GLOBAL_INIT, fun_type, Some(Linkage::External));
        f.as_global_value()
            .set_visibility(GlobalVisibility::Protected);
        f
    }
}
