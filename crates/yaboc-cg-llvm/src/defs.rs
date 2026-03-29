use inkwell::values::{CallSiteValue, LLVMTailCallKind};
use yaboc_hir_types::VTABLE_BIT;
use yaboc_layout::represent::ParserFunKind;
use yaboc_layout::vtable;

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

    fn fun_val<F: FunctionTy>(
        &mut self,
        layout: IMonoLayout<'comp>,
        part: LayoutPart,
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
        fun
    }

    pub(super) fn single_forward_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> FunctionValue<'llvm> {
        self.fun_val::<vtable::SingleForwardFun>(layout, LayoutPart::SingleForward)
    }

    pub(super) fn array_len_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.fun_val::<vtable::ArrayLenFun>(layout, LayoutPart::ArrayLen)
    }

    pub(super) fn span_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.fun_val::<vtable::SpanFun>(layout, LayoutPart::Span)
    }

    pub(super) fn inner_array_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> FunctionValue<'llvm> {
        self.fun_val::<vtable::InnerArrayFun>(layout, LayoutPart::InnerArray)
    }

    pub(super) fn start_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.fun_val::<vtable::StartFun>(layout, LayoutPart::Start)
    }

    pub(super) fn end_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.fun_val::<vtable::EndFun>(layout, LayoutPart::End)
    }

    pub(super) fn current_element_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> FunctionValue<'llvm> {
        self.fun_val::<vtable::CurrentElementFun>(layout, LayoutPart::CurrentElement)
    }

    pub(super) fn skip_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.fun_val::<vtable::SkipFun>(layout, LayoutPart::Skip)
    }

    pub(super) fn access_field_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        name: Identifier,
    ) -> FunctionValue<'llvm> {
        let f = self.fun_val::<vtable::BlockFieldFun>(layout, LayoutPart::Field(name));
        self.set_always_inline(f);
        f
    }

    pub(super) fn typecast_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        let f = self.fun_val::<vtable::TypecastFun>(layout, LayoutPart::Typecast);
        self.set_always_inline(f);
        f
    }

    pub(super) fn parser_len_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> FunctionValue<'llvm> {
        self.fun_val::<vtable::LenFun>(layout, LayoutPart::Len)
    }

    pub(super) fn mask_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        // this function returns a usize instead of i64, so it's not a p_fun_val function
        self.fun_val::<vtable::MaskFun>(layout, LayoutPart::Mask)
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

    pub(super) fn parser_fun_val_wrapper(
        &mut self,
        layout: IMonoLayout<'comp>,
        from: ILayout<'comp>,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        let part = self.parser_layout_part(from, req, ParserFunKind::Wrapper);
        let ret = self.fun_val::<vtable::ParserFun>(layout, part);
        ret
    }

    pub(super) fn parser_fun_val_tail(
        &mut self,
        layout: IMonoLayout<'comp>,
        from: ILayout<'comp>,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        let part = self.parser_layout_part(from, req, ParserFunKind::TailWrapper);
        let ret = self.fun_val::<vtable::ParserFun>(layout, part);
        ret.set_call_conventions(self.tailcc());
        ret
    }

    pub(super) fn parser_impl_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        from: ILayout<'comp>,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        let part = self.parser_layout_part(from, req, ParserFunKind::Worker);
        let ret = self.fun_val::<vtable::ParserFun>(layout, part);
        ret.set_call_conventions(self.tailcc());
        ret
    }

    pub(super) fn eval_fun_fun_val_wrapper(
        &mut self,
        layout: IMonoLayout<'comp>,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        let ret = self.fun_val::<vtable::EvalFunFun>(
            layout,
            LayoutPart::EvalFun(req, ParserFunKind::Wrapper),
        );
        ret
    }

    pub(super) fn eval_fun_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        let ret = self
            .fun_val::<vtable::EvalFunFun>(layout, LayoutPart::EvalFun(req, ParserFunKind::Worker));
        ret.set_call_conventions(self.tailcc());
        ret
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
        slot: PSize,
    ) -> FunctionValue<'llvm> {
        let f = self.fun_val::<vtable::CreateArgFun>(layout, LayoutPart::CreateArgs(slot));
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
