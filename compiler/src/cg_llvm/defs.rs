use super::*;

impl<'llvm, 'comp> CodeGenCtx<'llvm, 'comp> {
    fn fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        part: LayoutPart,
        types: &[BasicMetadataTypeEnum<'llvm>],
    ) -> FunctionValue<'llvm> {
        let sf_sym = self.sym(layout, part);
        if let Some(x) = self.module.get_function(&sf_sym) {
            return x;
        }
        let sf_type = self.llvm.i64_type().fn_type(types, false);
        let fun = self
            .module
            .add_function(&sf_sym, sf_type, Some(Linkage::External));
        fun.as_global_value()
            .set_unnamed_address(UnnamedAddress::Global);
        fun.as_global_value()
            .set_visibility(GlobalVisibility::Hidden);
        fun
    }

    fn pp_fun_val(&mut self, layout: IMonoLayout<'comp>, part: LayoutPart) -> FunctionValue<'llvm> {
        self.fun_val(
            layout,
            part,
            &[self.any_ptr().into(), self.any_ptr().into()],
        )
    }

    fn pip_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        part: LayoutPart,
    ) -> FunctionValue<'llvm> {
        self.fun_val(
            layout,
            part,
            &[
                self.any_ptr().into(),
                self.llvm.i64_type().into(),
                self.any_ptr().into(),
            ],
        )
    }

    fn ppp_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        part: LayoutPart,
    ) -> FunctionValue<'llvm> {
        self.fun_val(
            layout,
            part,
            &[
                self.any_ptr().into(),
                self.any_ptr().into(),
                self.any_ptr().into(),
            ],
        )
    }

    fn ppip_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        part: LayoutPart,
    ) -> FunctionValue<'llvm> {
        self.fun_val(
            layout,
            part,
            &[
                self.any_ptr().into(),
                self.any_ptr().into(),
                self.llvm.i64_type().into(),
                self.any_ptr().into(),
            ],
        )
    }

    pub(super) fn single_forward_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pp_fun_val(layout, LayoutPart::SingleForward)
    }

    pub(super) fn start_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pp_fun_val(layout, LayoutPart::Start)
    }

    pub(super) fn end_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pp_fun_val(layout, LayoutPart::End)
    }

    pub(super) fn current_element_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pip_fun_val(layout, LayoutPart::CurrentElement)
    }

    pub(super) fn skip_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pip_fun_val(layout, LayoutPart::Skip)
    }

    pub(super) fn access_field_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        name: Identifier,
    ) -> FunctionValue<'llvm> {
        let f = self.pip_fun_val(layout, LayoutPart::Field(name));
        self.set_always_inline(f);
        f
    }

    pub(super) fn typecast_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        let f = self.pip_fun_val(layout, LayoutPart::Typecast);
        self.set_always_inline(f);
        f
    }

    pub(super) fn parser_len_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        slot: PSize,
    ) -> FunctionValue<'llvm> {
        self.ppp_fun_val(layout, LayoutPart::LenImpl(slot))
    }

    pub(super) fn deref_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pip_fun_val(layout, LayoutPart::Deref(true))
    }

    pub(super) fn deref_impl_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pp_fun_val(layout, LayoutPart::Deref(false))
    }

    pub(super) fn parser_val_impl_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        slot: PSize,
    ) -> FunctionValue<'llvm> {
        self.ppp_fun_val(layout, LayoutPart::ValImpl(slot, false))
    }

    pub(super) fn parser_val_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        slot: PSize,
    ) -> FunctionValue<'llvm> {
        self.ppip_fun_val(layout, LayoutPart::ValImpl(slot, true))
    }

    pub(super) fn arg_disc_and_offset(&mut self, layout: IMonoLayout<'comp>, argnum: PSize) -> (i64, PSize) {
        let (pd, args) = if let MonoLayout::NominalParser(pd, args) = layout.mono_layout().0 {
            (*pd, args)
        } else {
            panic!("trying to get parser set arg struct for non-nominal-parser layout");
        };
        let parserdef_args = pd.lookup(&self.compiler_database.db).unwrap().args.unwrap();
        let arg_index = parserdef_args.len() - argnum as usize - 1;
        let (arg_layout, ty) = args[arg_index];
        let head = self.compiler_database.db.head_discriminant(ty)
            | matches!(&arg_layout.layout, Layout::Multi(_)) as i64;
        // needed so that the manifestation exists
        let _ = layout.inner().size_align(self.layouts).unwrap();
        let manifestation = self.layouts.dcx.manifestation(layout.inner());
        let arg_defid = parserdef_args[arg_index];
        let field_offset = manifestation.field_offsets[&arg_defid.0];
        (head, field_offset)
    }

    pub(super) fn parser_set_arg_struct_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        argnum: PSize,
    ) -> StructValue<'llvm> {
        let (head, offset) = self.arg_disc_and_offset(layout, argnum);
        let head = self.const_i64(head);
        let offset = self.const_size_t(offset);
        self.llvm.const_struct(&[head.into(), offset.into()], false)
    }

    pub(super) fn parser_impl_struct_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        slot: PSize,
        is_non_null: bool,
    ) -> StructValue<'llvm> {
        let (len, val) = if is_non_null {
            let len_fn_ptr = self
                .parser_len_fun_val(layout, slot)
                .as_global_value()
                .as_pointer_value();
            let val_fn_ptr = self
                .parser_val_fun_val(layout, slot)
                .as_global_value()
                .as_pointer_value();
            (len_fn_ptr, val_fn_ptr)
        } else {
            let len_fn_null_ptr = <fn(*const u8, *const u8, *mut u8) -> i64>::codegen_ty(self)
                .into_pointer_type()
                .const_null();
            let val_fn_null_ptr = <fn(*const u8, *const u8, i64, *mut u8) -> i64>::codegen_ty(self)
                .into_pointer_type()
                .const_null();
            (len_fn_null_ptr, val_fn_null_ptr)
        };
        self.llvm.const_struct(&[val.into(), len.into()], false)
    }

    pub(super) fn function_create_args_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        slot: PSize,
    ) -> FunctionValue<'llvm> {
        let f = self.pip_fun_val(layout, LayoutPart::CreateArgs(slot));
        self.set_always_inline(f);
        f
    }
}