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

    fn p_fun_val(&mut self, layout: IMonoLayout<'comp>, part: LayoutPart) -> FunctionValue<'llvm> {
        self.fun_val(layout, part, &[self.any_ptr().into()])
    }

    fn ppi_fun_val(
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
            ],
        )
    }

    fn pi_fun_val(&mut self, layout: IMonoLayout<'comp>, part: LayoutPart) -> FunctionValue<'llvm> {
        self.fun_val(
            layout,
            part,
            &[self.any_ptr().into(), self.llvm.i64_type().into()],
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

    pub(super) fn single_forward_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> FunctionValue<'llvm> {
        self.p_fun_val(layout, LayoutPart::SingleForward)
    }

    pub(super) fn span_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.ppip_fun_val(layout, LayoutPart::Span)
    }

    pub(super) fn start_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pp_fun_val(layout, LayoutPart::Start)
    }

    pub(super) fn end_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pp_fun_val(layout, LayoutPart::End)
    }

    pub(super) fn current_element_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> FunctionValue<'llvm> {
        self.ppi_fun_val(layout, LayoutPart::CurrentElement)
    }

    pub(super) fn skip_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pi_fun_val(layout, LayoutPart::Skip)
    }

    pub(super) fn access_field_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        name: Identifier,
    ) -> FunctionValue<'llvm> {
        let f = self.ppi_fun_val(layout, LayoutPart::Field(name));
        self.set_always_inline(f);
        f
    }

    pub(super) fn typecast_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        let f = self.ppi_fun_val(layout, LayoutPart::Typecast);
        self.set_always_inline(f);
        f
    }

    pub(super) fn parser_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        slot: PSize,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        self.ppip_fun_val(layout, LayoutPart::Parse(slot, req, true))
    }

    pub(super) fn parser_impl_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        slot: PSize,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        self.ppip_fun_val(layout, LayoutPart::Parse(slot, req, false))
    }

    pub(super) fn arg_level_and_offset(
        &mut self,
        layout: IMonoLayout<'comp>,
        argnum: PSize,
    ) -> (i64, PSize) {
        if let MonoLayout::ArrayParser(_) = layout.mono_layout().0 {
            assert!(argnum == 0);
            return (0, 0);
        }
        let (pd, args) = if let MonoLayout::NominalParser(pd, args, _) = layout.mono_layout().0 {
            (*pd, args)
        } else {
            dbpanic!(
                &self.compiler_database.db,
                "trying to get parser set arg struct for non-nominal-parser layout {}",
                &layout.inner()
            );
        };
        let parserdef_args = pd.lookup(&self.compiler_database.db).unwrap().args.unwrap();
        let arg_index = parserdef_args.len() - argnum as usize - 1;
        let (arg_layout, ty) = args[arg_index];
        let is_multi = matches!(&arg_layout.layout.1, Layout::Multi(_));
        let head = self
            .compiler_database
            .db
            .deref_level(ty)
            .unwrap()
            .into_shifted_runtime_value() as i64
            | is_multi as i64;
        // needed so that the manifestation exists
        let _ = layout.inner().size_align(self.layouts).unwrap();
        let manifestation = self.layouts.dcx.manifestation(layout.inner());
        let arg_defid = parserdef_args[arg_index];
        let mut field_offset = manifestation.field_offsets[&arg_defid.0];
        if is_multi {
            field_offset += self.word_size();
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
        slot: PSize,
        req: RequirementSet,
        is_non_null: bool,
    ) -> PointerValue<'llvm> {
        if is_non_null {
            self.parser_fun_val(layout, slot, req)
                .as_global_value()
                .as_pointer_value()
        } else {
            ParserFun::codegen_ty(self).into_pointer_type().const_null()
        }
    }

    pub(super) fn function_create_args_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        slot: PSize,
    ) -> FunctionValue<'llvm> {
        let f = self.ppi_fun_val(layout, LayoutPart::CreateArgs(slot));
        self.set_always_inline(f);
        f
    }
}
