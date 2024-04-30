use inkwell::values::{CallSiteValue, LLVMTailCallKind};
use yaboc_base::dbformat;
use yaboc_hir_types::VTABLE_BIT;
use yaboc_layout::represent::ParserFunKind;
use yaboc_resolve::Resolves;

use super::*;

pub const YABO_GLOBAL_ADDRESS: &str = "yabo_global_address";
pub const YABO_GLOBAL_INIT: &str = "yabo_global_init";

impl<'llvm, 'comp> CodeGenCtx<'llvm, 'comp> {
    pub(crate) fn tailcc(&self) -> u32 {
        if self.yabo_target.use_tailcc {
            18
        } else {
            0
        }
    }
    pub(crate) fn set_tail_call(&self, call: CallSiteValue<'llvm>, tail: bool) {
        call.set_call_convention(self.tailcc());
        if tail && self.yabo_target.use_musttail {
            call.set_tail_call_kind(LLVMTailCallKind::LLVMTailCallKindMustTail);
        }
    }
    fn fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        part: LayoutPart,
        types: &[BasicMetadataTypeEnum<'llvm>],
        ret_ty: BasicTypeEnum<'llvm>,
    ) -> FunctionValue<'llvm> {
        let sf_sym = self.sym(layout, part);
        if let Some(x) = self.module.get_function(&sf_sym) {
            return x;
        }
        let sf_type = ret_ty.fn_type(types, false);
        let fun = self
            .module
            .add_function(&sf_sym, sf_type, Some(Linkage::Internal));
        let noalias = Attribute::get_named_enum_kind_id("noalias");
        let noalias_attr = self.llvm.create_enum_attribute(noalias, 0);
        for (i, ty) in types.iter().enumerate() {
            if ty.is_pointer_type() {
                fun.add_attribute(AttributeLoc::Param(i as u32), noalias_attr);
            }
        }
        fun.as_global_value()
            .set_unnamed_address(UnnamedAddress::Global);
        fun
    }

    fn pp_fun_val(&mut self, layout: IMonoLayout<'comp>, part: LayoutPart) -> FunctionValue<'llvm> {
        self.fun_val(
            layout,
            part,
            &[self.any_ptr().into(), self.any_ptr().into()],
            self.llvm.i64_type().into(),
        )
    }

    fn p_fun_val(&mut self, layout: IMonoLayout<'comp>, part: LayoutPart) -> FunctionValue<'llvm> {
        self.fun_val(
            layout,
            part,
            &[self.any_ptr().into()],
            self.llvm.i64_type().into(),
        )
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
            self.llvm.i64_type().into(),
        )
    }

    fn pi_fun_val(&mut self, layout: IMonoLayout<'comp>, part: LayoutPart) -> FunctionValue<'llvm> {
        self.fun_val(
            layout,
            part,
            &[self.any_ptr().into(), self.llvm.i64_type().into()],
            self.llvm.i64_type().into(),
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
            self.llvm.i64_type().into(),
        )
    }

    pub(super) fn single_forward_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> FunctionValue<'llvm> {
        self.p_fun_val(layout, LayoutPart::SingleForward)
    }

    pub(super) fn array_len_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.p_fun_val(layout, LayoutPart::ArrayLen)
    }

    pub(super) fn span_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.ppip_fun_val(layout, LayoutPart::Span)
    }

    pub(super) fn inner_array_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> FunctionValue<'llvm> {
        self.ppi_fun_val(layout, LayoutPart::InnerArray)
    }

    pub(super) fn start_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.ppi_fun_val(layout, LayoutPart::Start)
    }

    pub(super) fn end_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.ppi_fun_val(layout, LayoutPart::End)
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

    pub(super) fn parser_len_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> FunctionValue<'llvm> {
        self.pp_fun_val(layout, LayoutPart::Len)
    }

    pub(super) fn mask_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        // this function returns a usize instead of i64, so it's not a p_fun_val function
        let size_t = self.llvm.ptr_sized_int_type(&self.target_data, None);
        self.fun_val(
            layout,
            LayoutPart::Mask,
            &[self.any_ptr().into()],
            size_t.into(),
        )
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
        let ret = self.ppip_fun_val(layout, part);
        ret
    }

    pub(super) fn parser_fun_val_tail(
        &mut self,
        layout: IMonoLayout<'comp>,
        from: ILayout<'comp>,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        let part = self.parser_layout_part(from, req, ParserFunKind::TailWrapper);
        let ret = self.ppip_fun_val(layout, part);
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
        let ret = self.ppip_fun_val(layout, part);
        ret.set_call_conventions(self.tailcc());
        ret
    }

    pub(super) fn eval_fun_fun_val_wrapper(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> FunctionValue<'llvm> {
        let ret = self.ppi_fun_val(layout, LayoutPart::EvalFun(ParserFunKind::Wrapper));
        ret
    }

    pub(super) fn eval_fun_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        let ret = self.ppi_fun_val(layout, LayoutPart::EvalFun(ParserFunKind::Worker));
        ret.set_call_conventions(self.tailcc());
        ret
    }

    fn array_arg_level_and_offset(
        &mut self,
        layout: IMonoLayout<'comp>,
        argnum: u64,
    ) -> Option<(i64, u64)> {
        if let MonoLayout::ArrayParser(s) = layout.mono_layout().0 {
            let Some((parser, maybe_len)) = s else {
                dbpanic!(
                    &self.compiler_database.db,
                    "trying to get arg struct for non-array parser layout {}",
                    &layout.inner()
                );
            };
            if argnum == 1 {
                let offset = parser.size_align(self.layouts).unwrap().next_offset(0);
                return Some(((parser.is_multi() as i64) << VTABLE_BIT, offset));
            }
            assert!(argnum == 0);
            let Some(len_layout) = maybe_len else {
                dbpanic!(
                    &self.compiler_database.db,
                    "trying to non-existent int arg struct for array parser layout {}",
                    &layout.inner()
                );
            };
            let int_size = len_layout.size_align(self.layouts).unwrap().after;
            let whole_size = layout.inner().size_align(self.layouts).unwrap().after;
            return Some((0, whole_size - int_size));
        }
        None
    }

    pub(super) fn arg_level_and_offset(
        &mut self,
        layout: IMonoLayout<'comp>,
        argnum: PSize,
    ) -> (i64, PSize) {
        if let Some(value) = self.array_arg_level_and_offset(layout, argnum) {
            return value;
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
        let is_multi = arg_layout.is_multi();
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
        let f = self.ppi_fun_val(layout, LayoutPart::CreateArgs(slot));
        self.set_always_inline(f);
        f
    }

    pub(super) fn yabo_global_address(&mut self) -> CgMonoValue<'comp, 'llvm> {
        let ptr = if let Some(x) = self.module.get_global(YABO_GLOBAL_ADDRESS) {
            x.as_pointer_value()
        } else {
            let llvm_ty = <[*const u8; 2]>::codegen_ty(self);
            let global = self.module.add_global(llvm_ty, None, YABO_GLOBAL_ADDRESS);
            global.set_initializer(&llvm_ty.const_zero());
            global.as_pointer_value()
        };
        let u8_ptr = ptr.const_cast(self.any_ptr());
        let layout = IMonoLayout::u8_array(self.layouts);
        CgMonoValue::new(layout, u8_ptr)
    }

    pub(super) fn global_constant(&mut self, pd: ParserDefId) -> PointerValue<'llvm> {
        let layout = self.collected_layouts.globals[&pd].1;
        let hash = truncated_hex(&self.compiler_database.db.def_hash(pd.0));
        let name = pd.0.unwrap_name(&self.compiler_database.db);
        let sym = dbformat!(&self.compiler_database.db, "g${}${}", &hash, &name);
        let sa = layout.size_align(self.layouts).unwrap();
        let global_val = if let Some(x) = self.module.get_global(&sym) {
            x
        } else {
            let llvm_ty = self.sa_type(sa);
            let global = self.module.add_global(llvm_ty, None, &sym);
            global.set_alignment(sa.align() as u32);
            global.set_initializer(&llvm_ty.const_zero());
            global
        };
        let center_offset = sa.allocation_center_offset();
        if center_offset == 0 {
            return global_val.as_pointer_value();
        }
        self.const_byte_gep(global_val.as_pointer_value(), center_offset as i64)
    }

    pub(super) fn create_all_statics(&mut self) -> IResult<()> {
        let global_sequence = self.compiler_database.db.global_sequence().unwrap();
        for pd in global_sequence.iter() {
            if !self.collected_layouts.globals.contains_key(pd) {
                continue;
            };
            self.global_constant(*pd);
        }
        Ok(())
    }

    pub(super) fn global_constant_init_fun_val(&mut self) -> FunctionValue<'llvm> {
        if let Some(x) = self.module.get_function(YABO_GLOBAL_INIT) {
            return x;
        }
        let fun_type = self.llvm.i64_type().fn_type(&[], false);
        let f = self
            .module
            .add_function(YABO_GLOBAL_INIT, fun_type, Some(Linkage::External));
        f
    }
}
