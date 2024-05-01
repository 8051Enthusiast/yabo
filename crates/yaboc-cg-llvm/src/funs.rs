use fxhash::FxHashSet;
use yaboc_base::low_effort_interner::Uniq;
use yaboc_constraint::Constraints;
use yaboc_hir_types::VTABLE_BIT;
use yaboc_layout::{
    collect::{fun_req, pd_len_req, pd_val_req},
    mir_subst::function_substitute,
    represent::ParserFunKind,
    Layout,
};
use yaboc_mir::{FunKind, MirKind};
use yaboc_req::NeededBy;
use yaboc_resolve::Resolves;

use crate::{
    convert_regex::RegexTranslator,
    convert_thunk::{BlockThunk, TransmuteCopyThunk, TypecastThunk, ValThunk},
};

use super::*;

impl<'llvm, 'comp> CodeGenCtx<'llvm, 'comp> {
    fn non_zero_early_return(
        &mut self,
        fun: FunctionValue<'llvm>,
        status: IntValue<'llvm>,
    ) -> IResult<()> {
        let success_bb = self.llvm.append_basic_block(fun, "deref_succ");
        let fail_bb = self.llvm.append_basic_block(fun, "deref_fail");
        let is_not_zero = self.builder.build_int_compare(
            IntPredicate::NE,
            status,
            self.const_i64(ReturnStatus::Ok as i64),
            "deref_status_is_zero",
        )?;
        self.builder
            .build_conditional_branch(is_not_zero, fail_bb, success_bb)?;

        self.builder.position_at_end(fail_bb);
        self.builder.build_return(Some(&status))?;

        self.builder.position_at_end(success_bb);
        Ok(())
    }

    fn terminate_tail_typecast(
        &mut self,
        arg: CgValue<'comp, 'llvm>,
        ret: CgReturnValue<'llvm>,
    ) -> IResult<()> {
        let ret = self.call_typecast_fun(ret, arg)?;
        self.builder.build_return(Some(&ret))?;
        Ok(())
    }

    fn wrap_direct_call(
        &mut self,
        fun: FunctionValue<'llvm>,
        wrapper: FunctionValue<'llvm>,
        tail: bool,
    ) -> IResult<FunctionValue<'llvm>> {
        let args = wrapper
            .get_param_iter()
            .map(|x| x.into())
            .collect::<Vec<_>>();
        if wrapper.get_last_basic_block().is_none() {
            self.add_entry_block(wrapper);
        }
        let call = self.builder.build_call(fun, &args, "call")?;
        self.set_tail_call(call, tail);
        let ret = call.try_as_basic_value().left().unwrap().into_int_value();
        self.builder.build_return(Some(&ret))?;
        Ok(wrapper)
    }

    fn setup_tail_fun_copy(
        &mut self,
        from: ILayout<'comp>,
        fun: CgMonoValue<'comp, 'llvm>,
    ) -> IResult<Option<CgMonoValue<'comp, 'llvm>>> {
        let tail_info = self.collected_layouts.tail_sa[&(from, fun.layout)];
        if !tail_info.has_tailsites {
            return Ok(None);
        }
        let fun_copy_ptr = self.build_sa_alloca(tail_info.sa, "fun_copy")?;
        let fun_buf = CgMonoValue::new(fun.layout, fun_copy_ptr);
        self.build_copy_invariant(fun_buf.into(), fun.into())?;
        Ok(Some(fun_buf))
    }

    fn create_wrapper_parse(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        req: RequirementSet,
        inner: FunctionValue<'llvm>,
    ) -> IResult<FunctionValue<'llvm>> {
        let wrapper = self.parser_fun_val_wrapper(layout, from, req);
        let (ret, fun_arg, from) = parser_values(wrapper, layout, from);
        self.add_entry_block(wrapper);
        let Some(val) = self.setup_tail_fun_copy(from.layout, fun_arg)? else {
            // cannot be a tail call because of different calling conventions
            return self.wrap_direct_call(inner, wrapper, false);
        };
        let ret = self.build_tailcc_call_with_int_ret(
            inner,
            &[
                ret.ptr.into(),
                val.ptr.into(),
                ret.head.into(),
                from.ptr.into(),
            ],
        )?;
        self.builder.build_return(Some(&ret))?;
        Ok(wrapper)
    }

    fn mir_pd_fun(&mut self, layout: IMonoLayout<'comp>) -> IResult<FunctionSubstitute<'comp>> {
        let MonoLayout::NominalParser(pd, _, _) = layout.mono_layout().0 else {
            panic!("mir_pd_len_fun has to be called with a nominal parser layout");
        };
        let kind = FunKind::ParserDef(*pd);
        let req = MirKind::Call(fun_req());
        let mir = self.compiler_database.db.mir(kind, req).unwrap();
        let strictness = self.compiler_database.db.strictness(kind, req).unwrap();
        Ok(
            FunctionSubstitute::new_from_pd(mir, &strictness, None, layout, *pd, self.layouts)
                .unwrap(),
        )
    }

    fn mir_pd_parser(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        mut req: RequirementSet,
    ) -> FunctionSubstitute<'comp> {
        let MonoLayout::NominalParser(pd, _, bt) = layout.mono_layout().0 else {
            panic!("mir_pd_len_fun has to be called with a nominal parser layout");
        };
        if !*bt {
            req &= !NeededBy::Backtrack;
        }
        let kind = FunKind::ParserDef(*pd);
        let req = MirKind::Call(req);
        let mir = self.compiler_database.db.mir(kind, req).unwrap();
        let strictness = self.compiler_database.db.strictness(kind, req).unwrap();
        FunctionSubstitute::new_from_pd(mir, &strictness, Some(from), layout, *pd, self.layouts)
            .unwrap()
    }

    fn mir_block(
        &mut self,
        from: Option<ILayout<'comp>>,
        layout: IMonoLayout<'comp>,
        mut req: RequirementSet,
    ) -> FunctionSubstitute<'comp> {
        let MonoLayout::BlockParser(bd, _, _, bt) = layout.mono_layout().0 else {
            panic!("mir_pd_len_fun has to be called with a nominal parser layout");
        };
        if from.is_some() && !bt {
            req &= !NeededBy::Backtrack;
        }
        let req = MirKind::Call(req);
        let kind = FunKind::Block(*bd);
        let mir = self.compiler_database.db.mir(kind, req).unwrap();
        let strictness = self.compiler_database.db.strictness(kind, req).unwrap();
        FunctionSubstitute::new_from_block(mir, &strictness, from, layout, self.layouts).unwrap()
    }

    fn mir_if_fun(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        req: RequirementSet,
    ) -> FunctionSubstitute<'comp> {
        let (MonoLayout::IfParser(_, cid, wiggle), ty) = layout.mono_layout() else {
            panic!("mir_pd_len_fun has to be called with a nominal parser layout");
        };
        let kind = FunKind::If(*cid, ty, *wiggle);
        let req = MirKind::Call(req);
        let mir = self.compiler_database.db.mir(kind, req).unwrap();
        let strictness = self.compiler_database.db.strictness(kind, req).unwrap();
        FunctionSubstitute::new_from_if(mir, &strictness, from, layout, self.layouts).unwrap()
    }

    fn create_typecast(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        if let MonoLayout::Nominal(..) = layout.mono_layout().0 {
            let (from, fun) = layout.unapply_nominal(self.layouts);
            self.create_pd_parse_impl(from, fun, pd_val_req().req)?;
        }
        let thunk_info = TypecastThunk::new(self, layout)?;
        ThunkContext::new(self, thunk_info).build()?;
        Ok(())
    }

    fn create_mask_simple(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.mask_fun_val(layout);
        self.add_entry_block(fun);
        let sa = layout.inner().size_align(self.layouts).unwrap();
        self.builder
            .build_return(Some(&self.const_size_t(sa.after as i64)))?;
        Ok(())
    }

    fn create_mask_single(
        &mut self,
        layout: IMonoLayout<'comp>,
        inner: ILayout<'comp>,
    ) -> IResult<()> {
        let fun = self.mask_fun_val(layout);
        self.add_entry_block(fun);
        let [arg] = get_fun_args(fun).map(|x| x.into_pointer_value());
        let inner_sa = inner.size_align(self.layouts).unwrap();
        let offset = inner_sa.allocation_center_offset();
        let inner_ptr = self.build_byte_gep(arg, self.const_i64(offset as i64), "inner_ptr")?;
        self.call_mask_fun(CgValue::new(inner, inner_ptr))?;
        self.builder
            .build_return(Some(&self.const_size_t(inner_sa.allocation_size() as i64)))?;
        Ok(())
    }

    fn create_mask_pair(
        &mut self,
        layout: IMonoLayout<'comp>,
        inner: [ILayout<'comp>; 2],
    ) -> IResult<()> {
        let fun = self.mask_fun_val(layout);
        self.add_entry_block(fun);
        let [arg] = get_fun_args(fun).map(|x| x.into_pointer_value());
        let inner_sa = inner.map(|x| x.size_align(self.layouts).unwrap());
        let offsets = SizeAlign::offsets(inner_sa);
        let sa = layout
            .inner()
            .size_align_without_vtable(self.layouts)
            .unwrap();
        let padding_range = (offsets[0] + inner_sa[0].after)..(offsets[1] - inner_sa[1].before);
        let padding_size = self.const_i64(padding_range.clone().count() as i64);
        let zero = self.llvm.i8_type().const_zero();
        let padding_ptr =
            self.build_byte_gep(arg, self.const_i64(padding_range.start as i64), "padding")?;
        self.builder
            .build_memset(padding_ptr, 1, zero, padding_size)?;
        for (offset, inner) in offsets.iter().zip(inner.iter()) {
            let inner_ptr = self.build_byte_gep(arg, self.const_i64(*offset as i64), "inner")?;
            self.call_mask_fun(CgValue::new(*inner, inner_ptr))?;
        }
        self.builder
            .build_return(Some(&self.const_size_t(sa.after as i64)))?;
        Ok(())
    }

    fn create_mask_manifested(
        &mut self,
        layout: IMonoLayout<'comp>,
        mut layouts: impl FnMut(DefId) -> ILayout<'comp>,
    ) -> IResult<()> {
        let fun = self.mask_fun_val(layout);
        self.add_entry_block(fun);
        let [arg] = get_fun_args(fun).map(|x| x.into_pointer_value());
        let manifestation = self.layouts.dcx.manifestation(layout.inner());
        for (offset, mask) in manifestation.padding_mask.iter().enumerate() {
            if let 0xff = *mask {
                continue;
            }
            let offset_ptr = self.build_byte_gep(arg, self.const_i64(offset as i64), "offset")?;
            if let 0 = *mask {
                self.builder
                    .build_store(offset_ptr, self.llvm.i8_type().const_zero())?;
            } else {
                let mask = self.llvm.i8_type().const_int(*mask as u64, false);
                let old_value = self.build_byte_load(offset_ptr, "old_value")?;
                let new_value = self.builder.build_and(old_value, mask, "new_value")?;
                self.builder.build_store(offset_ptr, new_value)?;
            }
        }
        for (&id, &offset) in manifestation.field_offsets.iter() {
            let inner = layouts(id);
            let cont_bb = if let Some(bit_offset) = manifestation.discriminant_mapping.get(&id) {
                let cont_bb = self.llvm.append_basic_block(fun, "cont");
                let zero_bb = self.llvm.append_basic_block(fun, "zero");
                let mask_bb = self.llvm.append_basic_block(fun, "mask");
                let byte_offset = bit_offset / 8;
                let inner_bit_offset = bit_offset % 8;
                let disc_byte_ptr =
                    self.build_byte_gep(arg, self.const_i64(byte_offset as i64), "disc_byte_ptr")?;
                let disc_byte = self.build_byte_load(disc_byte_ptr, "disc_byte")?;
                let disc_bit = self.builder.build_and(
                    disc_byte,
                    self.llvm.i8_type().const_int(1 << inner_bit_offset, false),
                    "disc_bit",
                )?;
                let is_nonzero = self.builder.build_int_compare(
                    IntPredicate::NE,
                    disc_bit,
                    self.llvm.i8_type().const_zero(),
                    "is_nonzero",
                )?;
                self.builder
                    .build_conditional_branch(is_nonzero, cont_bb, zero_bb)?;
                self.builder.position_at_end(zero_bb);
                let inner_sa = inner.size_align(self.layouts).unwrap();
                let val_ptr = self.build_byte_gep(
                    arg,
                    self.const_i64((offset - inner_sa.before) as i64),
                    "val_ptr",
                )?;
                let size = self.const_i64(inner_sa.total_size() as i64);
                let val = self.llvm.i8_type().const_zero();
                self.builder
                    .build_memset(val_ptr, inner_sa.start_alignment() as u32, val, size)?;
                self.builder.build_unconditional_branch(cont_bb)?;
                self.builder.position_at_end(mask_bb);
                Some(cont_bb)
            } else {
                None
            };
            let inner_ptr = self.build_byte_gep(arg, self.const_i64(offset as i64), "inner")?;
            self.call_mask_fun(CgValue::new(inner, inner_ptr))?;
            if let Some(cont_bb) = cont_bb {
                self.builder.build_unconditional_branch(cont_bb)?;
                self.builder.position_at_end(cont_bb);
            }
        }
        self.builder
            .build_return(Some(&self.const_size_t(manifestation.size.after as i64)))?;
        Ok(())
    }

    fn create_mask_funs(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        match layout.mono_layout().0 {
            MonoLayout::Primitive(_)
            | MonoLayout::SlicePtr
            | MonoLayout::Single
            | MonoLayout::Nil
            | MonoLayout::Regex(_, _)
            | MonoLayout::ArrayParser(None) => self.create_mask_simple(layout),
            MonoLayout::IfParser(inner, _, _) | MonoLayout::ArrayParser(Some((inner, None))) => {
                self.create_mask_single(layout, *inner)
            }
            MonoLayout::ArrayParser(Some((first, Some(second))))
            | MonoLayout::Array {
                parser: first,
                slice: second,
            } => self.create_mask_pair(layout, [*first, *second]),
            MonoLayout::BlockParser(_, cap, _, _) => {
                self.create_mask_manifested(layout, |id| cap[&id])
            }
            MonoLayout::NominalParser(pd, args, _) => self.create_mask_manifested(layout, |id| {
                let idx = self
                    .compiler_database
                    .db
                    .parserdef_arg_index(*pd, id)
                    .unwrap()
                    .unwrap();
                args[idx].0
            }),
            MonoLayout::Nominal(pd, from, args) => self.create_mask_manifested(layout, |id| {
                let (inner, _) = self
                    .compiler_database
                    .db
                    .parserdef_arg_index(*pd, id)
                    .unwrap()
                    .map(|idx| args[idx])
                    .or(*from)
                    .unwrap();
                inner
            }),
            MonoLayout::Block(_, layouts) => self.create_mask_manifested(layout, |id| {
                let field = id.unwrap_name(&self.compiler_database.db);
                layouts[&FieldName::Ident(field)]
            }),
            MonoLayout::Tuple(_) => unreachable!(),
        }
    }

    fn create_pd_parse(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        req: RequirementSet,
    ) -> IResult<FunctionValue<'llvm>> {
        let impl_fun = self.create_pd_parse_impl(from, layout, req)?;

        let MonoLayout::NominalParser(pd, args, _) = layout.mono_layout().0 else {
            panic!("create_pd_parse has to be called with a nominal parser layout");
        };
        let Type::ParserArg { arg: arg_ty, .. } = self
            .compiler_database
            .db
            .lookup_intern_type(layout.mono_layout().1)
        else {
            panic!("create_pd_parse has to be called with a nominal parser layout");
        };

        let thunky = pd.lookup(&self.compiler_database.db).unwrap().kind.thunky();

        if !req.contains(NeededBy::Val) || !thunky {
            // just call impl_fun and return
            let llvm_fun = self.parser_fun_val_tail(layout, from, req);
            return self.wrap_direct_call(impl_fun, llvm_fun, true);
        }

        if !(req & !NeededBy::Val).is_empty() {
            self.create_pd_parse_impl(from, layout, req & !NeededBy::Val)?;
        }

        let result = self
            .compiler_database
            .db
            .parser_result(layout.mono_layout().1)
            .expect("pd parsers type is not parser");
        let mut map = FxHashMap::default();
        map.insert(Arg::From, (from, arg_ty));
        let parserdef_args = pd
            .lookup(&self.compiler_database.db)
            .unwrap()
            .args
            .unwrap_or_default();
        for (idx, arg) in args.iter().enumerate() {
            map.insert(Arg::Named(parserdef_args[idx].0), *arg);
        }
        let return_layout = ILayout::make_thunk(self.layouts, *pd, result, &map)
            .unwrap()
            .maybe_mono()
            .unwrap();
        let val_thunk_info = ValThunk::new(Some(from), layout, return_layout, req);
        ThunkContext::new(self, val_thunk_info).build()
    }

    fn create_pd_parse_impl(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        req: RequirementSet,
    ) -> IResult<FunctionValue<'llvm>> {
        let part = self.parser_layout_part(from, req, ParserFunKind::Worker);
        let sym = self.sym(layout, part);
        let llvm_fun = match self.module.get_function(&sym) {
            Some(f) => return Ok(f),
            None => self.parser_impl_fun_val(layout, from, req),
        };
        if from.is_int() {
            self.add_entry_block(llvm_fun);
            self.set_always_inline(llvm_fun);
            self.builder
                .build_return(Some(&self.const_i64(ReturnStatus::Error as i64)))?;
            return Ok(llvm_fun);
        }
        let mir_fun = Rc::new(self.mir_pd_parser(from, layout, req));
        let (ret, fun, arg) = parser_values(llvm_fun, layout, from);
        let mut translator = MirTranslator::new(self, mir_fun, llvm_fun, fun, arg)?;
        if req.contains(NeededBy::Val) {
            translator = translator.with_ret_val(ret);
        }
        translator.build()
    }

    fn create_pd_end(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let llvm_fun = self.end_fun_val(layout);
        self.add_entry_block(llvm_fun);
        let [ret, nom, head] = get_fun_args(llvm_fun);
        let [ret, nom] = [ret, nom].map(|x| x.into_pointer_value());
        let ret_val = CgReturnValue::new(head.into_int_value(), ret);
        let nom = CgMonoValue::new(layout, nom);
        let (from, fun) = self.build_nominal_components(nom)?;
        let from_copy = self.build_alloca_value(from.layout, "from_copy")?;
        self.build_copy_invariant(from_copy, from)?;
        let no_ret = self.undef_ret();
        let ret = self.build_parser_call(no_ret, fun.into(), from_copy, pd_len_req())?;
        self.non_zero_early_return(llvm_fun, ret)?;
        self.terminate_tail_typecast(from_copy, ret_val)
    }

    fn create_pd_start(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.start_fun_val(layout);
        self.add_entry_block(fun);
        let [to, nom, head] = get_fun_args(fun);
        let [to, nom] = [to, nom].map(|x| x.into_pointer_value());
        let head = head.into_int_value();
        let ret = CgReturnValue::new(head, to);
        let nom = CgMonoValue::new(layout, nom);
        let (from, _) = self.build_nominal_components(nom)?;
        self.terminate_tail_typecast(from, ret)
    }

    fn get_slice_ptrs(&mut self, arg: PointerValue<'llvm>) -> IResult<[PointerValue<'llvm>; 2]> {
        let ptr = self.build_ptr_load(arg, "load_ptr")?;
        let ptr_ty = self.any_ptr();
        let end_ptr_ptr = unsafe {
            self.builder
                .build_in_bounds_gep(ptr_ty, arg, &[self.const_i64(1)], "end_ptr_ptr")?
        };
        let end_ptr = self.build_ptr_load(end_ptr_ptr, "load_end_ptr")?;
        Ok([ptr, end_ptr])
    }

    fn create_sliceptr_single_forward(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.single_forward_fun_val(layout);
        self.set_always_inline(fun);

        let [arg] = get_fun_args(fun).map(|x| x.into_pointer_value());
        self.add_entry_block(fun);
        let arg_ptr = self.build_cast::<*mut *const u8, _>(arg)?;
        let ptr = self.build_ptr_load(arg_ptr, "load_ptr")?;
        let inc_ptr = self.build_byte_gep(ptr, self.const_i64(1), "inc_ptr")?;
        self.builder.build_store(arg, inc_ptr)?;
        self.builder
            .build_return(Some(&self.const_i64(ReturnStatus::Ok as i64)))?;
        Ok(())
    }

    fn create_sliceptr_len(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.array_len_fun_val(layout);
        self.set_always_inline(fun);
        self.add_entry_block(fun);
        let [arg] = get_fun_args(fun).map(|x| x.into_pointer_value());
        let [arg_start, arg_end] = self.get_slice_ptrs(arg)?;
        let i8 = self.llvm.i8_type();
        let len = self.builder.build_ptr_diff(i8, arg_end, arg_start, "len")?;
        self.builder.build_return(Some(&len))?;
        Ok(())
    }

    fn create_sliceptr_skip(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.skip_fun_val(layout);
        self.set_always_inline(fun);
        self.add_entry_block(fun);
        let [arg, len] = get_fun_args(fun);
        let len = len.into_int_value();
        let arg = arg.into_pointer_value();
        let arg_ptr = self.build_cast::<*mut *const u8, _>(arg)?;
        let [arg_start, _] = self.get_slice_ptrs(arg)?;
        let inc_ptr = self.build_byte_gep(arg_start, len, "inc_ptr")?;
        self.builder.build_store(arg_ptr, inc_ptr)?;
        self.builder
            .build_return(Some(&self.const_i64(ReturnStatus::Ok as i64)))?;
        Ok(())
    }

    fn create_sliceptr_span(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.span_fun_val(layout);
        self.set_always_inline(fun);
        self.add_entry_block(fun);
        let [ret, start, head, end] = get_fun_args(fun);
        let [ret, start, end] = [ret, start, end].map(|x| x.into_pointer_value());
        let ret = CgReturnValue::new(head.into_int_value(), ret);
        let buf = self.build_alloca_value(layout.inner(), "buf")?;
        let [start_ptr, _] = self.get_slice_ptrs(start)?;
        let [end_ptr, _] = self.get_slice_ptrs(end)?;
        let bufsl = self.build_cast::<*mut *const u8, _>(buf.ptr)?;
        let ty = self.any_ptr();
        self.builder.build_store(bufsl, start_ptr)?;
        let bufsl = unsafe {
            self.builder
                .build_in_bounds_gep(ty, bufsl, &[self.const_i64(1)], "ret")?
        };
        self.builder.build_store(bufsl, end_ptr)?;
        self.terminate_tail_typecast(buf, ret)
    }

    fn create_u8_current_element(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.current_element_fun_val(layout);
        self.set_always_inline(fun);
        self.add_entry_block(fun);
        let int_buf = self.build_alloca_int("int_buf")?;
        let [return_ptr, from, target_head] = get_fun_args(fun);
        let from = self.build_cast::<*const *const u8, _>(from)?;
        let ret = CgReturnValue::new(
            target_head.into_int_value(),
            return_ptr.into_pointer_value(),
        );
        let int_ptr = self.build_ptr_load(from, "load_ptr")?;
        let byte = self.build_byte_load(int_ptr, "load_byte")?;
        let int = self
            .builder
            .build_int_z_extend(byte, self.llvm.i64_type(), "int")?;
        let bitcasted_buf = self.build_cast::<*mut i64, _>(int_buf.ptr)?;
        self.builder.build_store(bitcasted_buf, int)?;
        self.terminate_tail_typecast(int_buf.into(), ret)
    }

    fn create_sliceptr_current_element(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.current_element_fun_val(layout);
        self.set_always_inline(fun);
        self.add_entry_block(fun);
        let [return_ptr, from, target_head] = get_fun_args(fun);
        let layout = self
            .layouts
            .dcx
            .primitive(self.layouts.db, PrimitiveType::U8);
        let ret = CgReturnValue::new(
            target_head.into_int_value(),
            return_ptr.into_pointer_value(),
        );
        let from = CgValue::new(layout, from.into_pointer_value());
        self.terminate_tail_typecast(from, ret)
    }

    fn create_sliceptr_inner_array(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.inner_array_fun_val(layout);
        self.add_entry_block(fun);
        let ret = self.const_i64(ReturnStatus::Backtrack as i64);
        self.builder.build_return(Some(&ret))?;
        Ok(())
    }

    fn build_array_item_len_get(
        &mut self,
        array: CgMonoValue<'comp, 'llvm>,
        int_buf: CgMonoValue<'comp, 'llvm>,
        fun: FunctionValue<'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        let parser = self.build_array_parser_get(array)?;
        let status = self.call_len_fun(int_buf.ptr, parser)?;
        self.non_zero_early_return(fun, status)?;
        self.build_i64_load(int_buf.ptr, "item_len")
    }

    fn create_array_single_forward(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.single_forward_fun_val(layout);
        self.add_entry_block(fun);
        let [arg] = get_fun_args(fun).map(|x| x.into_pointer_value());
        let array = CgMonoValue::new(layout, arg);
        let int_buf = self.build_alloca_int("item_len_buf")?;
        let len = self.build_array_item_len_get(array, int_buf, fun)?;
        let slice = self.build_array_slice_get(array)?;
        let ret = self.call_skip_fun(slice, len)?;
        self.builder.build_return(Some(&ret))?;
        Ok(())
    }

    fn create_array_current_element(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.current_element_fun_val(layout);
        self.add_entry_block(fun);
        let [return_ptr, from, target_head] = get_fun_args(fun);
        let ret = CgReturnValue::new(
            target_head.into_int_value(),
            return_ptr.into_pointer_value(),
        );
        let array = CgMonoValue::new(layout, from.into_pointer_value());
        let parser = self.build_array_parser_get(array)?;
        let slice = self.build_array_slice_get(array)?;
        let ret = self.build_parser_call(
            ret,
            parser,
            slice,
            CallMeta::new(NeededBy::Val.into(), false),
        )?;
        self.builder.build_return(Some(&ret))?;
        Ok(())
    }

    fn create_array_len(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.array_len_fun_val(layout);
        self.add_entry_block(fun);
        let [arg] = get_fun_args(fun).map(|x| x.into_pointer_value());
        let array = CgMonoValue::new(layout, arg);
        let int_buf = self.build_alloca_int("item_len_buf")?;
        let len = self.build_array_item_len_get(array, int_buf, fun)?;
        let slice = self.build_array_slice_get(array)?;
        let slice_len = self.call_array_len_fun(slice)?;
        let ret = self.builder.build_int_signed_div(slice_len, len, "ret")?;
        self.builder.build_return(Some(&ret))?;
        Ok(())
    }

    fn create_array_skip(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.skip_fun_val(layout);
        self.add_entry_block(fun);
        let [arg, len] = get_fun_args(fun);
        let len = len.into_int_value();
        let arg = arg.into_pointer_value();
        let array = CgMonoValue::new(layout, arg);
        let int_buf = self.build_alloca_int("item_len_buf")?;
        let item_len = self.build_array_item_len_get(array, int_buf, fun)?;
        let slice = self.build_array_slice_get(array)?;
        let skip_len = self.builder.build_int_mul(item_len, len, "skip_len")?;
        let ret = self.call_skip_fun(slice, skip_len)?;
        self.builder.build_return(Some(&ret))?;
        Ok(())
    }

    fn create_array_span(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.span_fun_val(layout);
        self.set_always_inline(fun);
        self.add_entry_block(fun);
        let [ret, start, head, end] = get_fun_args(fun);
        let [ret, start, end] = [ret, start, end].map(|x| x.into_pointer_value());
        let bufsl = self.build_alloca_mono_value(layout, "bufsl")?;
        let ret = CgReturnValue::new(head.into_int_value(), ret);
        let start = CgMonoValue::new(layout, start);
        let end = CgMonoValue::new(layout, end);
        let buf_parser = self.build_array_parser_get(bufsl)?;
        let start_parser = self.build_array_parser_get(start)?;
        self.build_copy_invariant(buf_parser, start_parser)?;
        let start_slice = self.build_array_slice_get(start)?;
        let end_slice = self.build_array_slice_get(end)?;
        let buf_slice = self.build_array_slice_get(bufsl)?;
        let buf_slice_ret = self.build_return_value(buf_slice)?;
        self.call_span_fun(buf_slice_ret, start_slice, end_slice)?;
        self.terminate_tail_typecast(bufsl.into(), ret)?;
        Ok(())
    }

    fn create_array_inner_array(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.inner_array_fun_val(layout);
        self.add_entry_block(fun);
        let [ret, from, head] = get_fun_args(fun);
        let ret = CgReturnValue::new(head.into_int_value(), ret.into_pointer_value());
        let array = CgMonoValue::new(layout, from.into_pointer_value());
        let slice = self.build_array_slice_get(array)?;
        let ret = self.call_typecast_fun(ret, slice)?;
        self.builder.build_return(Some(&ret))?;
        Ok(())
    }

    fn create_block_parse(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        req: RequirementSet,
    ) -> IResult<FunctionValue<'llvm>> {
        let MonoLayout::BlockParser(block, _, _, _) = layout.mono_layout().0 else {
            panic!("Expected block parser layout")
        };
        let block = block.lookup(&self.compiler_database.db).unwrap();
        let llvm_fun = self.parser_fun_val_tail(layout, from, req);
        let mir_fun = Rc::new(self.mir_block(Some(from), layout, req));
        let impl_fun = self.parser_impl_fun_val(layout, from, req);

        let (ret, fun, arg) = parser_values(impl_fun, layout, from);
        let mut translator = MirTranslator::new(self, mir_fun, impl_fun, fun, arg)?;
        if req.contains(NeededBy::Val) {
            translator = translator.with_ret_val(ret)
        }
        translator.build()?;

        // if we have no return value, we do not need the thunk which is just for
        // making sure the vtable pointer for the block return is properly returned
        // which is actually counterproductive since we may not even have collected
        // the block layout during collection
        if block.returns || !req.contains(NeededBy::Val) {
            self.wrap_direct_call(impl_fun, llvm_fun, true)?;
            return Ok(llvm_fun);
        }

        let return_layout = self.layouts.block_result()[&(Some(from), layout.inner())]
            .val()
            .as_ref()
            .unwrap()
            .returned;
        let mono_layout = return_layout.maybe_mono().unwrap();

        let block_data = BlockThunk {
            from: Some(from),
            fun: layout,
            result: mono_layout,
            req,
        };

        ThunkContext::new(self, block_data).build()
    }

    fn create_if_parse(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        req: RequirementSet,
    ) -> IResult<FunctionValue<'llvm>> {
        let llvm_fun = self.parser_fun_val_tail(layout, from, req);
        let mir_fun = Rc::new(self.mir_if_fun(from, layout, req));
        let (ret, fun, arg) = parser_values(llvm_fun, layout, from);
        let mut trans = MirTranslator::new(self, mir_fun, llvm_fun, fun, arg)?;
        if req.contains(NeededBy::Val) {
            trans = trans.with_ret_val(ret)
        }
        trans.build()?;
        Ok(llvm_fun)
    }

    fn create_single_parse(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        req: RequirementSet,
    ) -> IResult<FunctionValue<'llvm>> {
        let llvm_fun = self.parser_fun_val_tail(layout, from, req);
        self.set_always_inline(llvm_fun);
        self.add_entry_block(llvm_fun);
        let (ret, _, arg) = parser_values(llvm_fun, layout, from);
        let fail_block = self.llvm.append_basic_block(llvm_fun, "fail");
        let ok_block = self.llvm.append_basic_block(llvm_fun, "ok");
        let ptr_diff = self.call_array_len_fun(arg)?;
        let is_zero = self.builder.build_int_compare(
            IntPredicate::EQ,
            ptr_diff,
            self.const_i64(0),
            "is_zero",
        )?;
        self.builder
            .build_conditional_branch(is_zero, fail_block, ok_block)?;
        self.builder.position_at_end(fail_block);
        self.builder
            .build_return(Some(&self.const_i64(ReturnStatus::Eof as i64)))?;
        self.builder.position_at_end(ok_block);
        if req.contains(NeededBy::Val) {
            let ret = self.call_current_element_fun(ret, arg)?;
            self.non_zero_early_return(llvm_fun, ret)?;
        }
        if req.contains(NeededBy::Len) {
            let ret = self.call_single_forward_fun(arg)?;
            self.builder.build_return(Some(&ret))?;
        } else {
            self.builder.build_return(Some(&self.const_i64(0)))?;
        }
        Ok(llvm_fun)
    }

    fn create_nil_parse(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        req: RequirementSet,
    ) -> IResult<FunctionValue<'llvm>> {
        let llvm_fun = self.parser_fun_val_tail(layout, from, req);
        self.add_entry_block(llvm_fun);
        self.set_always_inline(llvm_fun);
        let (ret, _, _) = parser_values(llvm_fun, layout, from);
        if req.contains(NeededBy::Val) {
            let unit_type = self
                .compiler_database
                .db
                .intern_type(Type::Primitive(PrimitiveType::Unit));
            let unit_layout = canon_layout(self.layouts, unit_type).unwrap();
            let undef = CgValue::new(unit_layout, self.invalid_ptr());
            self.terminate_tail_typecast(undef, ret)?;
        } else {
            self.builder.build_return(Some(&self.const_i64(0)))?;
        }
        Ok(llvm_fun)
    }

    fn create_error_parse(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        req: RequirementSet,
    ) -> IResult<FunctionValue<'llvm>> {
        let llvm_fun = self.parser_fun_val_tail(layout, from, req);
        self.add_entry_block(llvm_fun);
        self.set_always_inline(llvm_fun);
        self.builder
            .build_return(Some(&self.const_i64(ReturnStatus::Error as i64)))?;
        Ok(llvm_fun)
    }

    fn create_regex_parse(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        req: RequirementSet,
    ) -> IResult<FunctionValue<'llvm>> {
        let MonoLayout::Regex(regex, bt) = layout.mono_layout().0 else {
            panic!("called build_regex_parse on non-regex")
        };
        let regex_str = self.compiler_database.db.lookup_intern_regex(*regex);
        let regex_impl = self.create_regex_parse_impl(from, layout, &regex_str, req)?;
        let llvm_fun = self.parser_fun_val_tail(layout, from, req);
        self.add_entry_block(llvm_fun);
        let (ret, fun, arg) = parser_values(llvm_fun, layout, from);
        let ret_copy = if !req.contains(NeededBy::Val) {
            let buf_ptr = self.build_layout_alloca(from, "ret_copy")?;
            self.undef_ret().with_ptr(buf_ptr)
        } else {
            ret
        };
        let arg_copy = if !req.contains(NeededBy::Len) {
            let a = self.build_alloca_value(from, "arg_copy")?;
            self.build_copy_invariant(a, arg)?;
            a
        } else {
            arg
        };
        let ret = self.build_tailcc_call_with_int_ret(
            regex_impl,
            &[
                ret_copy.ptr.into(),
                fun.ptr.into(),
                ret_copy.head.into(),
                arg_copy.ptr.into(),
            ],
        )?;
        let ret = if !bt {
            let is_bt = self.builder.build_int_compare(
                IntPredicate::EQ,
                ret,
                self.const_i64(ReturnStatus::Backtrack as i64),
                "is_bt",
            )?;
            self.builder
                .build_select(
                    is_bt,
                    self.const_i64(ReturnStatus::Error as i64),
                    ret,
                    "btless",
                )?
                .into_int_value()
        } else {
            ret
        };
        self.builder.build_return(Some(&ret))?;
        Ok(llvm_fun)
    }

    fn create_regex_parse_impl(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        regex: &str,
        req: RequirementSet,
    ) -> IResult<FunctionValue<'llvm>> {
        let part = self.parser_layout_part(from, req, ParserFunKind::Worker);
        let sym = self.sym(layout, part);
        let llvm_fun = match self.module.get_function(&sym) {
            Some(f) => return Ok(f),
            None => self.parser_impl_fun_val(layout, from, req),
        };
        let dfa = regex_automata::dense::Builder::new()
            .anchored(true)
            .allow_invalid_utf8(true)
            .dot_matches_new_line(true)
            .case_insensitive(false)
            .minimize(true)
            .build(regex)
            .expect("invalid regex");
        let mut trans = RegexTranslator::new(self, llvm_fun, &dfa, from, true)?;
        trans.build()?;
        Ok(llvm_fun)
    }

    fn create_array_parse(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        req: RequirementSet,
    ) -> IResult<FunctionValue<'llvm>> {
        let result_layout = layout
            .inner()
            .apply_arg(self.layouts, from)
            .unwrap()
            .maybe_mono()
            .unwrap();
        let llvm_fun = self.parser_fun_val_tail(layout, from, req);
        self.add_entry_block(llvm_fun);
        let (ret_val, parser, mut arg) = parser_values(llvm_fun, layout, from);
        let arg_copy = self.build_alloca_value(from, "arg_copy")?;
        let ret_buf = self.build_alloca_mono_value(result_layout, "ret_buf")?;
        let int_buf = self.build_alloca_int("inner_parser_len")?;
        // make sure we don't modify the original arg if the length is not required
        if !req.contains(NeededBy::Len) {
            let arg_second_copy = self.build_alloca_value(from, "arg_second_copy")?;
            self.build_copy_invariant(arg_second_copy, arg)?;
            arg = arg_second_copy;
        }
        self.build_copy_invariant(arg_copy, arg)?;

        let status = self.call_len_fun(int_buf.ptr, parser.into())?;
        self.non_zero_early_return(llvm_fun, status)?;
        let full_len = self.build_i64_load(int_buf.ptr, "parser_len")?;
        let slice_len = self.call_array_len_fun(arg)?;
        let is_out_of_bounds = self.builder.build_int_compare(
            IntPredicate::ULT,
            slice_len,
            full_len,
            "is_out_of_bounds",
        )?;
        let succ_block = self.llvm.append_basic_block(llvm_fun, "succ");
        let fail_block = self.llvm.append_basic_block(llvm_fun, "fail");
        self.builder
            .build_conditional_branch(is_out_of_bounds, fail_block, succ_block)?;
        self.builder.position_at_end(fail_block);
        self.builder
            .build_return(Some(&self.const_i64(ReturnStatus::Eof as i64)))?;
        self.builder.position_at_end(succ_block);
        let ret = self.call_skip_fun(arg, full_len)?;
        self.non_zero_early_return(llvm_fun, ret)?;
        if req.contains(NeededBy::Val) {
            let inner_slice = if let (
                MonoLayout::ArrayParser(Some((
                    ILayout {
                        layout: Uniq(_, Layout::Mono(MonoLayout::Single, _)),
                    },
                    _,
                ))),
                _,
            ) = layout.mono_layout()
            {
                CgValue::new(result_layout.inner(), ret_buf.ptr)
            } else {
                let inner_parser = self.build_array_parser_get(parser)?;
                let result_parser = self.build_array_parser_get(ret_buf)?;
                self.build_copy_invariant(result_parser, inner_parser)?;
                self.build_array_slice_get(ret_buf)?
            };
            let inner_slice_ret = self.build_return_value(inner_slice)?;
            let ret = self.call_span_fun(inner_slice_ret, arg_copy, arg)?;
            self.non_zero_early_return(llvm_fun, ret)?;
            self.terminate_tail_typecast(ret_buf.into(), ret_val)?;
        } else {
            self.builder
                .build_return(Some(&self.const_i64(ReturnStatus::Ok as i64)))?;
        };
        Ok(llvm_fun)
    }

    fn create_field_access(
        &mut self,
        layout: IMonoLayout<'comp>,
        field: DefId,
        name: Identifier,
    ) -> IResult<()> {
        let fun = self.access_field_fun_val(layout, name);
        self.add_entry_block(fun);
        let [return_ptr, block, target_head] = get_fun_args(fun);
        let block = CgMonoValue::new(layout, block.into_pointer_value());
        let return_val = CgReturnValue::new(
            target_head.into_int_value(),
            return_ptr.into_pointer_value(),
        );
        let (id, inner_layout) = if let MonoLayout::Block(id, fields) = &layout.mono_layout().0 {
            (id, fields[&FieldName::Ident(name)])
        } else {
            dbpanic!(
                &self.compiler_database.db,
                "called create_field_access on non-block {}",
                &layout.inner()
            );
        };
        if let Some((ptr, mask)) =
            self.build_discriminant_info(*id, block.into(), FieldName::Ident(name))?
        {
            let next_bb = self.llvm.append_basic_block(fun, "next");
            let early_exit_bb = self.llvm.append_basic_block(fun, "early_exit");
            let is_disc_set = self.build_discriminant_check(ptr, mask)?;
            self.builder
                .build_conditional_branch(is_disc_set, next_bb, early_exit_bb)?;
            self.builder.position_at_end(early_exit_bb);
            self.builder
                .build_return(Some(&self.const_i64(ReturnStatus::Backtrack as i64)))?;
            self.builder.position_at_end(next_bb);
        }
        let field = self.build_field_gep(field, block.into(), inner_layout)?;
        self.terminate_tail_typecast(field, return_val)
    }

    fn create_const_len_fun(&mut self, layout: IMonoLayout<'comp>, len: i64) -> IResult<()> {
        let fun = self.parser_len_fun_val(layout);
        let [return_ptr, _] = get_fun_args(fun);
        self.add_entry_block(fun);
        let llvm_len = self.const_i64(len);
        let return_ptr = self.build_cast::<*mut i64, _>(return_ptr)?;
        self.builder.build_store(return_ptr, llvm_len)?;
        self.builder
            .build_return(Some(&self.const_i64(ReturnStatus::Ok as i64)))?;
        Ok(())
    }

    fn create_fail_len_fun(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.parser_len_fun_val(layout);
        self.add_entry_block(fun);
        self.builder
            .build_return(Some(&self.const_i64(ReturnStatus::Error as i64)))?;
        Ok(())
    }

    fn create_array_parser_len_fun(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.parser_len_fun_val(layout);
        self.add_entry_block(fun);
        let [return_ptr, fun_ptr] = get_fun_args(fun).map(|v| v.into_pointer_value());
        let parser = CgMonoValue::new(layout, fun_ptr);
        let int_buf = self.build_alloca_int("int_buf")?;
        let (_, len_offset) = self.arg_level_and_offset(layout, 0);
        let len_ptr =
            self.build_byte_gep(fun_ptr, self.const_size_t(len_offset as i64), "len_ptr")?;
        let len = self.build_i64_load(len_ptr, "len")?;

        let inner_parser = self.build_array_parser_get(parser)?;
        let status = self.call_len_fun(int_buf.ptr, inner_parser)?;
        self.non_zero_early_return(fun, status)?;
        let parser_len = self.build_i64_load(int_buf.ptr, "parser_len")?;

        let full_len = self.builder.build_int_mul(len, parser_len, "full_len")?;
        let return_ptr = self.build_cast::<*mut i64, _>(return_ptr)?;
        self.builder.build_store(return_ptr, full_len)?;
        self.builder
            .build_return(Some(&self.const_i64(ReturnStatus::Ok as i64)))?;
        Ok(())
    }

    fn create_mir_len_fun(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let fun = self.parser_len_fun_val(layout);
        let [return_ptr, fun_ptr] = get_fun_args(fun).map(|v| v.into_pointer_value());
        let int_layout = self.layouts.dcx.int(&self.compiler_database.db);
        let int_value = CgValue::new(int_layout, self.any_ptr().const_null());
        let fun_value = CgMonoValue::new(layout, fun_ptr);
        let head = self.const_i64(DerefLevel::zero().into_shifted_runtime_value() as i64);
        let ret_value = CgReturnValue::new(head, return_ptr);
        let fun_kind = match layout.mono_layout().0 {
            MonoLayout::NominalParser(pd, ..) => FunKind::ParserDef(*pd),
            MonoLayout::BlockParser(bd, ..) => FunKind::Block(*bd),
            MonoLayout::IfParser(_, constr, wiggle) => {
                FunKind::If(*constr, layout.mono_layout().1, *wiggle)
            }
            _ => dbpanic!(
                &self.compiler_database.db,
                "called create_mir_len_fun on non-parser {}",
                &layout.inner()
            ),
        };
        let mir = function_substitute(
            fun_kind,
            MirKind::Len,
            Some(int_layout),
            layout,
            self.layouts,
        )
        .unwrap();
        MirTranslator::new(self, Rc::new(mir), fun, fun_value, int_value)?
            .with_ret_val(ret_value)
            .build()?;
        Ok(())
    }

    fn create_len_fun(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        match layout.mono_layout().0 {
            MonoLayout::Single => self.create_const_len_fun(layout, 1),
            MonoLayout::Nil => self.create_const_len_fun(layout, 0),
            MonoLayout::Regex(regex, _) => {
                if let Some(len) = self.compiler_database.db.regex_len(*regex).unwrap() {
                    self.create_const_len_fun(layout, len as i64)
                } else {
                    self.create_fail_len_fun(layout)
                }
            }
            MonoLayout::ArrayParser(_) => self.create_array_parser_len_fun(layout),
            MonoLayout::IfParser(_, _, _)
            | MonoLayout::NominalParser(_, _, _)
            | MonoLayout::BlockParser(_, _, _, _) => self.create_mir_len_fun(layout),
            _ => dbpanic!(
                &self.compiler_database.db,
                "called create_len_fun on non-parser {}",
                &layout.inner()
            ),
        }?;
        Ok(())
    }

    fn create_eval_block(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let MonoLayout::BlockParser(block, _, _, _) = layout.mono_layout().0 else {
            panic!("Expected block parser layout")
        };
        let block = block.lookup(&self.compiler_database.db).unwrap();
        let mir_fun = Rc::new(self.mir_block(None, layout, fun_req()));
        let impl_fun = self.eval_fun_fun_val(layout);

        let (ret, fun) = eval_fun_values(impl_fun, layout);
        let undef = self.undef_val();
        let mut translator = MirTranslator::new(self, mir_fun, impl_fun, fun, undef)?;
        translator = translator.with_ret_val(ret);
        translator.build()?;

        // if the block function does not return itself, we do not need to write the block vtable
        if block.returns {
            let llvm_fun = self.eval_fun_fun_val_wrapper(layout);
            self.wrap_direct_call(impl_fun, llvm_fun, false)?;
            return Ok(());
        }

        let return_layout = self.layouts.block_result()[&(None, layout.inner())]
            .val()
            .as_ref()
            .unwrap()
            .returned;
        let mono_layout = return_layout.maybe_mono().unwrap();

        let block_data = BlockThunk {
            from: None,
            fun: layout,
            result: mono_layout,
            req: fun_req(),
        };

        ThunkContext::new(self, block_data).build()?;
        Ok(())
    }

    fn create_eval_pd_fun_fun_impl(
        &mut self,
        layout: IMonoLayout<'comp>,
    ) -> IResult<FunctionValue<'llvm>> {
        let llvm_fun = self.eval_fun_fun_val(layout);
        let arg = self.undef_val();
        let mir_fun = Rc::new(self.mir_pd_fun(layout)?);
        let (ret, fun) = eval_fun_values(llvm_fun, layout);
        let mut translator = MirTranslator::new(self, mir_fun, llvm_fun, fun, arg)?;
        translator = translator.with_ret_val(ret);
        translator.build()
    }

    fn create_eval_fun_fun_copy(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let target_layout = layout
            .inner()
            .eval_fun(self.layouts)
            .unwrap()
            .maybe_mono()
            .unwrap();
        let f = self.eval_fun_fun_val_wrapper(layout);
        self.add_entry_block(f);
        let create_args_thunk = TransmuteCopyThunk {
            from: layout,
            to: target_layout,
            f,
        };
        ThunkContext::new(self, create_args_thunk).build()?;
        Ok(())
    }

    fn create_eval_fun_fun(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let impl_fun = match layout.mono_layout().0 {
            MonoLayout::ArrayParser(_) => return self.create_eval_fun_fun_copy(layout),
            MonoLayout::NominalParser(pd, ..) => {
                let pd = pd.lookup(&self.compiler_database.db).unwrap();
                if pd.from.is_some() {
                    return self.create_eval_fun_fun_copy(layout);
                }
                self.create_eval_pd_fun_fun_impl(layout)?
            }
            MonoLayout::BlockParser(..) => return self.create_eval_block(layout),
            _ => dbpanic!(
                &self.compiler_database.db,
                "called create_eval_fun_fun on non-parser {}",
                &layout.inner()
            ),
        };
        let llvm_fun = self.eval_fun_fun_val_wrapper(layout);
        self.wrap_direct_call(impl_fun, llvm_fun, false)?;
        Ok(())
    }

    fn create_create_fun_args_fun(
        &mut self,
        layout: IMonoLayout<'comp>,
        args: ILayout<'comp>,
        slot: u64,
    ) -> IResult<()> {
        let args = if let MonoLayout::Tuple(args) = args.maybe_mono().unwrap().mono_layout().0 {
            args
        } else {
            dbpanic!(
                &self.compiler_database.db,
                "called create_fun_create with non-tuple args {}",
                &args
            );
        };
        let fun_type = self
            .compiler_database
            .db
            .lookup_intern_type(layout.mono_layout().1);
        let return_layout = if let Type::FunctionArg(_, arg_tys) = fun_type {
            let new_args = args.iter().copied().zip(arg_tys.iter().copied());
            let result = layout.inner().apply_fun(self.layouts, new_args).unwrap();
            result.maybe_mono().unwrap()
        } else {
            dbpanic!(
                &self.compiler_database.db,
                "called create_fun_create with non-nominal parser {}",
                &layout.inner()
            );
        };
        let f = self.function_create_args_fun_val(layout, slot);
        self.add_entry_block(f);
        ThunkContext::new(
            self,
            TransmuteCopyThunk {
                from: layout,
                to: return_layout,
                f,
            },
        )
        .build()?;
        Ok(())
    }

    fn create_all_header_funs(&mut self) -> IResult<()> {
        let collected_layouts = self.collected_layouts.clone();
        for layout in [
            &collected_layouts.arrays,
            &collected_layouts.blocks,
            &collected_layouts.nominals,
            &collected_layouts.parsers,
            &collected_layouts.primitives,
            &collected_layouts.functions,
        ]
        .into_iter()
        .flatten()
        {
            self.create_typecast(*layout)?;
            self.create_mask_funs(*layout)?;
        }
        Ok(())
    }

    fn create_parser_funs(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let collected_layouts = self.collected_layouts.clone();
        let mut visited = FxHashSet::default();
        for &(from, meta) in collected_layouts
            .parser_slots
            .occupied_entries
            .get(&layout)
            .cloned()
            .unwrap_or_default()
            .values()
        {
            if !visited.insert((from, meta.req)) {
                continue;
            }
            let mut create_fun = match layout.mono_layout().0 {
                MonoLayout::Single => Self::create_single_parse,
                MonoLayout::Nil => Self::create_nil_parse,
                MonoLayout::NominalParser(..) => Self::create_pd_parse,
                MonoLayout::BlockParser(..) => Self::create_block_parse,
                MonoLayout::Regex(..) => Self::create_regex_parse,
                MonoLayout::IfParser(..) => Self::create_if_parse,
                MonoLayout::ArrayParser(..) => Self::create_array_parse,
                _ => panic!("non-parser in parser layout collection"),
            };
            // if the from arg is an integer, that means that we created a int parse during
            // collection, which should only happen when a place that gets instantiated with
            // an undefined value has a thunk layout.
            // in this case the parse call gets created by the vtable even though the value
            // actually never gets created, so we need to create a parse call that just returns
            // an error
            if from.is_int() {
                create_fun = Self::create_error_parse
            }
            let fun = create_fun(self, from, layout, meta.req)?;
            self.create_wrapper_parse(from, layout, meta.req, fun)?;
        }
        Ok(())
    }

    fn create_funcalls(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let Type::FunctionArg(_, args) = self.layouts.db.lookup_intern_type(layout.mono_layout().1)
        else {
            panic!("attempting to create funcalls of non-function layout")
        };
        if args.is_empty() {
            self.create_eval_fun_fun(layout)?;
        }
        let collected_layouts = self.collected_layouts.clone();
        for (slot, args) in collected_layouts
            .funcall_slots
            .occupied_entries
            .get(&layout)
            .cloned()
            .unwrap_or_default()
            .iter()
        {
            self.create_create_fun_args_fun(layout, *args, *slot)?;
        }
        Ok(())
    }

    fn create_block_funs(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        let id = if let MonoLayout::Block(id, _) = layout.mono_layout().0 {
            *id
        } else {
            panic!("attempting to create block funs of non-block layout")
        };
        let fields = self
            .compiler_database
            .db
            .sorted_block_fields(id, false)
            .unwrap();
        let root_ctx = id
            .lookup(&self.compiler_database.db)
            .unwrap()
            .root_context
            .lookup(&self.compiler_database.db)
            .unwrap();
        let def_ids: Vec<_> = fields
            .iter()
            .map(|x| (*x, *root_ctx.vars.get(*x).unwrap().inner()))
            .collect();
        for (name, def_id) in def_ids {
            if let FieldName::Ident(name) = name {
                self.create_field_access(layout, def_id, name)?;
            }
        }
        Ok(())
    }

    fn create_array_funs(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        for create in match layout.mono_layout().0 {
            MonoLayout::SlicePtr => [
                Self::create_sliceptr_current_element,
                Self::create_sliceptr_single_forward,
                Self::create_sliceptr_len,
                Self::create_sliceptr_skip,
                Self::create_sliceptr_span,
                Self::create_sliceptr_inner_array,
            ],
            MonoLayout::Array { .. } => [
                Self::create_array_current_element,
                Self::create_array_single_forward,
                Self::create_array_len,
                Self::create_array_skip,
                Self::create_array_span,
                Self::create_array_inner_array,
            ],
            _ => panic!("attempting to create array funs of non-array layout"),
        } {
            create(self, layout)?
        }
        Ok(())
    }

    fn create_primitive_funs(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        if let MonoLayout::Primitive(PrimitiveType::U8) = layout.mono_layout().0 {
            self.create_u8_current_element(layout)?;
        }
        Ok(())
    }

    fn create_nominal_funs(&mut self, layout: IMonoLayout<'comp>) -> IResult<()> {
        self.create_pd_end(layout)?;
        self.create_pd_start(layout)?;
        Ok(())
    }

    fn create_init_fun(&mut self) -> IResult<()> {
        let llvm_fun = self.global_constant_init_fun_val();
        self.add_entry_block(llvm_fun);
        let global_sequence = self.compiler_database.db.global_sequence().unwrap();
        for pd in global_sequence.iter() {
            let Some(&(fun, layout)) = self.collected_layouts.globals.get(pd) else {
                continue;
            };
            let Type::FunctionArg(ret, _) = self
                .compiler_database
                .db
                .lookup_intern_type(fun.mono_layout().1)
            else {
                panic!("attempting to create init function with non-function type stored in layout")
            };
            let level = self.layouts.db.deref_level(ret).unwrap();
            let global = self.global_constant(*pd);
            let mut head = level.into_shifted_runtime_value();
            head |= (layout.is_multi() as u64) << VTABLE_BIT;
            let ret_val = CgReturnValue::new(self.const_i64(head as i64), global);
            let fun_val = CgValue::new(fun.inner(), self.any_ptr().const_null());
            let status = self.call_eval_fun_fun(ret_val, fun_val, ParserFunKind::Wrapper)?;
            self.non_zero_early_return(llvm_fun, status)?;
        }
        let zero = self.const_i64(0);
        self.builder.build_return(Some(&zero))?;
        Ok(())
    }

    pub fn create_all_funs(&mut self) -> IResult<()> {
        let collected_layouts = self.collected_layouts.clone();
        self.create_all_header_funs()?;
        self.create_init_fun()?;
        for layout in collected_layouts.arrays.iter() {
            self.create_array_funs(*layout)?;
        }
        for layout in collected_layouts.blocks.iter() {
            self.create_block_funs(*layout)?;
        }
        for layout in collected_layouts.nominals.iter() {
            self.create_nominal_funs(*layout)?;
        }
        for layout in collected_layouts.parsers.iter() {
            self.create_parser_funs(*layout)?;
        }
        for layout in collected_layouts.functions.iter() {
            self.create_funcalls(*layout)?;
        }
        for layout in collected_layouts.lens.iter() {
            self.create_len_fun(*layout)?;
        }
        for layout in collected_layouts.primitives.iter() {
            self.create_primitive_funs(*layout)?;
        }
        Ok(())
    }
}
