use yaboc_dependents::NeededBy;
use yaboc_layout::{
    collect::{pd_len_req, pd_val_req},
    represent::ParserFunKind,
};
use yaboc_mir::FunKind;

use crate::{
    convert_regex::RegexTranslator,
    convert_thunk::{BlockThunk, CreateArgsThunk, TypecastThunk, ValThunk},
    defs::TAILCC,
};

use super::*;

impl<'llvm, 'comp> CodeGenCtx<'llvm, 'comp> {
    fn non_zero_early_return(&mut self, fun: FunctionValue<'llvm>, status: IntValue<'llvm>) {
        let success_bb = self.llvm.append_basic_block(fun, "deref_succ");
        let fail_bb = self.llvm.append_basic_block(fun, "deref_fail");
        let is_not_zero = self.builder.build_int_compare(
            IntPredicate::NE,
            status,
            self.const_i64(ReturnStatus::Ok as i64),
            "deref_status_is_zero",
        );
        self.builder
            .build_conditional_branch(is_not_zero, fail_bb, success_bb);

        self.builder.position_at_end(fail_bb);
        self.builder.build_return(Some(&status));

        self.builder.position_at_end(success_bb);
    }

    fn terminate_tail_typecast(&mut self, arg: CgValue<'comp, 'llvm>, ret: CgReturnValue<'llvm>) {
        let ret = self.call_typecast_fun(ret, arg);
        self.builder.build_return(Some(&ret));
    }

    fn wrap_direct_call(
        &mut self,
        fun: FunctionValue<'llvm>,
        wrapper: FunctionValue<'llvm>,
        tail: bool,
    ) -> FunctionValue<'llvm> {
        let args = wrapper
            .get_param_iter()
            .map(|x| x.into())
            .collect::<Vec<_>>();
        if wrapper.get_last_basic_block().is_none() {
            self.add_entry_block(wrapper);
        }
        let call = self.builder.build_call(fun, &args, "call");
        call.set_tail_call(tail);
        call.set_call_convention(TAILCC);
        let ret = call.try_as_basic_value().left().unwrap().into_int_value();
        self.builder.build_return(Some(&ret));
        wrapper
    }

    fn setup_tail_fun_copy(
        &mut self,
        from: ILayout<'comp>,
        fun: CgMonoValue<'comp, 'llvm>,
    ) -> Option<CgMonoValue<'comp, 'llvm>> {
        let Some(sa) = self.collected_layouts.tail_sa[&(from, fun.layout)] else {
            return None
        };
        let fun_copy_ptr = self.build_sa_alloca(sa, Some(false), "fun_copy");
        let fun_buf = CgMonoValue::new(fun.layout, fun_copy_ptr);
        self.build_copy_invariant(fun_buf.into(), fun.into());
        Some(fun_buf)
    }

    fn create_wrapper_parse(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        slot: PSize,
        req: RequirementSet,
        inner: FunctionValue<'llvm>,
    ) -> FunctionValue<'llvm> {
        let wrapper = self.parser_fun_val_wrapper(layout, slot, req);
        let (ret, fun_arg, from) = parser_values(wrapper, layout, from);
        self.add_entry_block(wrapper);
        let Some(val) = self.setup_tail_fun_copy(from.layout, fun_arg) else {
            // cannot be a tail call because of different calling conventions
            return self.wrap_direct_call(inner, wrapper, false);
        };
        let ret = self.build_tailcc_call_with_int_ret(
            inner.into(),
            &[
                ret.ptr.into(),
                val.ptr.into(),
                ret.head.into(),
                from.ptr.into(),
            ],
        );
        self.builder.build_return(Some(&ret));
        wrapper
    }

    fn mir_pd_fun(
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
        let mir = self
            .compiler_database
            .db
            .mir(FunKind::ParserDef(*pd), req)
            .unwrap();
        let strictness = self
            .compiler_database
            .db
            .strictness(FunKind::ParserDef(*pd), req)
            .unwrap();
        FunctionSubstitute::new_from_pd(mir, &strictness, from, layout.inner(), *pd, self.layouts)
            .unwrap()
    }

    fn mir_block_fun(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        mut req: RequirementSet,
    ) -> FunctionSubstitute<'comp> {
        let MonoLayout::BlockParser(bd, _, bt) = layout.mono_layout().0
        else {
            panic!("mir_pd_len_fun has to be called with a nominal parser layout");
        };
        if !bt {
            req &= !NeededBy::Backtrack;
        }
        let mir = self
            .compiler_database
            .db
            .mir(FunKind::Block(*bd), req)
            .unwrap();
        let strictness = self
            .compiler_database
            .db
            .strictness(FunKind::Block(*bd), req)
            .unwrap();
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
        let funkind = FunKind::If(*cid, ty, *wiggle);
        let mir = self.compiler_database.db.mir(funkind, req).unwrap();
        let strictness = self.compiler_database.db.strictness(funkind, req).unwrap();
        FunctionSubstitute::new_from_if(mir, &strictness, from, layout, self.layouts).unwrap()
    }

    fn create_typecast(&mut self, layout: IMonoLayout<'comp>) {
        if let MonoLayout::Nominal(..) = layout.mono_layout().0 {
            let (from, fun) = layout.unapply_nominal(self.layouts);
            let slot = self.collected_layouts.parser_slots.layout_vtable_offsets
                [&((from, pd_val_req()), fun.inner())];
            self.create_pd_parse_impl(from, fun, slot, pd_val_req().req);
        }
        let thunk_info = TypecastThunk::new(self, layout);
        ThunkContext::new(self, thunk_info).build();
    }

    fn create_pd_parse(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        slot: PSize,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        let impl_fun = self.create_pd_parse_impl(from, layout, slot, req);

        let MonoLayout::NominalParser(pd, args, _) = layout.mono_layout().0 else {
            panic!("create_pd_parse has to be called with a nominal parser layout");
        };
        let Type::ParserArg { arg: arg_ty, .. } = self.compiler_database.db.lookup_intern_type(layout.mono_layout().1) else {
            panic!("create_pd_parse has to be called with a nominal parser layout");
        };

        let thunky = pd.lookup(&self.compiler_database.db).unwrap().thunky;

        if !req.contains(NeededBy::Val) || !thunky {
            // just call impl_fun and return
            let llvm_fun = self.parser_fun_val_tail(layout, slot, req);
            return self.wrap_direct_call(impl_fun, llvm_fun, true);
        }

        if !(req & !NeededBy::Val).is_empty() {
            self.create_pd_parse_impl(from, layout, slot, req & !NeededBy::Val);
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
        let val_thunk_info = ValThunk::new(from, layout, return_layout, slot, req);
        ThunkContext::new(self, val_thunk_info).build()
    }

    fn create_pd_parse_impl(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        slot: PSize,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        let sym = self.sym(layout, LayoutPart::Parse(slot, req, ParserFunKind::Worker));
        let llvm_fun = match self.module.get_function(&sym) {
            Some(f) => return f,
            None => self.parser_impl_fun_val(layout, slot, req),
        };
        let mir_fun = Rc::new(self.mir_pd_fun(from, layout, req));
        let (ret, fun, arg) = parser_values(llvm_fun, layout, from);
        let mut translator = MirTranslator::new(self, mir_fun, llvm_fun, fun, arg);
        if req.contains(NeededBy::Val) {
            translator = translator.with_ret_val(ret);
        }
        translator.build()
    }

    fn create_pd_end(&mut self, layout: IMonoLayout<'comp>) {
        let llvm_fun = self.end_fun_val(layout);
        self.add_entry_block(llvm_fun);
        let [ret, arg] = get_fun_args(llvm_fun).map(|x| x.into_pointer_value());
        let arg = CgMonoValue::new(layout, arg);
        let start_copy = self.start_fun_val(layout);
        self.builder
            .build_call(start_copy, &[ret.into(), arg.ptr.into()], "");
        let (from, fun, _) = self.build_nominal_components(arg, pd_len_req());
        let no_ret = self.undef_ret();
        let from_ret = from.with_ptr(ret);
        let ret = self.build_parser_call(no_ret, fun.into(), from_ret, pd_len_req());
        self.builder.build_return(Some(&ret));
    }

    fn create_pd_start(&mut self, layout: IMonoLayout<'comp>) {
        let fun = self.start_fun_val(layout);
        self.add_entry_block(fun);
        let [to, from] = get_fun_args(fun).map(|x| x.into_pointer_value());
        let (from_layout, _) = layout.unapply_nominal(self.layouts);
        let sa = from_layout
            .size_align(self.layouts)
            .expect("Could not get size/alignment of layout");
        let size = self
            .llvm
            .ptr_sized_int_type(&self.target_data, None)
            .const_int(sa.size, false);
        let align = sa.align();
        self.builder
            .build_memcpy(to, align as u32, from, align as u32, size)
            .unwrap();
        self.builder
            .build_return(Some(&self.const_i64(ReturnStatus::Ok as i64)));
    }

    fn get_slice_ptrs(&mut self, arg: PointerValue<'llvm>) -> [PointerValue<'llvm>; 2] {
        let arg_ptr = self.build_cast::<*mut *const u8, _>(arg);
        let ptr = self
            .builder
            .build_load(arg_ptr, "load_ptr")
            .into_pointer_value();
        let end_ptr_ptr = unsafe {
            self.builder
                .build_in_bounds_gep(arg_ptr, &[self.const_i64(1)], "end_ptr_ptr")
        };
        let end_ptr = self
            .builder
            .build_load(end_ptr_ptr, "load_end_ptr")
            .into_pointer_value();
        [ptr, end_ptr]
    }

    fn create_array_single_forward(&mut self, layout: IMonoLayout<'comp>) {
        let fun = self.single_forward_fun_val(layout);
        self.set_always_inline(fun);

        let [arg] = get_fun_args(fun).map(|x| x.into_pointer_value());
        self.add_entry_block(fun);
        let arg_ptr = self.build_cast::<*mut *const u8, _>(arg);
        let ptr = self
            .builder
            .build_load(arg_ptr, "load_ptr")
            .into_pointer_value();
        let inc_ptr = self.build_byte_gep(ptr, self.const_i64(1), "inc_ptr");
        self.builder.build_store(arg_ptr, inc_ptr);
        self.builder
            .build_return(Some(&self.const_i64(ReturnStatus::Ok as i64)));
    }

    fn create_array_skip(&mut self, layout: IMonoLayout<'comp>) {
        let fun = self.skip_fun_val(layout);
        self.set_always_inline(fun);
        let entry = self.llvm.append_basic_block(fun, "entry");
        self.builder.position_at_end(entry);
        let [arg, len] = get_fun_args(fun);
        let len = len.into_int_value();
        let arg = arg.into_pointer_value();
        let arg_ptr = self.build_cast::<*mut *const u8, _>(arg);
        let [arg_start, _] = self.get_slice_ptrs(arg);
        let inc_ptr = self.build_byte_gep(arg_start, len, "inc_ptr");
        self.builder.build_store(arg_ptr, inc_ptr);
        self.builder
            .build_return(Some(&self.const_i64(ReturnStatus::Ok as i64)));
    }

    fn create_array_span(&mut self, layout: IMonoLayout<'comp>) {
        let fun = self.span_fun_val(layout);
        self.set_always_inline(fun);
        self.add_entry_block(fun);
        let [ret, start, head, end] = get_fun_args(fun);
        let [ret, start, end] = [ret, start, end].map(|x| x.into_pointer_value());
        let ret = CgReturnValue::new(head.into_int_value(), ret);
        let buf = self.build_alloca_value(layout.inner(), "buf");
        let [start_ptr, _] = self.get_slice_ptrs(start);
        let [end_ptr, _] = self.get_slice_ptrs(end);
        let bufsl = self.build_cast::<*mut *const u8, _>(buf.ptr);
        self.builder.build_store(bufsl, start_ptr);
        let bufsl = unsafe {
            self.builder
                .build_in_bounds_gep(bufsl, &[self.const_i64(1)], "ret")
        };
        self.builder.build_store(bufsl, end_ptr);
        self.terminate_tail_typecast(buf, ret)
    }

    fn create_array_current_element(&mut self, layout: IMonoLayout<'comp>) {
        let fun = self.current_element_fun_val(layout);
        self.set_always_inline(fun);
        let entry = self.llvm.append_basic_block(fun, "entry");
        self.builder.position_at_end(entry);
        let int_layout = canon_layout(
            self.layouts,
            self.compiler_database
                .db
                .intern_type(Type::Primitive(PrimitiveType::Int)),
        )
        .unwrap();
        let int_buf = self.build_alloca_value(int_layout, "int_buf");
        let [return_ptr, from, target_head] = get_fun_args(fun);
        let from = self.build_cast::<*const *const u8, _>(from);
        let ret = CgReturnValue::new(
            target_head.into_int_value(),
            return_ptr.into_pointer_value(),
        );
        let int_ptr = self
            .builder
            .build_load(from, "load_ptr")
            .into_pointer_value();
        let byte = self
            .builder
            .build_load(int_ptr, "load_byte")
            .into_int_value();
        let int = self
            .builder
            .build_int_z_extend(byte, self.llvm.i64_type(), "int");
        let bitcasted_buf = self.build_cast::<*mut i64, _>(int_buf.ptr);
        self.builder.build_store(bitcasted_buf, int);
        self.terminate_tail_typecast(int_buf, ret);
    }

    fn create_block_parse(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        slot: PSize,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        let MonoLayout::BlockParser(block, _, _) = layout.mono_layout().0 else {
            panic!("Expected block parser layout")
        };
        let block = block.lookup(&self.compiler_database.db).unwrap();
        let llvm_fun = self.parser_fun_val_tail(layout, slot, req);
        let mir_fun = Rc::new(self.mir_block_fun(from, layout, req));
        let impl_fun = self.parser_impl_fun_val(layout, slot, req);

        let (ret, fun, arg) = parser_values(impl_fun, layout, from);
        let mut translator = MirTranslator::new(self, mir_fun, impl_fun, fun, arg);
        if req.contains(NeededBy::Val) {
            translator = translator.with_ret_val(ret)
        }
        translator.build();

        if block.returns {
            self.wrap_direct_call(impl_fun, llvm_fun, true);
            return llvm_fun;
        }

        let return_layout = self.layouts.block_result()[&(from, layout.inner())]
            .as_ref()
            .unwrap()
            .returned;
        let mono_layout = flat_layouts(&return_layout).next().unwrap();

        let block_data = BlockThunk {
            from,
            fun: layout,
            result: mono_layout,
            slot,
            req,
        };

        ThunkContext::new(self, block_data).build()
    }

    fn create_if_parse(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        slot: PSize,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        let llvm_fun = self.parser_fun_val_tail(layout, slot, req);
        let mir_fun = Rc::new(self.mir_if_fun(from, layout, req));
        let (ret, fun, arg) = parser_values(llvm_fun, layout, from);
        let mut trans = MirTranslator::new(self, mir_fun, llvm_fun, fun, arg);
        if req.contains(NeededBy::Val) {
            trans = trans.with_ret_val(ret)
        }
        trans.build();
        llvm_fun
    }

    fn create_single_parse(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        slot: PSize,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        let llvm_fun = self.parser_fun_val_tail(layout, slot, req);
        self.set_always_inline(llvm_fun);
        self.add_entry_block(llvm_fun);
        let (ret, _, arg) = parser_values(llvm_fun, layout, from);
        let [ptr, end_ptr] = self.get_slice_ptrs(arg.ptr);
        let fail_block = self.llvm.append_basic_block(llvm_fun, "fail");
        let ok_block = self.llvm.append_basic_block(llvm_fun, "ok");
        let ptr_diff = self.builder.build_ptr_diff(end_ptr, ptr, "ptr_diff");
        let is_zero = self.builder.build_int_compare(
            IntPredicate::EQ,
            ptr_diff,
            self.const_i64(0),
            "is_zero",
        );
        self.builder
            .build_conditional_branch(is_zero, fail_block, ok_block);
        self.builder.position_at_end(fail_block);
        self.builder
            .build_return(Some(&self.const_i64(ReturnStatus::Eof as i64)));
        self.builder.position_at_end(ok_block);
        if req.contains(NeededBy::Val) {
            let ret = self.call_current_element_fun(ret, arg);
            self.non_zero_early_return(llvm_fun, ret)
        }
        if req.contains(NeededBy::Len) {
            let ret = self.call_single_forward_fun(arg);
            self.builder.build_return(Some(&ret));
        } else {
            self.builder.build_return(Some(&self.const_i64(0)));
        }
        llvm_fun
    }

    fn create_nil_parse(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        slot: PSize,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        let llvm_fun = self.parser_fun_val_tail(layout, slot, req);
        self.add_entry_block(llvm_fun);
        self.set_always_inline(llvm_fun);
        let (ret, _, _) = parser_values(llvm_fun, layout, from);
        if req.contains(NeededBy::Val) {
            let unit_type = self
                .compiler_database
                .db
                .intern_type(Type::Primitive(PrimitiveType::Unit));
            let unit_layout = canon_layout(self.layouts, unit_type).unwrap();
            let undef = CgValue::new(unit_layout, self.any_ptr().get_undef());
            self.terminate_tail_typecast(undef, ret);
        } else {
            self.builder.build_return(Some(&self.const_i64(0)));
        }
        llvm_fun
    }

    fn create_regex_parse(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        slot: PSize,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        let MonoLayout::Regex(regex, bt) = layout.mono_layout().0 else {
                panic!("called build_regex_parse on non-regex")
        };
        let regex_str = self.compiler_database.db.lookup_intern_regex(*regex);
        let regex_impl = self.create_regex_parse_impl(from, layout, &regex_str, slot, req);
        let llvm_fun = self.parser_fun_val_tail(layout, slot, req);
        self.add_entry_block(llvm_fun);
        let (ret, fun, arg) = parser_values(llvm_fun, layout, from);
        let ret_copy = if !req.contains(NeededBy::Val) {
            let buf_ptr = self.build_layout_alloca(from, "ret_copy");
            self.undef_ret().with_ptr(buf_ptr)
        } else {
            ret
        };
        let arg_copy = if !req.contains(NeededBy::Len) {
            let a = self.build_alloca_value(from, "arg_copy");
            self.build_copy_invariant(a, arg);
            a
        } else {
            arg
        };
        let ret = self.build_tailcc_call_with_int_ret(
            regex_impl.into(),
            &[
                ret_copy.ptr.into(),
                fun.ptr.into(),
                ret_copy.head.into(),
                arg_copy.ptr.into(),
            ],
        );
        let ret = if *bt {
            let is_bt = self.builder.build_int_compare(
                IntPredicate::EQ,
                ret,
                self.const_i64(ReturnStatus::Backtrack as i64),
                "is_bt",
            );
            self.builder
                .build_select(
                    is_bt,
                    self.const_i64(ReturnStatus::Error as i64),
                    ret,
                    "btless",
                )
                .into_int_value()
        } else {
            ret
        };
        self.builder.build_return(Some(&ret));
        llvm_fun
    }

    fn create_regex_parse_impl(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        regex: &str,
        slot: PSize,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        let sym = self.sym(layout, LayoutPart::Parse(slot, req, ParserFunKind::Worker));
        let llvm_fun = match self.module.get_function(&sym) {
            Some(f) => return f,
            None => self.parser_impl_fun_val(layout, slot, req),
        };
        let dfa = regex_automata::dense::Builder::new()
            .anchored(true)
            .unicode(false)
            .allow_invalid_utf8(true)
            .dot_matches_new_line(true)
            .case_insensitive(false)
            .minimize(true)
            .build(regex)
            .expect("invalid regex");
        let mut trans = RegexTranslator::new(self, llvm_fun, &dfa, from, true);
        trans.build();
        llvm_fun
    }

    fn create_array_parse(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        slot: PSize,
        req: RequirementSet,
    ) -> FunctionValue<'llvm> {
        let llvm_fun = self.parser_fun_val_tail(layout, slot, req);
        self.add_entry_block(llvm_fun);
        let (ret_val, len_ptr, mut arg) = parser_values(llvm_fun, layout, from);
        let arg_copy = self.build_alloca_value(from, "arg_copy");
        // make sure we don't modify the original arg
        if !req.contains(NeededBy::Len) {
            let arg_second_copy = self.build_alloca_value(from, "arg_second_copy");
            self.build_copy_invariant(arg_second_copy, arg);
            arg = arg_second_copy;
        }
        self.build_copy_invariant(arg_copy, arg);

        let len_ptr = self.build_cast::<*const i64, _>(len_ptr.ptr);
        let len = self.builder.build_load(len_ptr, "len").into_int_value();
        let [start, end] = self.get_slice_ptrs(arg.ptr);
        let slice_len = self.builder.build_ptr_diff(end, start, "slice_len");
        let is_out_of_bounds =
            self.builder
                .build_int_compare(IntPredicate::ULT, slice_len, len, "is_out_of_bounds");
        let succ_block = self.llvm.append_basic_block(llvm_fun, "succ");
        let fail_block = self.llvm.append_basic_block(llvm_fun, "fail");
        self.builder
            .build_conditional_branch(is_out_of_bounds, fail_block, succ_block);
        self.builder.position_at_end(fail_block);
        self.builder
            .build_return(Some(&self.const_i64(ReturnStatus::Eof as i64)));
        self.builder.position_at_end(succ_block);
        let ret = self.call_skip_fun(arg, len);
        self.non_zero_early_return(llvm_fun, ret);
        let ret = if req.contains(NeededBy::Val) {
            self.call_span_fun(ret_val, arg_copy, arg)
        } else {
            self.const_i64(0)
        };
        self.builder.build_return(Some(&ret));
        llvm_fun
    }

    fn create_field_access(&mut self, layout: IMonoLayout<'comp>, field: DefId, name: Identifier) {
        let fun = self.access_field_fun_val(layout, name);
        let entry = self.llvm.append_basic_block(fun, "entry");
        self.builder.position_at_end(entry);
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
            self.build_discriminant_info(*id, block.into(), FieldName::Ident(name))
        {
            let next_bb = self.llvm.append_basic_block(fun, "next");
            let early_exit_bb = self.llvm.append_basic_block(fun, "early_exit");
            let is_disc_set = self.build_discriminant_check(ptr, mask);
            self.builder
                .build_conditional_branch(is_disc_set, next_bb, early_exit_bb);
            self.builder.position_at_end(early_exit_bb);
            self.builder
                .build_return(Some(&self.const_i64(ReturnStatus::Backtrack as i64)));
            self.builder.position_at_end(next_bb);
        }
        let field = self.build_field_gep(field, block.into(), inner_layout);
        self.terminate_tail_typecast(field, return_val);
    }

    fn create_create_fun_args(
        &mut self,
        layout: IMonoLayout<'comp>,
        args: ILayout<'comp>,
        slot: u64,
    ) {
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
            let all_monos = flat_layouts(&result);
            assert_eq!(all_monos.len(), 1);
            all_monos.last().unwrap()
        } else {
            dbpanic!(
                &self.compiler_database.db,
                "called create_fun_create with non-nominal parser {}",
                &layout.inner()
            );
        };
        ThunkContext::new(
            self,
            CreateArgsThunk {
                from: layout,
                to: return_layout,
                slot,
            },
        )
        .build();
    }

    fn create_all_typecast_funs(&mut self) {
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
            self.create_typecast(*layout)
        }
    }

    fn create_parser_funs(&mut self, layout: IMonoLayout<'comp>) {
        let collected_layouts = self.collected_layouts.clone();
        for (slot, &(from, req)) in collected_layouts
            .parser_slots
            .occupied_entries
            .get(&layout)
            .cloned()
            .unwrap_or_default()
            .iter()
        {
            let fun = match layout.mono_layout().0 {
                MonoLayout::Single => Self::create_single_parse,
                MonoLayout::Nil => Self::create_nil_parse,
                MonoLayout::NominalParser(_, _, _) => Self::create_pd_parse,
                MonoLayout::BlockParser(_, _, _) => Self::create_block_parse,
                MonoLayout::Regex(..) => Self::create_regex_parse,
                MonoLayout::IfParser(..) => Self::create_if_parse,
                MonoLayout::ArrayParser(..) => Self::create_array_parse,
                _ => panic!("non-parser in parser layout collection"),
            }(self, from, layout, *slot, req.req);
            self.create_wrapper_parse(from, layout, *slot, req.req, fun);
        }
    }

    fn create_funcalls(&mut self, layout: IMonoLayout<'comp>) {
        let collected_layouts = self.collected_layouts.clone();
        for (slot, args) in collected_layouts
            .funcall_slots
            .occupied_entries
            .get(&layout)
            .cloned()
            .unwrap_or_default()
            .iter()
        {
            self.create_create_fun_args(layout, *args, *slot);
        }
    }

    fn create_block_funs(&mut self, layout: IMonoLayout<'comp>) {
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
                self.create_field_access(layout, def_id, name);
            }
        }
    }

    fn create_array_funs(&mut self, layout: IMonoLayout<'comp>) {
        self.create_array_current_element(layout);
        self.create_array_single_forward(layout);
        self.create_array_skip(layout);
        self.create_array_span(layout);
    }

    fn create_nominal_funs(&mut self, layout: IMonoLayout<'comp>) {
        self.create_pd_end(layout);
        self.create_pd_start(layout);
    }

    pub fn create_all_funs(&mut self) {
        let collected_layouts = self.collected_layouts.clone();
        self.create_all_typecast_funs();
        for layout in collected_layouts.arrays.iter() {
            self.create_array_funs(*layout);
        }
        for layout in collected_layouts.blocks.iter() {
            self.create_block_funs(*layout);
        }
        for layout in collected_layouts.nominals.iter() {
            self.create_nominal_funs(*layout);
        }
        for layout in collected_layouts.parsers.iter() {
            self.create_parser_funs(*layout);
        }
        for layout in collected_layouts.functions.iter() {
            self.create_funcalls(*layout);
        }
    }
}
