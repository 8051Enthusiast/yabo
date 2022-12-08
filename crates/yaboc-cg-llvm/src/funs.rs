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

    fn wrap_tail_typecast(
        &mut self,
        fun: FunctionValue<'llvm>,
        thunk: FunctionValue<'llvm>,
        return_layout: ILayout<'comp>,
    ) {
        let entry = self.llvm.append_basic_block(thunk, "entry");
        self.builder.position_at_end(entry);
        let return_buffer = self.build_layout_alloca(return_layout, "return_buffer");
        let mut args = thunk.get_param_iter().map(|x| x.into()).collect::<Vec<_>>();
        let return_pointer = args.pop().unwrap();
        let target_head = args.pop().unwrap();
        args.push(return_buffer.into());
        let status = self
            .builder
            .build_call(fun, &args, "")
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_int_value();
        self.non_zero_early_return(thunk, status);
        self.terminate_tail_typecast(
            return_layout,
            return_buffer,
            target_head.into_int_value(),
            return_pointer.into_pointer_value(),
        );
    }

    fn terminate_tail_typecast(
        &mut self,
        layout: ILayout<'comp>,
        buffer: PointerValue<'llvm>,
        target_head: IntValue<'llvm>,
        pointer: PointerValue<'llvm>,
    ) {
        let typecast_fun = self.build_typecast_fun_get(layout.maybe_mono(), buffer);
        let mono_pointer = self.build_mono_ptr(buffer, layout);
        let ret = self.build_call_with_int_ret(
            typecast_fun,
            &[mono_pointer.into(), target_head.into(), pointer.into()],
        );
        self.builder.build_return(Some(&ret));
    }

    fn mir_pd_fun(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        call_kind: CallKind,
    ) -> FunctionSubstitute<'comp> {
        let pd = if let MonoLayout::NominalParser(id, _) = layout.mono_layout().0 {
            id
        } else {
            panic!("mir_pd_len_fun has to be called with a nominal parser layout");
        };
        let mir = self
            .compiler_database
            .db
            .mir_pd(*pd, call_kind, PdArgKind::Parse)
            .unwrap();
        FunctionSubstitute::new_from_pd(
            mir,
            from,
            layout.inner(),
            *pd,
            self.layouts,
            PdArgKind::Parse,
            call_kind,
        )
        .unwrap()
    }

    fn mir_pd_thunk_fun(
        &mut self,
        layout: IMonoLayout<'comp>,
        call_kind: CallKind,
    ) -> FunctionSubstitute<'comp> {
        let pd = if let MonoLayout::Nominal(id, _, _) = layout.mono_layout().0 {
            id
        } else {
            panic!("mir_pd_len_fun has to be called with a nominal parser layout");
        };
        let mir = self
            .compiler_database
            .db
            .mir_pd(*pd, call_kind, PdArgKind::Thunk)
            .unwrap();
        let from = ILayout::bottom(&mut self.layouts.dcx);
        FunctionSubstitute::new_from_pd(
            mir,
            from,
            layout.inner(),
            *pd,
            self.layouts,
            PdArgKind::Thunk,
            call_kind,
        )
        .unwrap()
    }

    fn mir_block_fun(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        call_kind: CallKind,
    ) -> FunctionSubstitute<'comp> {
        let bd = if let MonoLayout::BlockParser(id, _) = layout.mono_layout().0 {
            id
        } else {
            panic!("mir_pd_len_fun has to be called with a nominal parser layout");
        };
        let mir = self.compiler_database.db.mir_block(*bd, call_kind).unwrap();
        FunctionSubstitute::new_from_block(mir, from, layout, self.layouts, call_kind).unwrap()
    }

    fn create_typecast(&mut self, layout: IMonoLayout<'comp>) {
        ThunkContext::new(self, ThunkKind::Typecast(layout)).build();
    }

    fn create_pd_len(&mut self, from: ILayout<'comp>, layout: IMonoLayout<'comp>, slot: PSize) {
        let llvm_fun = self.parser_len_fun_val(layout, slot);
        let mir_fun = Rc::new(self.mir_pd_fun(from, layout, CallKind::Len));
        let [fun, arg, ret] = get_fun_args(llvm_fun).map(|x| x.into_pointer_value());
        MirTranslator::new(self, mir_fun, llvm_fun, fun, arg, ret).build();
    }

    fn create_pd_deref(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        let thunk = self.deref_fun_val(layout);
        let deref_impl = self.deref_impl_fun_val(layout);
        let mir_fun = Rc::new(self.mir_pd_thunk_fun(layout, CallKind::Val));
        let [fun, ret] = get_fun_args(deref_impl).map(|x| x.into_pointer_value());
        let arg = self.any_ptr().get_undef();
        let deref = layout
            .deref(self.layouts)
            .unwrap()
            .expect("trying to deref non-deref layout");
        MirTranslator::new(self, mir_fun, deref_impl, fun, arg, ret).build();
        self.wrap_tail_typecast(deref_impl, thunk, deref);
        thunk
    }

    fn create_pd_end(&mut self, layout: IMonoLayout<'comp>) {
        let llvm_fun = self.end_fun_val(layout);
        let mir_fun = Rc::new(self.mir_pd_thunk_fun(layout, CallKind::Len));
        let [arg, ret] = get_fun_args(llvm_fun).map(|x| x.into_pointer_value());
        let fun = self.any_ptr().get_undef();
        MirTranslator::new(self, mir_fun, llvm_fun, fun, arg, ret).build();
    }

    fn create_pd_start(&mut self, layout: IMonoLayout<'comp>) {
        let fun = self.start_fun_val(layout);
        let [from, to] = get_fun_args(fun).map(|x| x.into_pointer_value());
        let entry = self.llvm.append_basic_block(fun, "entry");
        self.builder.position_at_end(entry);
        let sa = layout
            .inner()
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

    fn create_array_single_forward(&mut self, layout: IMonoLayout<'comp>) {
        let fun = self.single_forward_fun_val(layout);
        self.set_always_inline(fun);

        let [from, to] = get_fun_args(fun).map(|x| x.into_pointer_value());
        let entry = self.llvm.append_basic_block(fun, "entry");
        self.builder.position_at_end(entry);
        let from_ptr = self.build_cast::<*const *const u8, _>(from);
        let to_ptr = self.build_cast::<*mut *const u8, _>(to);
        let ptr = self
            .builder
            .build_load(from_ptr, "load_ptr")
            .into_pointer_value();
        let inc_ptr = self.build_byte_gep(ptr, self.const_i64(1), "inc_ptr");
        self.builder.build_store(to_ptr, inc_ptr);
        self.builder
            .build_return(Some(&self.const_i64(ReturnStatus::Ok as i64)));
    }

    fn create_array_skip(&mut self, layout: IMonoLayout<'comp>) {
        let fun = self.skip_fun_val(layout);
        self.set_always_inline(fun);
        let entry = self.llvm.append_basic_block(fun, "entry");
        self.builder.position_at_end(entry);
        let [from, len, to] = get_fun_args(fun);
        let from = from.into_pointer_value();
        let len = len.into_int_value();
        let to = to.into_pointer_value();
        let from_ptr = self.build_cast::<*const *const u8, _>(from);
        let to_ptr = self.build_cast::<*mut *const u8, _>(to);
        let ptr = self
            .builder
            .build_load(from_ptr, "load_ptr")
            .into_pointer_value();
        let inc_ptr = self.build_byte_gep(ptr, len, "inc_ptr");
        self.builder.build_store(to_ptr, inc_ptr);
        self.builder
            .build_return(Some(&self.const_i64(ReturnStatus::Ok as i64)));
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
        let int_buf = self.build_layout_alloca(int_layout, "int_buf");
        let [from, target_head, return_ptr] = get_fun_args(fun);
        let from = self.build_cast::<*const *const u8, _>(from);
        let target_head = target_head.into_int_value();
        let return_ptr = return_ptr.into_pointer_value();
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
        let bitcasted_buf = self.build_cast::<*mut i64, _>(int_buf);
        self.builder.build_store(bitcasted_buf, int);
        self.terminate_tail_typecast(int_layout, int_buf, target_head, return_ptr);
    }

    fn create_block_len(&mut self, from: ILayout<'comp>, layout: IMonoLayout<'comp>, slot: PSize) {
        let llvm_fun = self.parser_len_fun_val(layout, slot);
        let mir_fun = Rc::new(self.mir_block_fun(from, layout, CallKind::Len));
        let [fun, arg, ret] = get_fun_args(llvm_fun).map(|x| x.into_pointer_value());
        MirTranslator::new(self, mir_fun, llvm_fun, fun, arg, ret).build();
    }

    fn create_single_len(&mut self, from: ILayout<'comp>, layout: IMonoLayout<'comp>, slot: PSize) {
        let llvm_fun = self.parser_len_fun_val(layout, slot);
        self.set_always_inline(llvm_fun);
        let [_, arg, ret] = get_fun_args(llvm_fun).map(|x| x.into_pointer_value());
        let entry = self.llvm.append_basic_block(llvm_fun, "entry");
        self.builder.position_at_end(entry);
        let single_forward_fun = self.build_single_forward_fun_get(from.maybe_mono(), arg);
        let from_mono = self.build_mono_ptr(arg, from);
        let ret = self.build_call_with_int_ret(single_forward_fun, &[from_mono.into(), ret.into()]);
        self.builder.build_return(Some(&ret));
    }

    fn create_single_val(&mut self, from: ILayout<'comp>, layout: IMonoLayout<'comp>, slot: PSize) {
        let llvm_fun = self.parser_val_fun_val(layout, slot);
        self.set_always_inline(llvm_fun);
        let [_, arg, target_head, ret] = get_fun_args(llvm_fun);
        let [arg, ret] = [arg, ret].map(|x| x.into_pointer_value());
        let target_head = target_head.into_int_value();
        let entry = self.llvm.append_basic_block(llvm_fun, "entry");
        self.builder.position_at_end(entry);
        let current_elmeent_fun = self.build_current_element_fun_get(from.maybe_mono(), arg);
        let from_mono = self.build_mono_ptr(arg, from);
        let ret = self.build_call_with_int_ret(
            current_elmeent_fun,
            &[from_mono.into(), target_head.into(), ret.into()],
        );
        self.builder.build_return(Some(&ret));
    }

    fn create_nil_len(&mut self, from: ILayout<'comp>, layout: IMonoLayout<'comp>, slot: PSize) {
        let llvm_fun = self.parser_len_fun_val(layout, slot);
        self.set_always_inline(llvm_fun);
        let [_, arg, ret] = get_fun_args(llvm_fun).map(|x| x.into_pointer_value());
        let entry = self.llvm.append_basic_block(llvm_fun, "entry");
        self.builder.position_at_end(entry);
        let arg_size = self.build_size_get(from.maybe_mono(), arg);
        self.builder.build_memcpy(ret, 1, arg, 1, arg_size).unwrap();
        self.builder.build_return(Some(&self.const_i64(0)));
    }

    fn create_nil_val(&mut self, _: ILayout<'comp>, layout: IMonoLayout<'comp>, slot: PSize) {
        let llvm_fun = self.parser_val_fun_val(layout, slot);
        self.set_always_inline(llvm_fun);
        let [_, _, target_head, ret] = get_fun_args(llvm_fun);
        let ret = ret.into_pointer_value();
        let target_head = target_head.into_int_value();
        let entry = self.llvm.append_basic_block(llvm_fun, "entry");
        self.builder.position_at_end(entry);
        let unit_type = self
            .compiler_database
            .db
            .intern_type(Type::Primitive(PrimitiveType::Unit));
        let unit_layout = canon_layout(self.layouts, unit_type).unwrap();
        let null_ptr = self.any_ptr().const_null();
        self.terminate_tail_typecast(unit_layout, null_ptr, target_head, ret);
    }

    fn create_compose_len(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        slot: PSize,
    ) {
        let llvm_fun = self.parser_len_fun_val(layout, slot);
        let [fun, arg, ret] = get_fun_args(llvm_fun).map(|x| x.into_pointer_value());
        let entry = self.llvm.append_basic_block(llvm_fun, "entry");
        self.builder.position_at_end(entry);
        let (first_layout, inner_ty, second_layout) =
            if let MonoLayout::ComposedParser(first, inner_ty, second) = layout.mono_layout().0 {
                (*first, *inner_ty, *second)
            } else {
                panic!("called build_compose_len on non-composed")
            };
        let first_slot =
            self.collected_layouts.parser_slots.layout_vtable_offsets[&(from, first_layout)];
        let inner_layout = first_layout.apply_arg(self.layouts, from).unwrap();
        let inner_head = self.const_i64(self.compiler_database.db.head_discriminant(inner_ty));
        let second_slot = self.collected_layouts.parser_slots.layout_vtable_offsets
            [&(inner_layout, second_layout)];
        let second_arg = self.build_layout_alloca(inner_layout, "second_arg");
        let second_len_ret = self.build_layout_alloca(inner_layout, "second_len_ret");
        let first_ptr = self.build_duple_gep(layout.inner(), DupleField::First, fun);
        let second_ptr = self.build_duple_gep(layout.inner(), DupleField::Second, fun);
        let [first_len_ptr, first_val_ptr] = [CallKind::Len, CallKind::Val].map(|call_kind| {
            self.build_parser_fun_get(first_layout.maybe_mono(), first_ptr, first_slot, call_kind)
        });
        let first_mono = self.build_mono_ptr(first_ptr, first_layout);
        let ret = self
            .build_call_with_int_ret(first_len_ptr, &[first_mono.into(), arg.into(), ret.into()]);
        self.non_zero_early_return(llvm_fun, ret);
        let ret = self.build_call_with_int_ret(
            first_val_ptr,
            &[
                first_mono.into(),
                arg.into(),
                inner_head.into(),
                second_arg.into(),
            ],
        );
        self.non_zero_early_return(llvm_fun, ret);
        let second_len_ptr = self.build_parser_fun_get(
            second_layout.maybe_mono(),
            second_ptr,
            second_slot,
            CallKind::Len,
        );
        let second_mono = self.build_mono_ptr(second_ptr, second_layout);
        let ret = self.build_call_with_int_ret(
            second_len_ptr,
            &[second_mono.into(), second_arg.into(), second_len_ret.into()],
        );
        self.builder.build_return(Some(&ret));
    }

    fn create_compose_val(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        slot: PSize,
    ) {
        let llvm_fun = self.parser_val_fun_val(layout, slot);
        let entry = self.llvm.append_basic_block(llvm_fun, "entry");
        self.builder.position_at_end(entry);
        let [fun, arg, target_head, return_ptr] = get_fun_args(llvm_fun);
        let [fun, arg, return_ptr] = [fun, arg, return_ptr].map(|x| x.into_pointer_value());
        let target_head = target_head.into_int_value();
        let (first_layout, inner_ty, second_layout) =
            if let MonoLayout::ComposedParser(first, inner_ty, second) = layout.mono_layout().0 {
                (*first, *inner_ty, *second)
            } else {
                panic!("called build_compose_len on non-composed")
            };
        let first_slot =
            self.collected_layouts.parser_slots.layout_vtable_offsets[&(from, first_layout)];
        let inner_layout = first_layout.apply_arg(self.layouts, from).unwrap();
        let inner_head = self.const_i64(self.compiler_database.db.head_discriminant(inner_ty));
        let second_slot = self.collected_layouts.parser_slots.layout_vtable_offsets
            [&(inner_layout, second_layout)];
        let second_arg = self.build_layout_alloca(inner_layout, "second_arg");
        let first_ptr = self.build_duple_gep(layout.inner(), DupleField::First, fun);
        let second_ptr = self.build_duple_gep(layout.inner(), DupleField::Second, fun);
        let first_val_ptr = self.build_parser_fun_get(
            first_layout.maybe_mono(),
            first_ptr,
            first_slot,
            CallKind::Val,
        );
        let first_mono = self.build_mono_ptr(first_ptr, first_layout);
        let ret = self.build_call_with_int_ret(
            first_val_ptr,
            &[
                first_mono.into(),
                arg.into(),
                inner_head.into(),
                second_arg.into(),
            ],
        );
        self.non_zero_early_return(llvm_fun, ret);
        let second_len_ptr = self.build_parser_fun_get(
            second_layout.maybe_mono(),
            second_ptr,
            second_slot,
            CallKind::Val,
        );
        let second_mono = self.build_mono_ptr(second_ptr, second_layout);
        let ret = self.build_call_with_int_ret(
            second_len_ptr,
            &[
                second_mono.into(),
                second_arg.into(),
                target_head.into(),
                return_ptr.into(),
            ],
        );
        self.builder.build_return(Some(&ret));
    }

    fn create_pd_val(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        slot: PSize,
        pd: ParserDefId,
    ) {
        let llvm_fun = self.parser_val_impl_fun_val(layout, slot);
        let mir_fun = Rc::new(self.mir_pd_fun(from, layout, CallKind::Val));
        let [fun, arg, ret] = get_fun_args(llvm_fun).map(|x| x.into_pointer_value());
        MirTranslator::new(self, mir_fun, llvm_fun, fun, arg, ret).build();
        let thunk = self.parser_val_fun_val(layout, slot);
        let result = self
            .compiler_database
            .db
            .parser_result(layout.mono_layout().1)
            .expect("pd parsers type is not parser");
        let mut map = FxHashMap::default();
        map.insert(Arg::From, from);
        if let MonoLayout::NominalParser(pd, args) = layout.mono_layout().0 {
            let parserdef_args = pd
                .lookup(&self.compiler_database.db)
                .unwrap()
                .args
                .unwrap_or_default();
            for (idx, (arg, _)) in args.iter().enumerate() {
                map.insert(Arg::Named(parserdef_args[idx].0), *arg);
            }
        }
        let return_layout = ILayout::make_thunk(self.layouts, pd, result, &map).unwrap();
        self.wrap_tail_typecast(llvm_fun, thunk, return_layout);
    }

    fn create_block_val(&mut self, from: ILayout<'comp>, layout: IMonoLayout<'comp>, slot: PSize) {
        let llvm_fun = self.parser_val_impl_fun_val(layout, slot);
        let mir_fun = Rc::new(self.mir_block_fun(from, layout, CallKind::Val));
        let [fun, arg, ret] = get_fun_args(llvm_fun).map(|x| x.into_pointer_value());
        MirTranslator::new(self, mir_fun, llvm_fun, fun, arg, ret).build();
        let thunk = self.parser_val_fun_val(layout, slot);
        let return_layout = self.layouts.block_result()[&(from, layout.inner())]
            .as_ref()
            .unwrap()
            .returned;
        self.wrap_tail_typecast(llvm_fun, thunk, return_layout);
    }

    fn create_field_access(&mut self, layout: IMonoLayout<'comp>, field: DefId, name: Identifier) {
        let fun = self.access_field_fun_val(layout, name);
        let entry = self.llvm.append_basic_block(fun, "entry");
        self.builder.position_at_end(entry);
        let [block, target_head, return_ptr] = get_fun_args(fun);
        let block = block.into_pointer_value();
        let target_head = target_head.into_int_value();
        let return_ptr = return_ptr.into_pointer_value();
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
            self.build_discriminant_info(*id, block, layout.inner(), FieldName::Ident(name))
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
        let field_ptr = self.build_field_gep(layout.inner(), field, block);
        self.terminate_tail_typecast(inner_layout, field_ptr, target_head, return_ptr);
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
            let new_args = args
                .iter()
                .copied()
                .zip(arg_tys.iter().copied())
                .collect::<Vec<_>>();
            let result = layout.inner().apply_fun(self.layouts, &new_args).unwrap();
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
            ThunkKind::CreateArgs {
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
        for (slot, &from) in collected_layouts
            .parser_slots
            .occupied_entries
            .get(&layout)
            .cloned()
            .unwrap_or_default()
            .iter()
        {
            match layout.mono_layout().0 {
                MonoLayout::Single => {
                    self.create_single_len(from, layout, *slot);
                    self.create_single_val(from, layout, *slot);
                }
                MonoLayout::Nil => {
                    self.create_nil_len(from, layout, *slot);
                    self.create_nil_val(from, layout, *slot);
                }
                MonoLayout::NominalParser(pd, _) => {
                    self.create_pd_len(from, layout, *slot);
                    self.create_pd_val(from, layout, *slot, *pd);
                }
                MonoLayout::BlockParser(_, _) => {
                    self.create_block_len(from, layout, *slot);
                    self.create_block_val(from, layout, *slot);
                }
                MonoLayout::ComposedParser(_, _, _) => {
                    self.create_compose_len(from, layout, *slot);
                    self.create_compose_val(from, layout, *slot);
                }
                _ => panic!("non-parser in parser layout collection"),
            }
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
    }

    fn create_nominal_funs(&mut self, layout: IMonoLayout<'comp>) {
        self.create_pd_deref(layout);
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
