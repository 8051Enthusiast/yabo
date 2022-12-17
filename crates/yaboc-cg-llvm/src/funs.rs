use yaboc_dependents::NeededBy;
use yaboc_hir_types::DerefLevel;

use crate::convert_thunk::{BlockThunk, CreateArgsThunk, TypecastThunk, ValThunk};

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

    fn terminate_tail_typecast(
        &mut self,
        layout: ILayout<'comp>,
        buffer: PointerValue<'llvm>,
        target_head: IntValue<'llvm>,
        pointer: PointerValue<'llvm>,
    ) {
        let typecast_fun = self.build_typecast_fun_get(layout.maybe_mono(), buffer);
        let ret = self.build_call_with_int_ret(
            typecast_fun,
            &[pointer.into(), buffer.into(), target_head.into()],
        );
        self.builder.build_return(Some(&ret));
    }

    fn mir_pd_fun(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        call_kind: RequirementSet,
    ) -> FunctionSubstitute<'comp> {
        let pd = if let MonoLayout::NominalParser(id, _) = layout.mono_layout().0 {
            id
        } else {
            panic!("mir_pd_len_fun has to be called with a nominal parser layout");
        };
        let mir = self.compiler_database.db.mir_pd(*pd, call_kind).unwrap();
        FunctionSubstitute::new_from_pd(mir, from, layout.inner(), *pd, self.layouts).unwrap()
    }

    fn mir_block_fun(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        call_kind: RequirementSet,
    ) -> FunctionSubstitute<'comp> {
        let bd = if let MonoLayout::BlockParser(id, _) = layout.mono_layout().0 {
            id
        } else {
            panic!("mir_pd_len_fun has to be called with a nominal parser layout");
        };
        let mir = self.compiler_database.db.mir_block(*bd, call_kind).unwrap();
        FunctionSubstitute::new_from_block(mir, from, layout, self.layouts).unwrap()
    }

    fn create_typecast(&mut self, layout: IMonoLayout<'comp>) {
        if let MonoLayout::Nominal(..) = layout.mono_layout().0 {
            let (from, fun) = layout.unapply_nominal(self.layouts);
            let slot = self.collected_layouts.parser_slots.layout_vtable_offsets[&(from, fun)];
            let mono = flat_layouts(&fun).next().unwrap();
            let sym = self.sym(mono, LayoutPart::ValImpl(slot, false));
            if self.module.get_function(&sym).is_none() {
                self.create_pd_val_impl(from, mono, slot);
            }
        }
        ThunkContext::new(self, TypecastThunk { layout }).build();
    }

    fn create_pd_val(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        slot: PSize,
        pd: ParserDefId,
    ) {
        let sym = self.sym(layout, LayoutPart::ValImpl(slot, false));
        if self.module.get_function(&sym).is_none() {
            self.create_pd_val_impl(from, layout, slot);
        }
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
        let return_layout =
            flat_layouts(&ILayout::make_thunk(self.layouts, pd, result, &map).unwrap())
                .next()
                .unwrap();
        ThunkContext::new(
            self,
            ValThunk {
                from,
                fun: layout,
                thunk: return_layout,
                slot,
            },
        )
        .build();
    }

    fn create_pd_val_impl(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        slot: PSize,
    ) {
        let llvm_fun = self.parser_val_impl_fun_val(layout, slot);
        let mir_fun = Rc::new(self.mir_pd_fun(from, layout, NeededBy::Val | NeededBy::Backtrack));
        let (ret, fun, head, arg, _) = parser_args(llvm_fun);
        MirTranslator::new(self, mir_fun, llvm_fun, fun, arg)
            .with_ret_ptr(ret, head)
            .build();
    }

    fn create_pd_len(&mut self, from: ILayout<'comp>, layout: IMonoLayout<'comp>, slot: PSize) {
        let llvm_fun = self.parser_len_fun_val(layout, slot);
        let mir_fun = Rc::new(self.mir_pd_fun(from, layout, NeededBy::Len | NeededBy::Backtrack));
        let (_, fun, _, arg, retlen) = parser_args(llvm_fun);
        MirTranslator::new(self, mir_fun, llvm_fun, fun, arg)
            .with_retlen_ptr(retlen)
            .build();
    }

    fn create_pd_end(&mut self, layout: IMonoLayout<'comp>) {
        let llvm_fun = self.end_fun_val(layout);
        self.add_entry_block(llvm_fun);
        let [ret, arg] = get_fun_args(llvm_fun).map(|x| x.into_pointer_value());
        let (from_layout, fun_layout) = layout.unapply_nominal(self.layouts);
        let (from, fun, _) = self.build_nominal_components(layout, arg);
        let ret = self.build_parser_call(
            self.any_ptr().get_undef(),
            (fun_layout, fun),
            self.const_i64(DerefLevel::max().into_shifted_runtime_value() as i64),
            (from_layout, from),
            ret,
            NeededBy::Len.into(),
        );
        self.builder.build_return(Some(&ret));
    }

    fn create_pd_start(&mut self, layout: IMonoLayout<'comp>) {
        let fun = self.start_fun_val(layout);
        self.add_entry_block(fun);
        let [to, from] = get_fun_args(fun).map(|x| x.into_pointer_value());
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

        let [to, from] = get_fun_args(fun).map(|x| x.into_pointer_value());
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
        let [to, from, len] = get_fun_args(fun);
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
        let [return_ptr, from, target_head] = get_fun_args(fun);
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
        let mir_fun =
            Rc::new(self.mir_block_fun(from, layout, NeededBy::Len | NeededBy::Backtrack));
        let (_, fun, _, arg, retlen) = parser_args(llvm_fun);
        MirTranslator::new(self, mir_fun, llvm_fun, fun, arg)
            .with_retlen_ptr(retlen)
            .build();
    }

    fn create_block_val(
        &mut self,
        block: BlockId,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        slot: PSize,
    ) {
        let block = block.lookup(&self.compiler_database.db).unwrap();
        let llvm_fun = self.parser_val_fun_val(layout, slot);
        let mir_fun =
            Rc::new(self.mir_block_fun(from, layout, NeededBy::Val | NeededBy::Backtrack));
        let impl_fun = if block.returns {
            llvm_fun
        } else {
            self.parser_val_impl_fun_val(layout, slot)
        };

        let (ret, fun, head, arg, _) = parser_args(impl_fun);
        MirTranslator::new(self, mir_fun, impl_fun, fun, arg)
            .with_ret_ptr(ret, head)
            .build();

        if block.returns {
            return;
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
        };

        ThunkContext::new(self, block_data).build();
    }

    fn create_single_len(&mut self, from: ILayout<'comp>, layout: IMonoLayout<'comp>, slot: PSize) {
        let llvm_fun = self.parser_len_fun_val(layout, slot);
        self.set_always_inline(llvm_fun);
        let (_, _, _, arg, ret) = parser_args(llvm_fun);
        self.add_entry_block(llvm_fun);
        let single_forward_fun = self.build_single_forward_fun_get(from.maybe_mono(), arg);
        let ret = self.build_call_with_int_ret(single_forward_fun, &[ret.into(), arg.into()]);
        self.builder.build_return(Some(&ret));
    }

    fn create_single_val(&mut self, from: ILayout<'comp>, layout: IMonoLayout<'comp>, slot: PSize) {
        let llvm_fun = self.parser_val_fun_val(layout, slot);
        self.set_always_inline(llvm_fun);
        let (ret, _, target_head, arg, _) = parser_args(llvm_fun);
        self.add_entry_block(llvm_fun);
        let current_element_fun = self.build_current_element_fun_get(from.maybe_mono(), arg);
        let ret = self.build_call_with_int_ret(
            current_element_fun,
            &[ret.into(), arg.into(), target_head.into()],
        );
        self.builder.build_return(Some(&ret));
    }

    fn create_nil_len(&mut self, from: ILayout<'comp>, layout: IMonoLayout<'comp>, slot: PSize) {
        let llvm_fun = self.parser_len_fun_val(layout, slot);
        self.set_always_inline(llvm_fun);
        let (_, _, _, arg, retlen) = parser_args(llvm_fun);
        self.add_entry_block(llvm_fun);
        let arg_size = self.build_size_get(from.maybe_mono(), arg);
        let arg_start = self.get_object_start(from.maybe_mono(), arg);
        let ret_start = self.get_object_start(from.maybe_mono(), retlen);
        self.builder
            .build_memcpy(ret_start, 1, arg_start, 1, arg_size)
            .unwrap();
        self.builder.build_return(Some(&self.const_i64(0)));
    }

    fn create_nil_val(&mut self, _: ILayout<'comp>, layout: IMonoLayout<'comp>, slot: PSize) {
        let llvm_fun = self.parser_val_fun_val(layout, slot);
        self.set_always_inline(llvm_fun);
        let (ret, _, target_head, _, _) = parser_args(llvm_fun);
        self.add_entry_block(llvm_fun);
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
        let (_, fun, _, arg, retlen) = parser_args(llvm_fun);
        self.add_entry_block(llvm_fun);
        let (first_layout, inner_ty, second_layout) =
            if let MonoLayout::ComposedParser(first, inner_ty, second) = layout.mono_layout().0 {
                (*first, *inner_ty, *second)
            } else {
                panic!("called build_compose_len on non-composed")
            };
        let inner_layout = first_layout.apply_arg(self.layouts, from).unwrap();
        let inner_level = self.deref_level(inner_ty);
        let second_arg = self.build_layout_alloca(inner_layout, "second_arg");
        let second_len_ret = self.build_layout_alloca(inner_layout, "second_len_ret");
        let first_ptr = self.build_duple_gep(layout.inner(), DupleField::First, fun, first_layout);
        let second_ptr =
            self.build_duple_gep(layout.inner(), DupleField::Second, fun, second_layout);

        [NeededBy::Len, NeededBy::Val].map(|call_kind| {
            let ret = self.build_parser_call(
                second_arg,
                (first_layout, first_ptr),
                inner_level,
                (from, arg),
                retlen,
                call_kind | NeededBy::Backtrack,
            );
            self.non_zero_early_return(llvm_fun, ret)
        });

        let ret = self.build_parser_call(
            second_len_ret,
            (second_layout, second_ptr),
            self.const_i64(DerefLevel::max().into_shifted_runtime_value() as i64),
            (inner_layout, second_arg),
            second_len_ret,
            NeededBy::Len | NeededBy::Backtrack,
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
        self.add_entry_block(llvm_fun);
        let (ret, fun, target_head, arg, retlen) = parser_args(llvm_fun);
        let (first_layout, inner_ty, second_layout) =
            if let MonoLayout::ComposedParser(first, inner_ty, second) = layout.mono_layout().0 {
                (*first, *inner_ty, *second)
            } else {
                panic!("called build_compose_len on non-composed")
            };
        let inner_layout = first_layout.apply_arg(self.layouts, from).unwrap();
        let inner_level = self.deref_level(inner_ty);
        let second_arg = self.build_layout_alloca(inner_layout, "second_arg");
        let first_ptr = self.build_duple_gep(layout.inner(), DupleField::First, fun, first_layout);
        let second_ptr =
            self.build_duple_gep(layout.inner(), DupleField::Second, fun, second_layout);

        let retstatus = self.build_parser_call(
            second_arg,
            (first_layout, first_ptr),
            inner_level,
            (from, arg),
            retlen,
            NeededBy::Val | NeededBy::Backtrack,
        );
        self.non_zero_early_return(llvm_fun, retstatus);

        let retstatus = self.build_parser_call(
            ret,
            (second_layout, second_ptr),
            target_head,
            (inner_layout, second_arg),
            self.any_ptr().get_undef(),
            NeededBy::Val | NeededBy::Backtrack,
        );
        self.builder.build_return(Some(&retstatus));
    }

    fn create_field_access(&mut self, layout: IMonoLayout<'comp>, field: DefId, name: Identifier) {
        let fun = self.access_field_fun_val(layout, name);
        let entry = self.llvm.append_basic_block(fun, "entry");
        self.builder.position_at_end(entry);
        let [return_ptr, block, target_head] = get_fun_args(fun);
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
        let field_ptr = self.build_field_gep(layout.inner(), field, block, inner_layout);
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
                MonoLayout::BlockParser(block, _) => {
                    self.create_block_len(from, layout, *slot);
                    self.create_block_val(*block, from, layout, *slot);
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
