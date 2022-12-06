use super::*;

impl<'llvm, 'comp> CodeGenCtx<'llvm, 'comp> {
    fn vtable_get<T: TargetSized>(
        &mut self,
        vtable_ptr_ptr: PointerValue<'llvm>,
        field_path: &[u64],
    ) -> BasicValueEnum<'llvm> {
        let ty = <&&T>::codegen_ty(self).into_pointer_type();
        let actual_ptr_ptr = self
            .builder
            .build_bitcast(vtable_ptr_ptr, ty, "")
            .into_pointer_value();
        let actual_ptr = self
            .builder
            .build_load(actual_ptr_ptr, "")
            .into_pointer_value();
        let mut path = Vec::with_capacity(field_path.len() + 1);
        let zero = self.llvm.i32_type().const_int(0, false);
        path.push(zero);
        path.extend(
            field_path
                .iter()
                .map(|x| self.llvm.i32_type().const_int(*x, false)),
        );
        let target = unsafe { self.builder.build_in_bounds_gep(actual_ptr, &path, "") };
        self.builder.build_load(target, "")
    }

    pub(super) fn build_deref_fun_get(
        &mut self,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
    ) -> CallableValue<'llvm> {
        match layout {
            Some(mono) => self.sym_callable(mono, LayoutPart::Deref(true)),
            None => self
                .vtable_get::<vtable::NominalVTable>(ptr, &[NominalVTableFields::deref_impl as u64])
                .into_pointer_value()
                .try_into()
                .unwrap(),
        }
    }

    pub(super) fn build_typecast_fun_get(
        &mut self,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
    ) -> CallableValue<'llvm> {
        match layout {
            Some(mono) => self.sym_callable(mono, LayoutPart::Typecast),
            None => self
                .vtable_get::<vtable::VTableHeader>(
                    ptr,
                    &[VTableHeaderFields::typecast_impl as u64],
                )
                .into_pointer_value()
                .try_into()
                .unwrap(),
        }
    }

    pub(super) fn build_field_access_fun_get(
        &mut self,
        block: BlockId,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
        field: Identifier,
    ) -> CallableValue<'llvm> {
        match layout {
            Some(mono) => self.sym_callable(mono, LayoutPart::Field(field)),
            None => {
                let index = self
                    .compiler_database
                    .db
                    .sorted_field_index(block, FieldName::Ident(field), false)
                    .expect("failed to lookup field index")
                    .expect("could not find field");
                self.vtable_get::<vtable::BlockVTable>(
                    ptr,
                    &[BlockVTableFields::access_impl as u64, index as u64],
                )
                .into_pointer_value()
                .try_into()
                .unwrap()
            }
        }
    }

    pub(super) fn build_parser_fun_get(
        &mut self,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
        slot: u64,
        call_kind: CallKind,
    ) -> CallableValue<'llvm> {
        match layout {
            Some(mono) => {
                let part = match call_kind {
                    CallKind::Len => LayoutPart::LenImpl(slot),
                    CallKind::Val => LayoutPart::ValImpl(slot, true),
                };
                self.sym_callable(mono, part)
            }
            None => {
                let part = match call_kind {
                    CallKind::Len => ParserArgImplFields::len_impl as u64,
                    CallKind::Val => ParserArgImplFields::val_impl as u64,
                };
                self.vtable_get::<vtable::ParserVTable>(
                    ptr,
                    &[ParserVTableFields::apply_table as u64, slot, part],
                )
                .into_pointer_value()
                .try_into()
                .unwrap()
            }
        }
    }

    pub(super) fn build_fun_create_get(
        &mut self,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
        slot: PSize,
    ) -> CallableValue<'llvm> {
        match layout {
            Some(mono) => self.sym_callable(mono, LayoutPart::CreateArgs(slot)),
            None => self
                .vtable_get::<vtable::FunctionVTable>(
                    ptr,
                    &[FunctionVTableFields::apply_table as u64, slot],
                )
                .into_pointer_value()
                .try_into()
                .unwrap(),
        }
    }

    pub(super) fn build_current_element_fun_get(
        &mut self,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
    ) -> CallableValue<'llvm> {
        match layout {
            Some(mono) => self.sym_callable(mono, LayoutPart::CurrentElement),
            None => self
                .vtable_get::<vtable::ArrayVTable>(
                    ptr,
                    &[ArrayVTableFields::current_element_impl as u64],
                )
                .into_pointer_value()
                .try_into()
                .unwrap(),
        }
    }

    pub(super) fn build_single_forward_fun_get(
        &mut self,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
    ) -> CallableValue<'llvm> {
        match layout {
            Some(mono) => self.sym_callable(mono, LayoutPart::SingleForward),
            None => self
                .vtable_get::<vtable::ArrayVTable>(
                    ptr,
                    &[ArrayVTableFields::single_forward_impl as u64],
                )
                .into_pointer_value()
                .try_into()
                .unwrap(),
        }
    }

    pub(super) fn build_head_disc_get(
        &mut self,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
    ) -> IntValue<'llvm> {
        match layout {
            Some(mono) => {
                let ty = mono.mono_layout().1;
                let head = self.compiler_database.db.head_discriminant(ty);
                self.const_i64(head)
            }
            None => self
                .vtable_get::<vtable::VTableHeader>(ptr, &[VTableHeaderFields::head as u64])
                .into_int_value(),
        }
    }

    pub(super) fn build_size_get(
        &mut self,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
    ) -> IntValue<'llvm> {
        match layout {
            Some(mono) => {
                let size = mono.inner().size_align(self.layouts).unwrap().size;
                self.const_i64(size as i64)
            }
            None => self
                .vtable_get::<vtable::VTableHeader>(ptr, &[VTableHeaderFields::size as u64])
                .into_int_value(),
        }
    }

    pub(super) fn build_check_i64_bit_set(
        &mut self,
        val: IntValue<'llvm>,
        bit: u64,
    ) -> IntValue<'llvm> {
        let set_bit = self.const_i64(1 << bit);
        let and = self.builder.build_and(set_bit, val, "");
        self.builder
            .build_int_compare(IntPredicate::NE, and, self.const_i64(0), "")
    }

    pub(super) fn build_vtable_arg_set_info_get(
        &mut self,
        fun: PointerValue<'llvm>,
        argnum: PSize,
    ) -> (IntValue<'llvm>, IntValue<'llvm>) {
        let arginfo_ptr_ptr = self.build_cast::<*const *const vtable::ArgDescriptor, _>(fun);
        let arginfo_ptr = self
            .builder
            .build_load(arginfo_ptr_ptr, "")
            .into_pointer_value();
        let head_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                arginfo_ptr,
                &[
                    self.const_i64(-1 - argnum as i64),
                    self.llvm
                        .i32_type()
                        .const_int(vtable::ArgDescriptorFields::head as u64, false),
                ],
                "",
            )
        };
        let offset_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                arginfo_ptr,
                &[
                    self.const_i64(-1 - argnum as i64),
                    self.llvm
                        .i32_type()
                        .const_int(vtable::ArgDescriptorFields::offset as u64, false),
                ],
                "",
            )
        };
        let head = self.builder.build_load(head_ptr, "").into_int_value();
        let offset = self.builder.build_load(offset_ptr, "").into_int_value();
        (head, offset)
    }

    pub(super) fn build_arg_set(
        &mut self,
        fun_layout: ILayout<'comp>,
        fun: PointerValue<'llvm>,
        arg_layout: ILayout<'comp>,
        arg: PointerValue<'llvm>,
        argnum: PSize,
    ) -> IntValue<'llvm> {
        let (head, offset) = match fun_layout.maybe_mono() {
            Some(mono) => {
                let (head, offset) = self.arg_disc_and_offset(mono, argnum);
                (self.const_i64(head), self.const_size_t(offset))
            }
            None => self.build_vtable_arg_set_info_get(fun, argnum),
        };
        let fun_any_ptr = self.build_cast::<*mut u8, _>(fun);
        let fun_mono_ptr = self.build_mono_ptr(fun_any_ptr, fun_layout);
        let fun_arg_ptr = unsafe {
            self.builder
                .build_in_bounds_gep(fun_mono_ptr, &[offset], "")
        };
        let typecast_fun = self.build_typecast_fun_get(arg_layout.maybe_mono(), arg);
        let arg_mono_ptr = self.build_mono_ptr(arg, arg_layout);
        self.build_call_with_int_ret(
            typecast_fun,
            &[arg_mono_ptr.into(), head.into(), fun_arg_ptr.into()],
        )
    }
}