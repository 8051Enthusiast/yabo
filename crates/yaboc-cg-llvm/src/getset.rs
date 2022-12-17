use yaboc_dependents::NeededBy;
use yaboc_types::TypeId;

use super::*;

impl<'llvm, 'comp> CodeGenCtx<'llvm, 'comp> {
    fn vtable_get<T: TargetSized>(
        &mut self,
        value_ptr: PointerValue<'llvm>,
        field_path: &[u64],
    ) -> BasicValueEnum<'llvm> {
        let ty = <&&T>::codegen_ty(self).into_pointer_type();
        let actual_ptr_ptr = self
            .builder
            .build_bitcast(value_ptr, ty, "casted_ptr_ptr")
            .into_pointer_value();
        let before_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                actual_ptr_ptr,
                &[self.const_i64(-1)],
                "vtable_ptr_ptr",
            )
        };
        let vtable_ptr = self
            .builder
            .build_load(before_ptr, "vtable_ptr")
            .into_pointer_value();
        let mut path = Vec::with_capacity(field_path.len() + 1);
        path.push(self.const_i64(0));
        path.extend(
            field_path
                .iter()
                .map(|x| self.llvm.i32_type().const_int(*x, false)),
        );
        let target = unsafe { self.builder.build_in_bounds_gep(vtable_ptr, &path, "") };
        self.builder.build_load(target, "")
    }

    pub(super) fn get_object_start(
        &mut self,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
    ) -> PointerValue<'llvm> {
        match layout {
            Some(_) => ptr,
            None => self.build_byte_gep(ptr, self.const_i64(-(self.word_size() as i64)), "start"),
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
        call_kind: RequirementSet,
        use_impl: bool,
    ) -> CallableValue<'llvm> {
        match layout {
            Some(mono) => {
                let part = if call_kind.contains(NeededBy::Len) {
                    LayoutPart::LenImpl(slot)
                } else {
                    LayoutPart::ValImpl(slot, !use_impl)
                };
                self.sym_callable(mono, part)
            }
            None => {
                let part = if call_kind.contains(NeededBy::Len) {
                    ParserArgImplFields::len_impl as u64
                } else {
                    ParserArgImplFields::val_impl as u64
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

    pub(super) fn deref_level(&mut self, ty: TypeId) -> IntValue<'llvm> {
        let level = self.compiler_database.db.deref_level(ty).unwrap();
        self.const_i64(level.into_shifted_runtime_value() as i64)
    }

    pub(super) fn build_deref_level_get(
        &mut self,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
    ) -> IntValue<'llvm> {
        match layout {
            Some(mono) => {
                let ty = mono.mono_layout().1;
                self.deref_level(ty)
            }
            None => self
                .vtable_get::<vtable::VTableHeader>(ptr, &[VTableHeaderFields::deref_level as u64])
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
        let vtable_ptr_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                arginfo_ptr_ptr,
                &[self.const_i64(-1)],
                "vtable_ptr_ptr",
            )
        };
        let vtable_ptr = self
            .builder
            .build_load(vtable_ptr_ptr, "vtable_ptr")
            .into_pointer_value();
        let head_ptr = unsafe {
            self.builder.build_in_bounds_gep(
                vtable_ptr,
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
                vtable_ptr,
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
                let (head, offset) = self.arg_level_and_offset(mono, argnum);
                (self.const_i64(head), self.const_size_t(offset as i64))
            }
            None => self.build_vtable_arg_set_info_get(fun, argnum),
        };
        let fun_any_ptr = self.build_cast::<*mut u8, _>(fun);
        let fun_arg_ptr = self.build_byte_gep(fun_any_ptr, offset, "");
        let typecast_fun = self.build_typecast_fun_get(arg_layout.maybe_mono(), arg);
        self.build_call_with_int_ret(typecast_fun, &[fun_arg_ptr.into(), arg.into(), head.into()])
    }
}
