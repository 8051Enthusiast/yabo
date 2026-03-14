use yaboc_layout::vtable::{CreateArgFun, EvalFunFun, LenFun};

use super::*;

impl<'llvm, 'comp> CodeGenCtx<'llvm, 'comp> {
    fn vtable_header(
        &mut self,
        layout: IMonoLayout<'comp>,
        named: bool,
        vtable_global: GlobalValue<'llvm>,
    ) -> StructValue<'llvm> {
        let size_align = layout
            .inner()
            .size_align_without_vtable(self.layouts)
            .unwrap();
        let head_disc = layout.head_kind(&self.compiler_database.db);
        let head_disc_val = self.const_i64(head_disc as i64);
        let typecast = self.typecast_fun_val(layout);
        let mask = self.mask_fun_val(layout);
        let size = self.const_size_t(size_align.after as i64);
        let align = self.const_size_t(size_align.align() as i64);
        let zst = self.const_zst();
        if named {
            let vtable_ty = vtable_global.get_value_type().into_struct_type();
            vtable_ty.const_named_struct(&[
                head_disc_val.into(),
                self.vtable_ptr_from_function(vtable_global, typecast),
                self.vtable_ptr_from_function(vtable_global, mask),
                size.into(),
                align.into(),
                zst.into(),
            ])
        } else {
            self.llvm.const_struct(
                &[
                    head_disc_val.into(),
                    self.vtable_ptr_from_function(vtable_global, typecast),
                    self.vtable_ptr_from_function(vtable_global, mask),
                    size.into(),
                    align.into(),
                    zst.into(),
                ],
                false,
            )
        }
    }

    fn vtable_ty(&mut self, layout: IMonoLayout<'comp>) -> inkwell::types::StructType<'llvm> {
        let vtable_ty_name = self.sym(layout, LayoutPart::VTableTy);
        if let Some(vtable_ty) = self.module.get_struct_type(&vtable_ty_name) {
            return vtable_ty;
        }
        let vtable_ty = self.llvm.opaque_struct_type(&vtable_ty_name);
        vtable_ty
    }

    fn create_vtable<T: TargetSized>(&mut self, layout: IMonoLayout<'comp>) -> GlobalValue<'llvm> {
        let vtable_fields = T::codegen_ty(self).into_struct_type().get_field_types();
        let vtable_ty = self.vtable_ty(layout);
        vtable_ty.set_body(&vtable_fields, false);
        let vtable_sym = self.sym(layout, LayoutPart::VTable);
        let vtable = self
            .module
            .add_global(vtable_ty, Some(AddressSpace::default()), &vtable_sym);
        vtable.set_linkage(Linkage::Internal);
        vtable.set_constant(true);
        vtable
    }

    fn create_resized_vtable<T: TargetSized>(
        &mut self,
        layout: IMonoLayout<'comp>,
        size: u32,
    ) -> GlobalValue<'llvm> {
        let mut vtable_fields = T::codegen_ty(self).into_struct_type().get_field_types();
        Self::resize_struct_end_array(&mut vtable_fields, size);
        if let Some((argnum, _)) = layout.arg_num(&self.compiler_database.db).unwrap() {
            Self::resize_struct_start_array(&mut vtable_fields, argnum as u32)
        }
        let vtable_ty = self.vtable_ty(layout);
        vtable_ty.set_body(&vtable_fields, false);
        let vtable_sym = self.sym(layout, LayoutPart::VTable);
        let vtable = self
            .module
            .add_global(vtable_ty, Some(AddressSpace::default()), &vtable_sym);
        vtable.set_linkage(Linkage::Internal);
        vtable.set_constant(true);
        vtable
    }

    fn create_primitive_vtable(&mut self, layout: IMonoLayout<'comp>) {
        let vtable = if self.options.target.relative_vptrs {
            self.create_vtable::<vtable::VTableHeader<RelPtr>>(layout)
        } else {
            self.create_vtable::<vtable::VTableHeader<AbsPtr>>(layout)
        };
        let vtable_header = self.vtable_header(layout, true, vtable);
        if let MonoLayout::Ptr = layout.mono_layout() {
            // predeclare the current_element function so that we can call it
            // but don't put it into a vtable since it's a primitive type
            // and cannot be in a multilayout with other types having this function
            let _ = self.current_element_fun_val(layout);
        }
        vtable.set_initializer(&vtable_header);
    }

    fn create_array_vtable(&mut self, layout: IMonoLayout<'comp>) {
        let vtable = if self.options.target.relative_vptrs {
            self.create_vtable::<vtable::ArrayVTable<RelPtr>>(layout)
        } else {
            self.create_vtable::<vtable::ArrayVTable<AbsPtr>>(layout)
        };
        let vtable_header = self.vtable_header(layout, false, vtable);
        let single_forward = self.single_forward_fun_val(layout);
        let current_element = self.current_element_fun_val(layout);
        let array_len = self.array_len_fun_val(layout);
        let skip = self.skip_fun_val(layout);
        let span = self.span_fun_val(layout);
        let inner_array = self.inner_array_fun_val(layout);
        let vtable_ty = self.vtable_ty(layout);
        let vtable_val = vtable_ty.const_named_struct(&[
            vtable_header.into(),
            self.vtable_ptr_from_function(vtable, single_forward),
            self.vtable_ptr_from_function(vtable, current_element),
            self.vtable_ptr_from_function(vtable, array_len),
            self.vtable_ptr_from_function(vtable, skip),
            self.vtable_ptr_from_function(vtable, span),
            self.vtable_ptr_from_function(vtable, inner_array),
        ]);
        vtable.set_initializer(&vtable_val);
    }

    fn create_nominal_vtable(&mut self, layout: IMonoLayout<'comp>) {
        let vtable = if self.options.target.relative_vptrs {
            self.create_vtable::<vtable::NominalVTable<RelPtr>>(layout)
        } else {
            self.create_vtable::<vtable::NominalVTable<AbsPtr>>(layout)
        };
        let vtable_header = self.vtable_header(layout, false, vtable);
        let MonoLayout::Nominal(pd, _, _) = layout.mono_layout() else {
            panic!("attempting to create nominal vtable of non-nominal layout")
        };
        let ident = pd.0.unwrap_name(&self.compiler_database.db);
        let name = self
            .compiler_database
            .db
            .lookup_intern_identifier(ident)
            .name;
        let llvm_name = self.module_string(&name);
        let start = self.start_fun_val(layout);
        let end = self.end_fun_val(layout);
        let vtable_ty = self.vtable_ty(layout);
        let vtable_val = vtable_ty.const_named_struct(&[
            vtable_header.into(),
            self.vtable_ptr_from_ptr(vtable, llvm_name),
            self.vtable_ptr_from_function(vtable, start),
            self.vtable_ptr_from_function(vtable, end),
        ]);
        vtable.set_initializer(&vtable_val);
    }

    fn create_block_vtable(&mut self, layout: IMonoLayout<'comp>) {
        let id = if let MonoLayout::Block(id, _) = layout.mono_layout() {
            *id
        } else {
            panic!("attempting to create block vtable of non-block layout")
        };
        let fields = self
            .compiler_database
            .db
            .sorted_block_fields(id, false)
            .expect("no sort block fields");
        let vtable = if self.options.target.relative_vptrs {
            self.create_resized_vtable::<vtable::BlockVTable<RelPtr>>(layout, fields.len() as u32)
        } else {
            self.create_resized_vtable::<vtable::BlockVTable<AbsPtr>>(layout, fields.len() as u32)
        };
        let funs: Vec<_> = fields
            .iter()
            .map(|field| {
                let FieldName::Ident(name) = field else {
                    panic!("no field access for return")
                };
                let access_field = self.access_field_fun_val(layout, *name);
                self.vtable_ptr_from_function(vtable, access_field)
            })
            .collect();
        let vtable_header = self.vtable_header(layout, false, vtable);
        let block_info = self.block_info(id);
        let access_array = self.vtable_ptr_const_array(&funs);
        let vtable_ty = self.vtable_ty(layout);
        let vtable_val = vtable_ty.const_named_struct(&[
            vtable_header.into(),
            self.vtable_ptr_from_ptr(vtable, block_info),
            access_array.into(),
        ]);
        vtable.set_initializer(&vtable_val);
    }

    fn create_set_arg_array(&mut self, layout: IMonoLayout<'comp>) -> ArrayValue<'llvm> {
        let (total_args, settable_args) = layout
            .arg_num(&self.compiler_database.db)
            .unwrap()
            .unwrap_or((0, 0));
        let mut arg_impls = Vec::with_capacity(total_args);
        // /  settable args  \/   non-settable args  \
        // | [0] | [1] | [2] | [3] | [4] | [5] | [6] | array indices
        // |  6  |  5  |  4  |  3  |  2  |  1  |  0  | arg num
        // -------------------------------------------
        for i in 0..settable_args {
            let argnum = total_args - i - 1;
            arg_impls.push(self.parser_set_arg_struct_val(layout, argnum as PSize));
        }
        let struct_ty = vtable::ArgDescriptor::struct_type(self);
        let zero_struct = struct_ty.const_zero();
        for _ in settable_args..total_args {
            arg_impls.push(zero_struct);
        }
        struct_ty.const_array(&arg_impls)
    }

    fn gather_slots<F: TargetSized, M: Copy>(
        &mut self,
        slots: &FxHashMap<u64, M>,
        vtable: GlobalValue<'llvm>,
        size: PSize,
        f: impl Fn(&mut Self, M, u64) -> PointerValue<'llvm>,
    ) -> ArrayValue<'llvm> {
        let impls = (0..size)
            .map(|slot| {
                let val = if let Some(arg) = slots.get(&slot).copied() {
                    f(self, arg, slot)
                } else {
                    F::codegen_ty(self).into_pointer_type().const_null()
                };
                self.vtable_ptr_from_ptr(vtable, val)
            })
            .collect::<Vec<_>>();
        self.vtable_ptr_const_array(&impls)
    }

    fn create_parser_vtable(&mut self, layout: IMonoLayout<'comp>) {
        // it may be that a parser is not used at all, in which case we are already finished
        let slots = self.collected_layouts.parser_slots.occupied_slots(layout);
        let max = slots.keys().copied().max().map(|x| x + 1).unwrap_or(0);
        let arg_impl_array = self.create_set_arg_array(layout);
        let vtable = if self.options.target.relative_vptrs {
            self.create_resized_vtable::<vtable::ParserVTable<RelPtr>>(layout, max as u32)
        } else {
            self.create_resized_vtable::<vtable::ParserVTable<AbsPtr>>(layout, max as u32)
        };
        let vtable_header = self.vtable_header(layout, false, vtable);
        let vtable_array =
            self.gather_slots::<ParserFun, _>(&slots, vtable, max, |this, (from, req), _| {
                this.parser_impl_struct_val(layout, from, req)
            });
        let len_impl = if self.collected_layouts.lens.contains(&layout) {
            self.parser_len_fun_val(layout)
                .as_global_value()
                .as_pointer_value()
        } else {
            LenFun::codegen_ty(self).into_pointer_type().const_null()
        };
        let vtable_ty = self.vtable_ty(layout);
        let vtable_val = vtable_ty.const_named_struct(&[
            arg_impl_array.into(),
            vtable_header.into(),
            self.vtable_ptr_from_ptr(vtable, len_impl),
            vtable_array.into(),
        ]);
        vtable.set_initializer(&vtable_val)
    }

    fn create_function_vtable(&mut self, layout: IMonoLayout<'comp>) {
        let slots = self.collected_layouts.funcall_slots.occupied_slots(layout);
        let len = slots.keys().copied().max().map(|x| x + 1).unwrap_or(0);
        let arg_impl_array = self.create_set_arg_array(layout);
        let vtable = if self.options.target.relative_vptrs {
            self.create_resized_vtable::<vtable::FunctionVTable<RelPtr>>(layout, len as u32)
        } else {
            self.create_resized_vtable::<vtable::FunctionVTable<AbsPtr>>(layout, len as u32)
        };
        let vtable_header = self.vtable_header(layout, false, vtable);
        let vtable_array =
            self.gather_slots::<CreateArgFun, _>(&slots, vtable, len, |this, _, slot| {
                this.function_create_args_fun_val(layout, slot)
                    .as_global_value()
                    .as_pointer_value()
            });

        let eval_slots = self.collected_layouts.eval_slots.occupied_slots(layout);
        let eval_funcs =
            self.gather_slots::<EvalFunFun, _>(&eval_slots, vtable, 3, |this, req, _| {
                this.eval_fun_fun_val_wrapper(layout, req)
                    .as_global_value()
                    .as_pointer_value()
            });

        let vtable_ty = self.vtable_ty(layout);
        let vtable_val = vtable_ty.const_named_struct(&[
            arg_impl_array.into(),
            vtable_header.into(),
            eval_funcs.into(),
            vtable_array.into(),
        ]);
        vtable.set_initializer(&vtable_val)
    }

    pub fn create_all_vtables(&mut self) {
        let collected_layouts = self.collected_layouts.clone();
        for layout in collected_layouts.arrays.iter() {
            self.create_array_vtable(*layout);
        }
        for layout in collected_layouts.blocks.iter() {
            self.create_block_vtable(*layout);
        }
        for layout in collected_layouts.nominals.iter() {
            self.create_nominal_vtable(*layout);
        }
        for layout in collected_layouts.parsers.iter() {
            self.create_parser_vtable(*layout);
        }
        for layout in collected_layouts.primitives.iter() {
            self.create_primitive_vtable(*layout);
        }
        for layout in collected_layouts.functions.iter() {
            self.create_function_vtable(*layout);
        }
    }
}
