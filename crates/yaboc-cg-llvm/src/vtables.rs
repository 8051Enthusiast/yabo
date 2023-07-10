use yaboc_layout::vtable::{BlockFieldFun, CreateArgFun, LenFun};

use super::*;

impl<'llvm, 'comp> CodeGenCtx<'llvm, 'comp> {
    fn vtable_header(&mut self, layout: IMonoLayout<'comp>) -> StructValue<'llvm> {
        let size_align = vtable::VTableHeader::tsize();
        let head_discriminant = self
            .compiler_database
            .db
            .head_discriminant(layout.mono_layout().1);
        let head_disc_val = self.const_i64(head_discriminant);
        let deref_level = self.deref_level(layout.mono_layout().1);
        let typecast = self
            .typecast_fun_val(layout)
            .as_global_value()
            .as_pointer_value();
        let size = self.const_size_t(size_align.size as i64);
        let align = self.const_size_t(size_align.align() as i64);
        self.llvm.const_struct(
            &[
                head_disc_val.into(),
                deref_level.into(),
                typecast.into(),
                size.into(),
                align.into(),
            ],
            false,
        )
    }

    fn create_vtable<T: TargetSized>(&mut self, layout: IMonoLayout<'comp>) -> GlobalValue<'llvm> {
        let vtable_type = T::codegen_ty(self);
        let vtable_sym = self.sym(layout, LayoutPart::VTable);
        let vtable =
            self.module
                .add_global(vtable_type, Some(AddressSpace::default()), &vtable_sym);
        vtable.set_linkage(Linkage::Internal);
        vtable
    }

    fn create_resized_vtable<T: TargetSized>(
        &mut self,
        layout: IMonoLayout<'comp>,
        size: u32,
    ) -> GlobalValue<'llvm> {
        let mut vtable_type = T::codegen_ty(self).into_struct_type();
        vtable_type = self.resize_struct_end_array(vtable_type, size);
        if let Some((argnum, _)) = layout.arg_num(&self.compiler_database.db).unwrap() {
            vtable_type = self.resize_struct_start_array(vtable_type, argnum as u32)
        }
        let vtable_sym = self.sym(layout, LayoutPart::VTable);
        let vtable =
            self.module
                .add_global(vtable_type, Some(AddressSpace::default()), &vtable_sym);
        vtable.set_linkage(Linkage::Internal);
        vtable
    }

    fn create_primitive_vtable(&mut self, layout: IMonoLayout<'comp>) {
        let vtable = self.create_vtable::<vtable::VTableHeader>(layout);
        let vtable_header = self.vtable_header(layout);
        vtable.set_initializer(&vtable_header);
    }

    fn create_array_vtable(&mut self, layout: IMonoLayout<'comp>) {
        let vtable = self.create_vtable::<vtable::ArrayVTable>(layout);
        let vtable_header = self.vtable_header(layout);
        let single_foward = self.single_forward_fun_val(layout);
        let current_element = self.current_element_fun_val(layout);
        let array_len = self.array_len_fun_val(layout);
        let skip = self.skip_fun_val(layout);
        let span = self.span_fun_val(layout);
        let vtable_val = self.llvm.const_struct(
            &[
                vtable_header.into(),
                single_foward.as_global_value().as_pointer_value().into(),
                current_element.as_global_value().as_pointer_value().into(),
                array_len.as_global_value().as_pointer_value().into(),
                skip.as_global_value().as_pointer_value().into(),
                span.as_global_value().as_pointer_value().into(),
            ],
            false,
        );
        vtable.set_initializer(&vtable_val);
    }

    fn create_nominal_vtable(&mut self, layout: IMonoLayout<'comp>) {
        let vtable = self.create_vtable::<vtable::NominalVTable>(layout);
        let vtable_header = self.vtable_header(layout);
        let start = self.start_fun_val(layout);
        let end = self.end_fun_val(layout);
        let vtable_val = self.llvm.const_struct(
            &[
                vtable_header.into(),
                start.as_global_value().as_pointer_value().into(),
                end.as_global_value().as_pointer_value().into(),
            ],
            false,
        );
        vtable.set_initializer(&vtable_val);
    }

    fn create_block_vtable(&mut self, layout: IMonoLayout<'comp>) {
        let id = if let MonoLayout::Block(id, _) = layout.mono_layout().0 {
            *id
        } else {
            panic!("attempting to create block vtable of non-block layout")
        };
        let fields = self
            .compiler_database
            .db
            .sorted_block_fields(id, false)
            .expect("no sort block fields");
        let funs: Vec<_> = fields
            .iter()
            .map(|field| {
                let name = match field {
                    FieldName::Return => panic!("no field access for return"),
                    FieldName::Ident(id) => *id,
                };
                self.access_field_fun_val(layout, name)
                    .as_global_value()
                    .as_pointer_value()
            })
            .collect();
        let vtable = self.create_resized_vtable::<vtable::BlockVTable>(layout, funs.len() as u32);
        let vtable_header = self.vtable_header(layout);
        let block_info = self.block_info(id);
        let access_array = BlockFieldFun::codegen_ty(self)
            .into_pointer_type()
            .const_array(&funs);
        let vtable_val = self.llvm.const_struct(
            &[vtable_header.into(), block_info.into(), access_array.into()],
            false,
        );
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

    fn create_parser_vtable(&mut self, layout: IMonoLayout<'comp>) {
        // it may be that a parser is not used at all, in which case we are already finished
        let slots = self
            .collected_layouts
            .parser_slots
            .occupied_entries
            .get(&layout)
            .cloned()
            .unwrap_or_default();
        let max = slots.keys().copied().max().unwrap_or(0);
        let arg_impl_array = self.create_set_arg_array(layout);
        let vtable = self.create_resized_vtable::<vtable::ParserVTable>(layout, (max + 1) as u32);
        let vtable_header = self.vtable_header(layout);
        let vtable_impls: Vec<_> = (0..=max)
            .map(|slot| {
                let is_non_null = slots.contains_key(&slot);
                let req = slots
                    .get(&slot)
                    .copied()
                    .map(|(_, req)| req)
                    .unwrap_or_default();
                self.parser_impl_struct_val(layout, slot, req, is_non_null)
            })
            .collect();
        let len_impl = if self.collected_layouts.lens.contains(&layout) {
            self.parser_len_fun_val(layout).as_global_value().as_pointer_value()
        } else {
            LenFun::codegen_ty(self).into_pointer_type().const_null()
        };
        let vtable_array = ParserFun::codegen_ty(self)
            .into_pointer_type()
            .const_array(&vtable_impls);
        let vtable_val = self.llvm.const_struct(
            &[
                arg_impl_array.into(),
                vtable_header.into(),
                len_impl.into(),
                vtable_array.into(),
            ],
            false,
        );
        vtable.set_initializer(&vtable_val)
    }

    fn create_function_vtable(&mut self, layout: IMonoLayout<'comp>) {
        let slots = self
            .collected_layouts
            .funcall_slots
            .occupied_entries
            .get(&layout)
            .cloned()
            .unwrap_or_default();
        let max = slots.keys().copied().max().unwrap_or(0);
        let arg_impl_array = self.create_set_arg_array(layout);
        let vtable = self.create_resized_vtable::<vtable::FunctionVTable>(layout, (max + 1) as u32);
        let vtable_header = self.vtable_header(layout);
        let vtable_impls: Vec<_> = (0..=max)
            .map(|slot| {
                let is_non_null = slots.contains_key(&slot);
                if is_non_null {
                    self.function_create_args_fun_val(layout, slot)
                        .as_global_value()
                        .as_pointer_value()
                } else {
                    CreateArgFun::codegen_ty(self)
                        .into_pointer_type()
                        .const_null()
                }
            })
            .collect();
        let vtable_array = CreateArgFun::codegen_ty(self)
            .into_pointer_type()
            .const_array(&vtable_impls);
        let vtable_val = self.llvm.const_struct(
            &[
                arg_impl_array.into(),
                vtable_header.into(),
                vtable_array.into(),
            ],
            false,
        );
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
