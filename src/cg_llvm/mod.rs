#![allow(dead_code)]
pub mod convert_mir;

use std::{marker::PhantomData, path::Path, rc::Rc};

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    passes::PassManager,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    },
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, PointerType, StructType},
    values::{FunctionValue, GlobalValue, IntValue, PointerValue, StructValue},
    AddressSpace, GlobalVisibility, OptimizationLevel,
};

use crate::{
    config::Configs,
    hir::{BlockId, Hirs},
    hir_types::TyHirs,
    interner::{FieldName, Identifier},
    layout::{
        collect::{LayoutCollection, LayoutCollector},
        prop::{CodegenTypeContext, PSize, SizeAlign, TargetSized},
        represent::LayoutPart,
        vtable::{
            self, ArrayVTableFields, BlockVTableFields, ParserArgImplFields, ParserVTableFields,
            VTableHeaderFields,
        },
        AbsLayoutCtx, IMonoLayout, LayoutError, MonoLayout,
    },
    mir::CallKind,
};

pub struct CodeGenCtx<'llvm, 'comp> {
    llvm: &'llvm Context,
    target: TargetMachine,
    builder: Builder<'llvm>,
    pass_manager: PassManager<Module<'llvm>>,
    module: Module<'llvm>,
    compiler_database: &'comp crate::context::Context,
    layouts: &'comp mut AbsLayoutCtx<'comp>,
    collected_layouts: Rc<LayoutCollection<'comp>>,
}

impl<'llvm, 'comp> CodeGenCtx<'llvm, 'comp> {
    pub fn new(
        llvm_context: &'llvm Context,
        compiler_database: &'comp crate::context::Context,
        layouts: &'comp mut AbsLayoutCtx<'comp>,
    ) -> Result<Self, LayoutError> {
        let mut layout_collector = LayoutCollector::new(layouts);
        let pd = compiler_database.parser("main");
        layout_collector.collect(&[pd])?;
        let collected_layouts = Rc::new(layout_collector.into_results());
        Target::initialize_all(&InitializationConfig {
            asm_parser: true,
            asm_printer: true,
            base: true,
            disassembler: false,
            info: true,
            machine_code: true,
        });
        let builder = llvm_context.create_builder();
        let module = llvm_context.create_module("yabo");
        let pass_manager = PassManager::create(());
        let cfg = compiler_database.db.config();
        let triple = TargetTriple::create(&cfg.target_triple);
        let target = Target::from_triple(&triple)
            .expect("no")
            .create_target_machine(
                &triple,
                &cfg.target_cpu,
                &cfg.target_features,
                OptimizationLevel::Aggressive,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .expect("Could not get target machine");
        Ok(CodeGenCtx {
            llvm: llvm_context,
            target,
            builder,
            pass_manager,
            module,
            compiler_database,
            layouts,
            collected_layouts,
        })
    }
    fn resize_struct_end_array(&self, st: StructType<'llvm>, size: u32) -> StructType<'llvm> {
        let mut field_types = st.get_field_types();
        if let Some(BasicTypeEnum::ArrayType(array)) = field_types.last_mut() {
            *array = array.get_element_type().array_type(size);
        } else {
            panic!("last field of struct is not an array")
        };
        self.llvm.struct_type(&field_types, false)
    }
    fn sym(&mut self, layout: IMonoLayout<'comp>, part: LayoutPart) -> String {
        layout.symbol(self.layouts, part, &self.compiler_database.db)
    }
    fn sym_ptr(&mut self, layout: IMonoLayout<'comp>, part: LayoutPart) -> PointerValue<'llvm> {
        let sym = self.sym(layout, part);
        match self.module.get_global(&sym) {
            Some(f) => f.as_pointer_value(),
            None => panic!("could not find symbol {sym}"),
        }
    }
    fn const_size_t(&self, val: u64) -> IntValue<'llvm> {
        self.llvm
            .ptr_sized_int_type(&self.target.get_target_data(), None)
            .const_int(val, false)
    }
    fn const_i64(&self, val: i64) -> IntValue<'llvm> {
        self.llvm.i64_type().const_int(val as u64, false)
    }
    fn any_ptr(&self) -> PointerType<'llvm> {
        self.llvm.i8_type().ptr_type(AddressSpace::Generic)
    }
    fn fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        part: LayoutPart,
        types: &[BasicMetadataTypeEnum<'llvm>],
    ) -> FunctionValue<'llvm> {
        let sf_sym = self.sym(layout, part);
        let sf_type = self.llvm.i64_type().fn_type(types, false);
        let fun = self
            .module
            .add_function(&sf_sym, sf_type, Some(Linkage::External));
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
    fn pip_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        part: LayoutPart,
    ) -> FunctionValue<'llvm> {
        self.fun_val(
            layout,
            part,
            &[
                self.any_ptr().into(),
                self.llvm.i64_type().into(),
                self.any_ptr().into(),
            ],
        )
    }
    fn ppp_fun_val(
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
                self.any_ptr().into(),
            ],
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
    fn single_forward_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pp_fun_val(layout, LayoutPart::SingleForward)
    }
    fn start_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pp_fun_val(layout, LayoutPart::Start)
    }
    fn end_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pp_fun_val(layout, LayoutPart::End)
    }
    fn current_element_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pip_fun_val(layout, LayoutPart::CurrentElement)
    }
    fn skip_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pip_fun_val(layout, LayoutPart::Skip)
    }
    fn access_field_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        name: Identifier,
    ) -> FunctionValue<'llvm> {
        self.pip_fun_val(layout, LayoutPart::Field(name))
    }
    fn typecast_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pip_fun_val(layout, LayoutPart::Typecast)
    }
    fn parser_impl_struct_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        slot: PSize,
        is_non_null: bool,
    ) -> StructValue<'llvm> {
        let (len, val) = if is_non_null {
            let len_fn_ptr = self
                .ppp_fun_val(layout, LayoutPart::LenImpl(slot))
                .as_global_value()
                .as_pointer_value();
            let val_fn_ptr = self
                .ppip_fun_val(layout, LayoutPart::ValImpl(slot))
                .as_global_value()
                .as_pointer_value();
            (len_fn_ptr, val_fn_ptr)
        } else {
            let len_fn_null_ptr = <fn(*const u8, *const u8, *mut u8) -> i64>::codegen_ty(self)
                .into_pointer_type()
                .const_null();
            let val_fn_null_ptr = <fn(*const u8, *const u8, i64, *mut u8) -> i64>::codegen_ty(self)
                .into_pointer_type()
                .const_null();
            (len_fn_null_ptr, val_fn_null_ptr)
        };
        self.llvm.const_struct(&[len.into(), val.into()], false)
    }
    fn vtable_header(&mut self, layout: IMonoLayout<'comp>) -> StructValue<'llvm> {
        let size_align = vtable::VTableHeader::tsize();
        let head_discriminant = self
            .compiler_database
            .db
            .head_discriminant(layout.mono_layout().1);
        let head_disc_val = self.const_i64(head_discriminant);
        let typecast = self
            .typecast_fun_val(layout)
            .as_global_value()
            .as_pointer_value();
        let size = self.const_size_t(size_align.size);
        let align = self.const_size_t(size_align.align());
        self.llvm.const_struct(
            &[
                head_disc_val.into(),
                typecast.into(),
                size.into(),
                align.into(),
            ],
            false,
        )
    }
    fn sa_type(&mut self, sa: SizeAlign) -> StructType<'llvm> {
        let align = sa.align();
        // note: this might or might not actually work and i have no idea how to do this properly
        // (there was something about i64 having an alignment of 4 on some 32-bit platform,
        // but there the maximum alignment would be 4 byte anyway so this is fine in that case?)
        let alignment_forcer = self.llvm.custom_width_int_type((align * 8) as u32);
        let array = self.llvm.i8_type().array_type((sa.size - align) as u32);
        self.llvm
            .struct_type(&[alignment_forcer.into(), array.into()], false)
    }
    fn create_vtable<T: TargetSized>(&mut self, layout: IMonoLayout<'comp>) -> GlobalValue<'llvm> {
        let vtable_type = T::codegen_ty(self);
        let vtable_sym = self.sym(layout, LayoutPart::VTable);
        let vtable = self
            .module
            .add_global(vtable_type, Some(AddressSpace::Const), &vtable_sym);
        vtable.set_visibility(GlobalVisibility::Hidden);
        vtable
    }
    fn create_resized_vtable<T: TargetSized>(
        &mut self,
        layout: IMonoLayout<'comp>,
        size: u32,
    ) -> GlobalValue<'llvm> {
        let vtable_type = T::codegen_ty(self).into_struct_type();
        let vtable_type = self.resize_struct_end_array(vtable_type, size);
        let vtable_sym = self.sym(layout, LayoutPart::VTable);
        let vtable = self
            .module
            .add_global(vtable_type, Some(AddressSpace::Const), &vtable_sym);
        vtable.set_visibility(GlobalVisibility::Hidden);
        vtable
    }
    fn create_array_vtable(&mut self, layout: IMonoLayout<'comp>) {
        let vtable = self.create_vtable::<vtable::ArrayVTable>(layout);
        let vtable_header = self.vtable_header(layout);
        let single_foward = self.single_forward_fun_val(layout);
        let current_element = self.current_element_fun_val(layout);
        let skip = self.skip_fun_val(layout);
        let vtable_val = self.llvm.const_struct(
            &[
                vtable_header.into(),
                single_foward.as_global_value().as_pointer_value().into(),
                current_element.as_global_value().as_pointer_value().into(),
                skip.as_global_value().as_pointer_value().into(),
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
        let access_array = <fn(*const u8, i64, *mut u8) -> i64>::codegen_ty(self)
            .into_pointer_type()
            .const_array(&funs);
        let vtable_val = self
            .llvm
            .const_struct(&[vtable_header.into(), access_array.into()], false);
        vtable.set_initializer(&vtable_val);
    }
    fn create_parser_vtable(&mut self, layout: IMonoLayout<'comp>) {
        let slots = self.collected_layouts.parser_occupied_entries[&layout].clone();
        let max = slots.keys().copied().max().unwrap_or(0);
        let vtable = self.create_resized_vtable::<vtable::ParserVTable>(layout, max as u32);
        let vtable_header = self.vtable_header(layout);
        let vtable_impls: Vec<_> = (0..max)
            .map(|slot| {
                let is_non_null = slots.contains_key(&slot);
                self.parser_impl_struct_val(layout, slot, is_non_null)
            })
            .collect();
        let vtable_array = vtable::ParserArgImpl::struct_type(self).const_array(&vtable_impls);
        let vtable_val = self
            .llvm
            .const_struct(&[vtable_header.into(), vtable_array.into()], false);
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
    }
    fn vtable_get<T: TargetSized>(
        &mut self,
        vtable_ptr_ptr: PointerValue<'llvm>,
        field_path: &[u64],
    ) -> PointerValue<'llvm> {
        let ty = <&&T>::codegen_ty(self).into_pointer_type();
        let actual_ptr_ptr = self
            .builder
            .build_bitcast(vtable_ptr_ptr, ty, "")
            .into_pointer_value();
        let mut path = Vec::with_capacity(field_path.len() + 2);
        let zero = self.llvm.i32_type().const_int(0, false);
        path.push(zero);
        path.push(zero);
        path.extend(
            field_path
                .iter()
                .map(|x| self.llvm.i32_type().const_int(*x, false)),
        );
        unsafe { self.builder.build_in_bounds_gep(actual_ptr_ptr, &path, "") }
    }
    fn build_typecast_fun_get(
        &mut self,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
    ) -> PointerValue<'llvm> {
        match layout {
            Some(mono) => self.sym_ptr(mono, LayoutPart::Typecast),
            None => self.vtable_get::<vtable::VTableHeader>(
                ptr,
                &[VTableHeaderFields::typecast_impl as u64],
            ),
        }
    }
    fn build_field_access_fun_get(
        &mut self,
        block: BlockId,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
        field: Identifier,
    ) -> PointerValue<'llvm> {
        match layout {
            Some(mono) => self.sym_ptr(mono, LayoutPart::Field(field)),
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
            }
        }
    }
    fn build_parser_fun_get(
        &mut self,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
        slot: u64,
        call_kind: CallKind,
    ) -> PointerValue<'llvm> {
        match layout {
            Some(mono) => {
                let part = match call_kind {
                    CallKind::Len => LayoutPart::LenImpl(slot),
                    CallKind::Val => LayoutPart::ValImpl(slot),
                };
                self.sym_ptr(mono, part)
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
            }
        }
    }
    fn build_current_element_fun_get(
        &mut self,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
    ) -> PointerValue<'llvm> {
        match layout {
            Some(mono) => self.sym_ptr(mono, LayoutPart::CurrentElement),
            None => self.vtable_get::<vtable::ArrayVTable>(
                ptr,
                &[ArrayVTableFields::current_element_impl as u64],
            ),
        }
    }
    fn build_single_forward_fun_get(
        &mut self,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
    ) -> PointerValue<'llvm> {
        match layout {
            Some(mono) => self.sym_ptr(mono, LayoutPart::SingleForward),
            None => self.vtable_get::<vtable::ArrayVTable>(
                ptr,
                &[ArrayVTableFields::single_forward_impl as u64],
            ),
        }
    }
    fn build_skip_fun_get(
        &mut self,
        layout: Option<IMonoLayout<'comp>>,
        ptr: PointerValue<'llvm>,
    ) -> PointerValue<'llvm> {
        match layout {
            Some(mono) => self.sym_ptr(mono, LayoutPart::Skip),
            None => {
                self.vtable_get::<vtable::ArrayVTable>(ptr, &[ArrayVTableFields::skip_impl as u64])
            }
        }
    }
    pub fn object_file(&mut self) {
        self.module.print_to_stderr();
        self.module.verify().expect("could not verify");
        self.target
            .write_to_file(&self.module, FileType::Object, Path::new("a.out"))
            .expect("Could not create object file");
    }
}

enum InvokeInfo<T: TargetSized> {
    Direct {
        direct_part: LayoutPart,
        _type: PhantomData<T>,
    },
    VTable {
        gep_path: &'static [i32],
        _type: PhantomData<T>,
    },
}

impl<'llvm, 'comp> CodegenTypeContext for CodeGenCtx<'llvm, 'comp> {
    type Type = BasicTypeEnum<'llvm>;

    fn int(&mut self, bits: u8, _signed: bool) -> Self::Type {
        self.llvm.custom_width_int_type(bits as u32).into()
    }

    fn size(&mut self, _signed: bool) -> Self::Type {
        self.llvm
            .ptr_sized_int_type(&self.target.get_target_data(), None)
            .into()
    }

    fn char(&mut self) -> Self::Type {
        self.llvm.i32_type().into()
    }

    fn ptr(&mut self, inner: Self::Type) -> Self::Type {
        inner.ptr_type(AddressSpace::Generic).into()
    }

    fn zst(&mut self) -> Self::Type {
        self.llvm.i8_type().array_type(0).into()
    }

    fn array(&mut self, ty: Self::Type, size: crate::layout::prop::PSize) -> Self::Type {
        ty.array_type(size.try_into().expect("array size too big"))
            .into()
    }

    type StructType = inkwell::types::StructType<'llvm>;

    fn tuple(&mut self, fields: &[Self::Type]) -> Self::StructType {
        self.llvm.struct_type(fields, false)
    }

    fn fun_ptr(&mut self, args: &[Self::Type], ret: Self::Type) -> Self::Type {
        let args: Vec<_> = args.iter().map(|x| (*x).into()).collect();
        ret.fn_type(&args, false)
            .ptr_type(AddressSpace::Generic)
            .into()
    }
}
