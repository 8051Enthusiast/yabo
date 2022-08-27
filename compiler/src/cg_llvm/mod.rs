pub mod convert_mir;
pub mod convert_thunk;

use std::{ffi::OsStr, fmt::Debug, path::Path, rc::Rc};

use fxhash::FxHashMap;
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    passes::{PassManager, PassManagerBuilder},
    support::LLVMString,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetData, TargetMachine,
        TargetTriple,
    },
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, PointerType, StructType},
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallableValue, FunctionValue,
        GlobalValue, IntValue, PointerValue, StructValue, UnnamedAddress,
    },
    AddressSpace, GlobalVisibility, IntPredicate, OptimizationLevel,
};

use crate::{
    absint::{AbstractDomain, Arg},
    config::Configs,
    dbpanic,
    hir::{BlockId, HirIdWrapper, Hirs, ParserDefId},
    hir_types::TyHirs,
    interner::{DefId, FieldName, Identifier, Interner},
    layout::{
        canon_layout,
        collect::{LayoutCollection, LayoutCollector},
        mir_subst::FunctionSubstitute,
        prop::{CodegenTypeContext, PSize, SizeAlign, TargetSized},
        represent::{truncated_hex, LayoutPart},
        vtable::{
            self, ArrayVTableFields, BlockVTableFields, NominalVTableFields, ParserArgImplFields,
            ParserVTableFields, VTableHeaderFields,
        },
        AbsLayoutCtx, ILayout, IMonoLayout, Layout, LayoutError, MonoLayout,
    },
    mir::{CallKind, DupleField, Mirs, PdArgKind, ReturnStatus},
    types::{PrimitiveType, Type, TypeInterner},
};

use self::{convert_mir::MirTranslator, convert_thunk::ThunkContext};

pub struct CodeGenCtx<'llvm, 'comp> {
    llvm: &'llvm Context,
    target: TargetMachine,
    target_data: TargetData,
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
        let pds = compiler_database.db.all_exported_parserdefs();
        layout_collector.collect(&pds)?;
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
        let cfg = compiler_database.db.config();
        let triple = TargetTriple::create(&cfg.target_triple);
        let target = Target::from_triple(&triple)
            .expect("no")
            .create_target_machine(
                &triple,
                &cfg.target_cpu,
                &cfg.target_features,
                OptimizationLevel::Default,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .expect("Could not get target machine");
        let target_data = target.get_target_data();
        module.set_data_layout(&target.get_target_data().get_data_layout());
        module.set_triple(&triple);
        let pmb = PassManagerBuilder::create();
        pmb.set_optimization_level(OptimizationLevel::Default);
        // pmb.set_inliner_with_threshold(250);
        let pass_manager = PassManager::create(());
        pass_manager.add_always_inliner_pass();
        pmb.populate_module_pass_manager(&pass_manager);
        Ok(CodeGenCtx {
            llvm: llvm_context,
            target,
            builder,
            pass_manager,
            module,
            compiler_database,
            layouts,
            collected_layouts,
            target_data,
        })
    }
    fn set_always_inline(&self, fun: FunctionValue<'llvm>) {
        let alwaysinline = Attribute::get_named_enum_kind_id("alwaysinline");
        fun.add_attribute(
            AttributeLoc::Function,
            self.llvm.create_enum_attribute(alwaysinline, 0),
        );
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
        if let Some(f) = self.module.get_function(&sym) {
            return f.as_global_value().as_pointer_value();
        }
        if let Some(f) = self.module.get_global(&sym) {
            return f.as_pointer_value();
        }
        panic!("could not find symbol {sym}");
    }
    fn sym_callable(
        &mut self,
        layout: IMonoLayout<'comp>,
        part: LayoutPart,
    ) -> CallableValue<'llvm> {
        let sym = self.sym(layout, part);
        let fun = self
            .module
            .get_function(&sym)
            .expect("could not find function");
        fun.into()
    }
    fn const_size_t(&self, val: u64) -> IntValue<'llvm> {
        self.llvm
            .ptr_sized_int_type(&self.target_data, None)
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
        if let Some(x) = self.module.get_function(&sf_sym) {
            return x;
        }
        let sf_type = self.llvm.i64_type().fn_type(types, false);
        let fun = self
            .module
            .add_function(&sf_sym, sf_type, Some(Linkage::External));
        fun.as_global_value()
            .set_unnamed_address(UnnamedAddress::Global);
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
        let f = self.pip_fun_val(layout, LayoutPart::Field(name));
        self.set_always_inline(f);
        f
    }
    fn typecast_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        let f = self.pip_fun_val(layout, LayoutPart::Typecast);
        self.set_always_inline(f);
        f
    }
    fn parser_len_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        slot: PSize,
    ) -> FunctionValue<'llvm> {
        self.ppp_fun_val(layout, LayoutPart::LenImpl(slot))
    }
    fn deref_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pip_fun_val(layout, LayoutPart::Deref(true))
    }
    fn deref_impl_fun_val(&mut self, layout: IMonoLayout<'comp>) -> FunctionValue<'llvm> {
        self.pp_fun_val(layout, LayoutPart::Deref(false))
    }
    fn parser_val_impl_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        slot: PSize,
    ) -> FunctionValue<'llvm> {
        self.ppp_fun_val(layout, LayoutPart::ValImpl(slot, false))
    }
    fn parser_val_fun_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        slot: PSize,
    ) -> FunctionValue<'llvm> {
        self.ppip_fun_val(layout, LayoutPart::ValImpl(slot, true))
    }
    fn parser_impl_struct_val(
        &mut self,
        layout: IMonoLayout<'comp>,
        slot: PSize,
        is_non_null: bool,
    ) -> StructValue<'llvm> {
        let (len, val) = if is_non_null {
            let len_fn_ptr = self
                .parser_len_fun_val(layout, slot)
                .as_global_value()
                .as_pointer_value();
            let val_fn_ptr = self
                .parser_val_fun_val(layout, slot)
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
        self.llvm.const_struct(&[val.into(), len.into()], false)
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
        let array = self
            .llvm
            .i8_type()
            .array_type(sa.size.saturating_sub(align) as u32);
        self.llvm
            .struct_type(&[alignment_forcer.into(), array.into()], false)
    }
    fn build_layout_alloca(&mut self, layout: ILayout<'comp>) -> PointerValue<'llvm> {
        let sa = layout
            .size_align(self.layouts)
            .expect("Could not get size/alignment of layout");
        let ty = self.sa_type(sa);
        let ptr = self.builder.build_alloca(ty, &format!("alloca"));
        let u8_ptr_ty = self.llvm.i8_type().ptr_type(AddressSpace::Generic);
        self.builder
            .build_bitcast(ptr, u8_ptr_ty, "allocacast")
            .into_pointer_value()
    }

    fn build_call_with_int_ret(
        &mut self,
        call_fun: CallableValue<'llvm>,
        args: &[BasicMetadataValueEnum<'llvm>],
    ) -> IntValue<'llvm> {
        let call = self.builder.build_call(call_fun, args, "call");
        call.try_as_basic_value()
            .left()
            .expect("function shuold not return void")
            .into_int_value()
    }

    fn create_vtable<T: TargetSized>(&mut self, layout: IMonoLayout<'comp>) -> GlobalValue<'llvm> {
        let vtable_type = T::codegen_ty(self);
        let vtable_sym = self.sym(layout, LayoutPart::VTable);
        let vtable = self
            .module
            .add_global(vtable_type, Some(AddressSpace::Generic), &vtable_sym);
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
            .add_global(vtable_type, Some(AddressSpace::Generic), &vtable_sym);
        vtable.set_visibility(GlobalVisibility::Hidden);
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
        let deref = self.deref_fun_val(layout);
        let start = self.start_fun_val(layout);
        let end = self.end_fun_val(layout);
        let vtable_val = self.llvm.const_struct(
            &[
                vtable_header.into(),
                deref.as_global_value().as_pointer_value().into(),
                start.as_global_value().as_pointer_value().into(),
                end.as_global_value().as_pointer_value().into(),
            ],
            false,
        );
        vtable.set_initializer(&vtable_val);
    }

    fn module_string(&mut self, s: &str) -> PointerValue<'llvm> {
        let sym_name = format!("s${}", s);
        if let Some(sym) = self.module.get_global(&sym_name) {
            return sym.as_pointer_value();
        }
        let cstr = self.llvm.const_string(s.as_bytes(), true);
        let global_value = self.module.add_global(cstr.get_type(), None, &sym_name);
        global_value.set_visibility(GlobalVisibility::Hidden);
        global_value.set_initializer(&cstr);
        global_value
            .as_pointer_value()
            .const_cast(self.llvm.i8_type().ptr_type(AddressSpace::Generic))
    }

    fn field_info(&mut self, name: Identifier) -> PointerValue<'llvm> {
        let name = self
            .compiler_database
            .db
            .lookup_intern_identifier(name)
            .name;
        self.module_string(&name)
    }

    fn block_info(&mut self, block: BlockId) -> PointerValue<'llvm> {
        let hash = self.compiler_database.db.def_hash(block.0);
        let info_sym = format!("block_info${}", &truncated_hex(&hash));
        if let Some(val) = self.module.get_global(&info_sym) {
            return val.as_pointer_value().const_cast(
                vtable::BlockFields::struct_type(self).ptr_type(AddressSpace::Generic),
            );
        }

        let fields = self
            .compiler_database
            .db
            .sorted_block_fields(block, false)
            .unwrap();
        let mut field_info = Vec::new();
        for field in fields.iter() {
            let name = match field {
                FieldName::Ident(id) => id,
                FieldName::Return => panic!("return field should not be in fields"),
            };
            field_info.push(self.field_info(*name));
        }

        let info_type = vtable::BlockFields::struct_type(self);
        let info_type = self.resize_struct_end_array(info_type, field_info.len() as u32);
        let field_info_array = <*const u8>::codegen_ty(self)
            .into_pointer_type()
            .const_array(&field_info);
        let len = self.const_size_t(field_info.len() as u64);
        let info_val = self
            .llvm
            .const_struct(&[len.into(), field_info_array.into()], false);
        let global = self.module.add_global(info_type, None, &info_sym);
        global.set_initializer(&info_val);
        global.set_visibility(GlobalVisibility::Hidden);
        global
            .as_pointer_value()
            .const_cast(vtable::BlockFields::struct_type(self).ptr_type(AddressSpace::Generic))
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
        let access_array = <fn(*const u8, i64, *mut u8) -> i64>::codegen_ty(self)
            .into_pointer_type()
            .const_array(&funs);
        let vtable_val = self.llvm.const_struct(
            &[vtable_header.into(), block_info.into(), access_array.into()],
            false,
        );
        vtable.set_initializer(&vtable_val);
    }

    fn create_parser_vtable(&mut self, layout: IMonoLayout<'comp>) {
        let slots = self.collected_layouts.parser_occupied_entries[&layout].clone();
        let max = slots.keys().copied().max().unwrap_or(0);
        let vtable = self.create_resized_vtable::<vtable::ParserVTable>(layout, (max + 1) as u32);
        let vtable_header = self.vtable_header(layout);
        let vtable_impls: Vec<_> = (0..=max)
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
        for layout in collected_layouts.primitives.iter() {
            self.create_primitive_vtable(*layout);
        }
    }
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
    fn build_deref_fun_get(
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
    fn build_typecast_fun_get(
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
    fn build_field_access_fun_get(
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
    fn build_parser_fun_get(
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
    fn build_current_element_fun_get(
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
    fn build_single_forward_fun_get(
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
    fn build_head_disc_get(
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
    fn build_check_i64_bit_set(&mut self, val: IntValue<'llvm>, bit: u64) -> IntValue<'llvm> {
        let set_bit = self.const_i64(1 << bit);
        let and = self.builder.build_and(set_bit, val, "");
        self.builder
            .build_int_compare(IntPredicate::NE, and, self.const_i64(0), "")
    }

    fn build_discriminant_info(
        &mut self,
        block_id: BlockId,
        block_ptr: PointerValue<'llvm>,
        layout: ILayout<'comp>,
        field: FieldName,
    ) -> Option<(PointerValue<'llvm>, IntValue<'llvm>)> {
        let manifestation = self.layouts.dcx.manifestation(layout);
        let disc_offset = manifestation.discriminant_offset;
        let root_ctx = block_id
            .lookup(&self.compiler_database.db)
            .unwrap()
            .root_context;
        let def_id = root_ctx.0.child_field(&self.compiler_database.db, field);
        let inner_offset = manifestation.discriminant_mapping.get(&def_id)?;
        let byte_offset = self
            .llvm
            .i32_type()
            .const_int(disc_offset + inner_offset / 8, false);
        let bit_offset = inner_offset % 8;
        let shifted_bit = self.llvm.i8_type().const_int(1 << bit_offset, false);
        let byte_ptr = unsafe {
            self.builder
                .build_in_bounds_gep(block_ptr, &[byte_offset], "gepdisc")
        };
        Some((byte_ptr, shifted_bit))
    }

    fn build_discriminant_check(
        &mut self,
        byte_ptr: PointerValue<'llvm>,
        shifted_bit: IntValue<'llvm>,
    ) -> IntValue<'llvm> {
        let byte = self
            .builder
            .build_load(byte_ptr, "ld_assert_field")
            .into_int_value();
        let and_byte = self
            .builder
            .build_and(byte, shifted_bit, "cmp_assert_field");
        self.builder.build_int_compare(
            IntPredicate::NE,
            and_byte,
            self.llvm.i8_type().const_zero(),
            "and_assert_not_zero",
        )
    }

    fn build_cast<T: TargetSized, R: TryFrom<BasicValueEnum<'llvm>>>(
        &mut self,
        v: impl BasicValue<'llvm>,
    ) -> R
    where
        R::Error: Debug,
    {
        let ty = T::codegen_ty(self);
        let casted = self.builder.build_bitcast(v, ty, "cast");
        R::try_from(casted).expect("could not cast")
    }

    fn create_typecast(&mut self, layout: IMonoLayout<'comp>) {
        ThunkContext::new(self, layout).build();
    }

    fn build_field_gep(
        &mut self,
        layout: ILayout<'comp>,
        field: DefId,
        ptr: PointerValue<'llvm>,
    ) -> PointerValue<'llvm> {
        let offset = self.layouts.dcx.manifestation(layout).field_offsets[&field];
        let offset_llvm_int = self.llvm.i64_type().const_int(offset, false);
        unsafe {
            self.builder
                .build_in_bounds_gep(ptr, &[offset_llvm_int], "")
        }
    }

    fn build_duple_gep(
        &mut self,
        layout: ILayout<'comp>,
        field: DupleField,
        ptr: PointerValue<'llvm>,
    ) -> PointerValue<'llvm> {
        if field == DupleField::First {
            return ptr;
        }
        let sa = layout.size_align(self.layouts).unwrap();
        let offset = match layout.layout {
            Layout::Mono(MonoLayout::ComposedParser(_, _, second), _) => {
                let second_sa = second.size_align(self.layouts).unwrap();
                sa.size - second_sa.size
            }
            _ => dbpanic!(
                &self.compiler_database.db,
                "duple field on non-composed or non-mono-layout place {}",
                &layout
            ),
        };
        let llvm_int = self.llvm.i64_type().const_int(offset, false);
        unsafe { self.builder.build_in_bounds_gep(ptr, &[llvm_int], "") }
    }

    fn build_mono_ptr(
        &mut self,
        place_ptr: PointerValue<'llvm>,
        layout: ILayout<'comp>,
    ) -> PointerValue<'llvm> {
        match layout.layout {
            Layout::None => self.any_ptr().get_undef(),
            Layout::Mono(_, _) => place_ptr,
            Layout::Multi(_) => {
                let ptr_width = self.any_ptr().size_of();
                unsafe {
                    self.builder
                        .build_in_bounds_gep(place_ptr, &[ptr_width], "ml_ptr_skip")
                }
            }
        }
    }

    fn mir_pd_fun(
        &mut self,
        from: ILayout<'comp>,
        layout: IMonoLayout<'comp>,
        call_kind: CallKind,
    ) -> FunctionSubstitute<'comp> {
        let pd = if let MonoLayout::NominalParser(id) = layout.mono_layout().0 {
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
        let pd = if let MonoLayout::Nominal(id, _) = layout.mono_layout().0 {
            id
        } else {
            panic!("mir_pd_len_fun has to be called with a nominal parser layout");
        };
        let mir = self
            .compiler_database
            .db
            .mir_pd(*pd, call_kind, PdArgKind::Thunk)
            .unwrap();
        let fun = ILayout::bottom(&mut self.layouts.dcx);
        FunctionSubstitute::new_from_pd(
            mir,
            layout.inner(),
            fun,
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
        let [arg, ret] = get_fun_args(deref_impl).map(|x| x.into_pointer_value());
        let fun = self.any_ptr().get_undef();
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
        let inc_ptr = unsafe {
            self.builder
                .build_in_bounds_gep(ptr, &[self.const_i64(1)], "inc_ptr")
        };
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
        let inc_ptr = unsafe { self.builder.build_in_bounds_gep(ptr, &[len], "inc_ptr") };
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
        let int_buf = self.build_layout_alloca(int_layout);
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
        let first_slot = self.collected_layouts.call_slots[&(from, first_layout)];
        let inner_layout = first_layout.apply_arg(self.layouts, from).unwrap();
        let inner_head = self.const_i64(self.compiler_database.db.head_discriminant(inner_ty));
        let second_slot = self.collected_layouts.call_slots[&(inner_layout, second_layout)];
        let second_arg = self.build_layout_alloca(inner_layout);
        let second_len_ret = self.build_layout_alloca(inner_layout);
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
        let first_slot = self.collected_layouts.call_slots[&(from, first_layout)];
        let inner_layout = first_layout.apply_arg(self.layouts, from).unwrap();
        let inner_head = self.const_i64(self.compiler_database.db.head_discriminant(inner_ty));
        let second_slot = self.collected_layouts.call_slots[&(inner_layout, second_layout)];
        let second_arg = self.build_layout_alloca(inner_layout);
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
        let return_buffer = self.build_layout_alloca(return_layout);
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

    fn create_all_typecast_funs(&mut self) {
        let collected_layouts = self.collected_layouts.clone();
        for layout in [
            &collected_layouts.arrays,
            &collected_layouts.blocks,
            &collected_layouts.nominals,
            &collected_layouts.parsers,
            &collected_layouts.primitives,
        ]
        .into_iter()
        .flatten()
        {
            self.create_typecast(*layout)
        }
    }

    fn create_parser_funs(&mut self, layout: IMonoLayout<'comp>) {
        let collected_layouts = self.collected_layouts.clone();
        for (slot, from) in &collected_layouts.parser_occupied_entries[&layout] {
            match layout.mono_layout().0 {
                MonoLayout::Single => {
                    self.create_single_len(from, layout, *slot);
                    self.create_single_val(from, layout, *slot);
                }
                MonoLayout::NominalParser(pd) => {
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
    }

    fn create_pd_export(&mut self, pd: ParserDefId, layout: IMonoLayout<'comp>, slot: PSize) {
        let name = match self.compiler_database.db.def_name(pd.0).unwrap() {
            FieldName::Return => unreachable!(),
            FieldName::Ident(d) => self.compiler_database.db.lookup_intern_identifier(d).name,
        };
        let val = self.parser_impl_struct_val(layout, slot, true);
        let global_ty = vtable::ParserArgImpl::struct_type(self);
        let global = self.module.add_global(global_ty, None, &name);
        global.set_initializer(&val);
        global.set_linkage(Linkage::External);
        global.set_visibility(GlobalVisibility::Default);
    }

    pub fn create_pd_exports(&mut self) {
        let collected_layouts = self.collected_layouts.clone();
        for (layout, slot) in collected_layouts.root.iter() {
            if let MonoLayout::NominalParser(pd) = layout.mono_layout().0 {
                self.create_pd_export(*pd, *layout, *slot);
            }
        }
    }

    pub fn llvm_code(self, outfile: &OsStr) -> Result<(), LLVMString> {
        //        self.module.verify()?;
        //        self.pass_manager.run_on(&self.module);
        self.module.print_to_file(outfile)?;
        Ok(())
    }

    pub fn object_file(self, outfile: &OsStr) -> Result<(), LLVMString> {
        self.module.verify()?;
        self.pass_manager.run_on(&self.module);
        self.target
            .write_to_file(&self.module, FileType::Object, Path::new(outfile))?;
        Ok(())
    }
}

fn get_fun_args<'llvm, const N: usize>(fun: FunctionValue<'llvm>) -> [BasicValueEnum<'llvm>; N] {
    std::array::from_fn(|i| fun.get_nth_param(i as u32).unwrap())
}

impl<'llvm, 'comp> CodegenTypeContext for CodeGenCtx<'llvm, 'comp> {
    type Type = BasicTypeEnum<'llvm>;

    fn int(&mut self, bits: u8, _signed: bool) -> Self::Type {
        self.llvm.custom_width_int_type(bits as u32).into()
    }

    fn size(&mut self, _signed: bool) -> Self::Type {
        self.llvm.ptr_sized_int_type(&self.target_data, None).into()
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
