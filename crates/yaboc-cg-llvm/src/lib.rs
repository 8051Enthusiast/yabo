mod convert_mir;
mod convert_thunk;
mod defs;
mod funs;
mod getset;
mod vtables;

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
    types::{ArrayType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, PointerType, StructType},
    values::{
        ArrayValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallableValue,
        FunctionValue, GlobalValue, IntValue, PointerValue, StructValue, UnnamedAddress,
    },
    AddressSpace, GlobalVisibility, IntPredicate, OptimizationLevel,
};

use yaboc_absint::{AbstractDomain, Arg};
use yaboc_base::config::Configs;
use yaboc_base::dbpanic;
use yaboc_base::interner::{DefId, FieldName, Identifier, Interner};
use yaboc_database::YabocDatabase;
use yaboc_hir::{BlockId, HirIdWrapper, Hirs, ParserDefId};
use yaboc_hir_types::TyHirs;
use yaboc_layout::{
    canon_layout,
    collect::{LayoutCollection, LayoutCollector},
    flat_layouts,
    mir_subst::FunctionSubstitute,
    prop::{CodegenTypeContext, PSize, SizeAlign, TargetSized},
    represent::{truncated_hex, LayoutPart},
    vtable::{
        self, ArrayVTableFields, BlockVTableFields, FunctionVTableFields, NominalVTableFields,
        ParserArgImplFields, ParserVTableFields, VTableHeaderFields,
    },
    AbsLayoutCtx, ILayout, IMonoLayout, Layout, LayoutError, MonoLayout,
};
use yaboc_mir::{CallKind, DupleField, Mirs, PdArgKind, ReturnStatus};
use yaboc_types::{PrimitiveType, Type, TypeInterner};

use self::{
    convert_mir::MirTranslator,
    convert_thunk::{ThunkContext, ThunkKind},
};

pub struct CodeGenCtx<'llvm, 'comp> {
    llvm: &'llvm Context,
    target: TargetMachine,
    target_data: TargetData,
    builder: Builder<'llvm>,
    pass_manager: PassManager<Module<'llvm>>,
    module: Module<'llvm>,
    compiler_database: &'comp yaboc_base::Context<YabocDatabase>,
    layouts: &'comp mut AbsLayoutCtx<'comp>,
    collected_layouts: Rc<LayoutCollection<'comp>>,
}

impl<'llvm, 'comp> CodeGenCtx<'llvm, 'comp> {
    pub fn new(
        llvm_context: &'llvm Context,
        compiler_database: &'comp yaboc_base::Context<YabocDatabase>,
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
        // pass_manager.add_always_inliner_pass();
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

    fn resize_struct_start_array(&self, st: StructType<'llvm>, size: u32) -> StructType<'llvm> {
        let mut field_types = st.get_field_types();
        if let Some(BasicTypeEnum::ArrayType(array)) = field_types.first_mut() {
            *array = array.get_element_type().array_type(size);
        } else {
            panic!("first field of struct is not an array")
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

    fn build_get_vtable_tag(&mut self, layout: IMonoLayout<'comp>) -> PointerValue<'llvm> {
        let vtable = self.sym_ptr(layout, LayoutPart::VTable);
        let vtable_ty = vtable.get_type().get_element_type().into_struct_type();
        let header = if matches!(
            vtable_ty.get_field_type_at_index(0),
            Some(BasicTypeEnum::ArrayType(_))
        ) {
            unsafe {
                vtable.const_in_bounds_gep(&[
                    self.llvm.i32_type().const_int(0, false),
                    self.llvm.i32_type().const_int(1, false),
                ])
            }
        } else {
            vtable
        };
        header.const_cast(self.any_ptr())
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

    fn const_size_t(&self, val: i64) -> IntValue<'llvm> {
        self.llvm
            .ptr_sized_int_type(&self.target_data, None)
            .const_int(val as u64, true)
    }

    fn const_i64(&self, val: i64) -> IntValue<'llvm> {
        self.llvm.i64_type().const_int(val as u64, true)
    }

    fn any_ptr(&self) -> PointerType<'llvm> {
        self.llvm.i8_type().ptr_type(AddressSpace::Generic)
    }

    fn word_size(&self) -> u64 {
        <*const u8>::tsize().size
    }

    fn sa_type(&mut self, sa: SizeAlign) -> ArrayType<'llvm> {
        self.llvm.i8_type().array_type(sa.size as u32)
    }

    fn set_last_instr_align(&mut self, sa: SizeAlign) -> Option<()> {
        let instr = self.builder.get_insert_block()?.get_last_instruction()?;
        instr.set_alignment(sa.align() as u32).ok()
    }

    fn build_layout_alloca(&mut self, layout: ILayout<'comp>, name: &str) -> PointerValue<'llvm> {
        let sa = layout
            .size_align(self.layouts)
            .expect("Could not get size/alignment of layout");
        let ty = self.sa_type(sa);
        let ptr = self.builder.build_alloca(ty, "alloca");
        self.set_last_instr_align(sa).unwrap();
        let u8_ptr_ty = self.llvm.i8_type().ptr_type(AddressSpace::Generic);
        match layout.layout.1 {
            Layout::None => self.any_ptr().get_undef(),
            Layout::Mono(_, _) => self
                .builder
                .build_bitcast(ptr, u8_ptr_ty, name)
                .into_pointer_value(),
            Layout::Multi(_) => {
                let cast_ptr = self
                    .builder
                    .build_bitcast(ptr, u8_ptr_ty, name)
                    .into_pointer_value();
                let ptr_width = self.any_ptr().size_of();
                self.build_byte_gep(cast_ptr, ptr_width, name)
            }
        }
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
        let len = self.const_size_t(field_info.len() as i64);
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
        let byte_ptr = self.build_byte_gep(block_ptr, byte_offset, "gepdisc");
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

    fn build_byte_gep(
        &mut self,
        ptr: PointerValue<'llvm>,
        int: IntValue<'llvm>,
        name: &str,
    ) -> PointerValue<'llvm> {
        assert!(ptr.get_type() == self.any_ptr());
        unsafe { self.builder.build_in_bounds_gep(ptr, &[int], name) }
    }

    fn build_field_gep(
        &mut self,
        layout: ILayout<'comp>,
        field: DefId,
        ptr: PointerValue<'llvm>,
        field_layout: ILayout<'comp>,
    ) -> PointerValue<'llvm> {
        let mut offset = self.layouts.dcx.manifestation(layout).field_offsets[&field];
        if field_layout.is_multi() {
            offset += self.word_size();
        }
        let offset_llvm_int = self.llvm.i64_type().const_int(offset, false);
        self.build_byte_gep(ptr, offset_llvm_int, "gepfield")
    }

    fn build_duple_gep(
        &mut self,
        layout: ILayout<'comp>,
        field: DupleField,
        ptr: PointerValue<'llvm>,
        inner_layout: ILayout<'comp>,
    ) -> PointerValue<'llvm> {
        if field == DupleField::First {
            return ptr;
        }
        let sa = layout.size_align(self.layouts).unwrap();
        let mut offset = match layout.layout.1 {
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
        if inner_layout.is_multi() {
            offset += self.word_size();
        }
        let llvm_int = self.llvm.i64_type().const_int(offset, false);
        self.build_byte_gep(ptr, llvm_int, "gepduple")
    }

    fn build_mono_ptr(
        &mut self,
        place_ptr: PointerValue<'llvm>,
        _layout: ILayout<'comp>,
    ) -> PointerValue<'llvm> {
        place_ptr
        //        match layout.layout.1 {
        //            Layout::None => self.any_ptr().get_undef(),
        //            Layout::Mono(_, _) => place_ptr,
        //            Layout::Multi(_) => {
        //                let ptr_width = self.any_ptr().size_of();
        //                self.build_byte_gep(place_ptr, ptr_width, "ml_ptr_skip")
        //            }
        //        }
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
            if let MonoLayout::NominalParser(pd, _) = layout.mono_layout().0 {
                self.create_pd_export(*pd, *layout, *slot);
            }
        }
    }

    pub fn create_free_fun(&mut self) {
        let free_fun = self.module.add_function(
            "yabo_free",
            self.llvm
                .void_type()
                .fn_type(&[self.any_ptr().into()], false),
            Some(Linkage::External),
        );
        let entry = self.llvm.append_basic_block(free_fun, "entry");
        self.builder.position_at_end(entry);
        let arg = free_fun.get_first_param().unwrap().into_pointer_value();
        self.builder.build_free(arg);
        self.builder.build_return(None);
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

fn get_fun_args<const N: usize>(fun: FunctionValue<'_>) -> [BasicValueEnum<'_>; N] {
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

    fn array(&mut self, ty: Self::Type, size: yaboc_layout::prop::PSize) -> Self::Type {
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
