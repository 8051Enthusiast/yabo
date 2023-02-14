mod convert_mir;
mod convert_regex;
mod convert_thunk;
mod defs;
mod funs;
mod getset;
mod val;
mod vtables;

use std::{ffi::OsStr, fmt::Debug, path::Path, rc::Rc};

use defs::TAILCC;
use fxhash::FxHashMap;
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
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

use val::{CgMonoValue, CgReturnValue, CgValue};
use yaboc_absint::{AbstractDomain, Arg};
use yaboc_base::config::Configs;
use yaboc_base::dbpanic;
use yaboc_base::interner::{DefId, FieldName, Identifier, Interner};
use yaboc_database::YabocDatabase;
use yaboc_dependents::RequirementSet;
use yaboc_hir::{BlockId, HirIdWrapper, Hirs, ParserDefId};
use yaboc_hir_types::{DerefLevel, TyHirs};
use yaboc_layout::{
    canon_layout,
    collect::{root_req, LayoutCollection, LayoutCollector},
    flat_layouts,
    mir_subst::FunctionSubstitute,
    prop::{CodegenTypeContext, PSize, SizeAlign, TargetSized},
    represent::{truncated_hex, LayoutPart},
    vtable::{
        self, ArrayVTableFields, BlockVTableFields, FunctionVTableFields, ParserFun,
        ParserVTableFields, VTableHeaderFields,
    },
    AbsLayoutCtx, ILayout, IMonoLayout, Layout, LayoutError, MonoLayout,
};
use yaboc_mir::{CallMeta, Mirs, ReturnStatus};
use yaboc_types::{PrimitiveType, Type, TypeInterner};

use self::{convert_mir::MirTranslator, convert_thunk::ThunkContext};

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
        let collected_layouts = Rc::new(layout_collector.into_results().unwrap());
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
        pmb.set_inliner_with_threshold(250);
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

    fn add_entry_block(&mut self, fun: FunctionValue<'llvm>) -> BasicBlock<'llvm> {
        let entry = self.llvm.append_basic_block(fun, "entry");
        self.builder.position_at_end(entry);
        entry
    }

    fn current_function(&self) -> FunctionValue<'llvm> {
        self.builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap()
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
        let Some(fun) = self
            .module
            .get_function(&sym)
        else {
            panic!("could not find symbol {sym}");
        };
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
        self.llvm.i8_type().ptr_type(AddressSpace::default())
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

    fn undef_ret(&self) -> CgReturnValue<'llvm> {
        let undef_ptr = self.any_ptr().get_undef();
        let head = self.const_i64(DerefLevel::max().into_shifted_runtime_value() as i64);
        CgReturnValue::new(head, undef_ptr)
    }

    fn build_sa_alloca(
        &mut self,
        sa: SizeAlign,
        vtable: Option<bool>,
        name: &str,
    ) -> PointerValue<'llvm> {
        let ty = self.sa_type(sa);
        let ptr = self.builder.build_alloca(ty, "alloca");
        self.set_last_instr_align(sa).unwrap();
        let u8_ptr_ty = self.llvm.i8_type().ptr_type(AddressSpace::default());
        match vtable {
            None => self.any_ptr().get_undef(),
            Some(false) => self
                .builder
                .build_bitcast(ptr, u8_ptr_ty, name)
                .into_pointer_value(),
            Some(true) => {
                let cast_ptr = self
                    .builder
                    .build_bitcast(ptr, u8_ptr_ty, name)
                    .into_pointer_value();
                let ptr_width = self.any_ptr().size_of();
                self.build_byte_gep(cast_ptr, ptr_width, name)
            }
        }
    }

    fn build_layout_alloca(&mut self, layout: ILayout<'comp>, name: &str) -> PointerValue<'llvm> {
        let sa = layout
            .size_align(self.layouts)
            .expect("Could not get size/alignment of layout");
        let vtable = match layout.layout.1 {
            Layout::None => None,
            Layout::Mono(_, _) => Some(false),
            Layout::Multi(_) => Some(true),
        };
        self.build_sa_alloca(sa, vtable, name)
    }

    fn build_alloca_value(&mut self, layout: ILayout<'comp>, name: &str) -> CgValue<'comp, 'llvm> {
        let ptr = self.build_layout_alloca(layout, name);
        CgValue::new(layout, ptr)
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

    fn build_tailcc_call_with_int_ret(
        &mut self,
        call_fun: CallableValue<'llvm>,
        args: &[BasicMetadataValueEnum<'llvm>],
    ) -> IntValue<'llvm> {
        let call = self.builder.build_call(call_fun, args, "call");
        call.set_call_convention(TAILCC);
        call.try_as_basic_value()
            .left()
            .expect("function shuold not return void")
            .into_int_value()
    }


    fn build_parser_call(
        &mut self,
        ret: CgReturnValue<'llvm>,
        fun: CgValue<'comp, 'llvm>,
        from: CgValue<'comp, 'llvm>,
        call_kind: CallMeta,
    ) -> IntValue<'llvm> {
        let slot = self.collected_layouts.parser_slots.layout_vtable_offsets
            [&((from.layout, call_kind), fun.layout)];
        self.call_parser_fun_wrapper(ret, fun, from, slot, call_kind.req)
    }

    fn module_string(&mut self, s: &str) -> PointerValue<'llvm> {
        let sym_name = format!("s${s}");
        if let Some(sym) = self.module.get_global(&sym_name) {
            return sym.as_pointer_value();
        }
        let cstr = self.llvm.const_string(s.as_bytes(), true);
        let global_value = self.module.add_global(cstr.get_type(), None, &sym_name);
        global_value.set_visibility(GlobalVisibility::Hidden);
        global_value.set_initializer(&cstr);
        global_value
            .as_pointer_value()
            .const_cast(self.llvm.i8_type().ptr_type(AddressSpace::default()))
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
                vtable::BlockFields::struct_type(self).ptr_type(AddressSpace::default()),
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
            .const_cast(vtable::BlockFields::struct_type(self).ptr_type(AddressSpace::default()))
    }

    fn build_discriminant_info(
        &mut self,
        block_id: BlockId,
        block: CgValue<'comp, 'llvm>,
        field: FieldName,
    ) -> Option<(PointerValue<'llvm>, IntValue<'llvm>)> {
        let manifestation = self.layouts.dcx.manifestation(block.layout);
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
        let byte_ptr = self.build_byte_gep(block.ptr, byte_offset, "gepdisc");
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
        field: DefId,
        val: CgValue<'comp, 'llvm>,
        field_layout: ILayout<'comp>,
    ) -> CgValue<'comp, 'llvm> {
        let mut offset = self.layouts.dcx.manifestation(val.layout).field_offsets[&field];
        if field_layout.is_multi() {
            offset += self.word_size();
        }
        let offset_llvm_int = self.llvm.i64_type().const_int(offset, false);

        let field_ptr = self.build_byte_gep(val.ptr, offset_llvm_int, "gepfield");
        CgValue::new(field_layout, field_ptr)
    }

    fn build_nominal_components(
        &mut self,
        val: CgMonoValue<'comp, 'llvm>,
        req: CallMeta,
    ) -> (CgValue<'comp, 'llvm>, CgMonoValue<'comp, 'llvm>, u64) {
        let MonoLayout::Nominal(pd, _, args) = val.layout.mono_layout().0 else {
            panic!("build_nominal_components has to be called with a nominal parser layout");
        };

        let parserdef = pd.lookup(&self.compiler_database.db).unwrap();
        let layout_sa = val.layout.inner().size_align(self.layouts).unwrap();
        let (from_layout, parser) = val.layout.unapply_nominal(self.layouts);
        let from_val = self.build_field_gep(parserdef.from.0, val.into(), from_layout);
        let slot = self.collected_layouts.parser_slots.layout_vtable_offsets
            [&((from_layout, req), parser.inner())];

        let arg_ptr = if !args.is_empty() {
            let mut from_sa = from_layout.size_align(self.layouts).unwrap();
            from_sa.align_mask |= layout_sa.align_mask;

            self.build_byte_gep(val.ptr, self.const_i64(from_sa.stride() as i64), "arg_ptr")
        } else {
            self.any_ptr().get_undef()
        };
        let arg = CgMonoValue::new(parser, arg_ptr);
        (from_val, arg, slot)
    }

    fn build_copy_invariant(&mut self, dest: CgValue<'comp, 'llvm>, src: CgValue<'comp, 'llvm>) {
        let sa = dest.layout.size_align(self.layouts).unwrap();
        let src_ptr = self.get_object_start(src);
        let dest_ptr = self.get_object_start(dest);
        let size = self.const_i64(sa.size as i64);
        let align = sa.align() as u32;
        self.builder
            .build_memcpy(dest_ptr, align, src_ptr, align, size)
            .unwrap();
    }

    fn create_pd_export(&mut self, pd: ParserDefId, layout: IMonoLayout<'comp>, slot: PSize) {
        let name = match self.compiler_database.db.def_name(pd.0).unwrap() {
            FieldName::Return => unreachable!(),
            FieldName::Ident(d) => self.compiler_database.db.lookup_intern_identifier(d).name,
        };
        let val = self.parser_impl_struct_val(layout, slot, root_req(), true);
        let global_ty = ParserFun::codegen_ty(self);
        let global = self.module.add_global(global_ty, None, &name);
        global.set_initializer(&val);
        global.set_linkage(Linkage::External);
        global.set_visibility(GlobalVisibility::Default);
    }

    pub fn create_pd_exports(&mut self) {
        let collected_layouts = self.collected_layouts.clone();
        for (layout, slot) in collected_layouts.root.iter() {
            if let MonoLayout::NominalParser(pd, _, _) = layout.mono_layout().0 {
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
        //  self.module.verify()?;
        //  self.pass_manager.run_on(&self.module);
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

fn get_fun_args<const N: usize>(fun: FunctionValue) -> [BasicValueEnum; N] {
    std::array::from_fn(|i| fun.get_nth_param(i as u32).unwrap())
}

fn parser_args(fun: FunctionValue) -> (PointerValue, PointerValue, IntValue, PointerValue) {
    let [ret, fun, head, from] = get_fun_args(fun);
    (
        ret.into_pointer_value(),
        fun.into_pointer_value(),
        head.into_int_value(),
        from.into_pointer_value(),
    )
}

fn parser_values<'comp, 'llvm>(
    fun: FunctionValue<'llvm>,
    fun_layout: IMonoLayout<'comp>,
    arg_layout: ILayout<'comp>,
) -> (
    CgReturnValue<'llvm>,
    CgMonoValue<'comp, 'llvm>,
    CgValue<'comp, 'llvm>,
) {
    let (ret, fun, head, from) = parser_args(fun);
    let ret = CgReturnValue::new(head, ret);
    let fun = CgMonoValue::new(fun_layout, fun);
    let arg = CgValue::new(arg_layout, from);
    (ret, fun, arg)
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
        inner.ptr_type(AddressSpace::default()).into()
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
            .ptr_type(AddressSpace::default())
            .into()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Value<'llvm, 'comp> {
    pub ptr: PointerValue<'llvm>,
    pub layout: ILayout<'comp>,
}
