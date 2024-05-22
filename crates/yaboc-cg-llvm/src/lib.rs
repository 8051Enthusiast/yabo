mod convert_mir;
mod convert_regex;
mod convert_thunk;
mod defs;
mod funs;
mod getset;
mod val;
mod vtables;

use std::{ffi::OsStr, fmt::Debug, path::Path, rc::Rc};

use fxhash::FxHashMap;
use getset::Callable;
pub use inkwell;
use inkwell::{
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    builder::{Builder, BuilderError},
    context::Context,
    module::{Linkage, Module},
    passes::PassBuilderOptions,
    support::LLVMString,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetData, TargetMachine,
        TargetTriple,
    },
    types::{ArrayType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, PointerType},
    values::{
        ArrayValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, GlobalValue,
        IntValue, PointerValue, StructValue, UnnamedAddress,
    },
    AddressSpace, GlobalVisibility, IntPredicate, OptimizationLevel,
};

use val::{CgMonoValue, CgReturnValue, CgValue};
use yaboc_absint::{AbstractDomain, Arg};
use yaboc_base::config::Configs;
use yaboc_base::dbpanic;
use yaboc_base::interner::{DefId, FieldName, Identifier, Interner};
use yaboc_database::YabocDatabase;
use yaboc_hir::{BlockId, HirIdWrapper, Hirs, ParserDefId};
use yaboc_hir_types::{DerefLevel, TyHirs};
use yaboc_layout::{
    canon_layout,
    collect::{root_req, LayoutCollection},
    mir_subst::FunctionSubstitute,
    represent::{truncated_hex, LayoutPart},
    vtable::{
        self, ArrayVTableFields, BlockVTableFields, FunctionVTableFields, ParserFun,
        ParserVTableFields, VTableHeaderFields,
    },
    AbsLayoutCtx, ILayout, IMonoLayout, MonoLayout,
};
use yaboc_mir::{CallMeta, Mirs, ReturnStatus};
use yaboc_req::RequirementSet;
use yaboc_target::layout::{CodegenTypeContext, PSize, SizeAlign, TargetSized};
use yaboc_types::{PrimitiveType, Type, TypeInterner};

use self::{convert_mir::MirTranslator, convert_thunk::ThunkContext};

pub type IResult<T> = Result<T, BuilderError>;

pub struct CodeGenCtx<'llvm, 'comp> {
    llvm: &'llvm Context,
    target: TargetMachine,
    target_data: TargetData,
    options: CodeGenOptions,
    builder: Builder<'llvm>,
    pass_options: PassBuilderOptions,
    module: Module<'llvm>,
    compiler_database: &'comp yaboc_base::Context<YabocDatabase>,
    layouts: &'comp mut AbsLayoutCtx<'comp>,
    collected_layouts: Rc<LayoutCollection<'comp>>,
}

#[derive(Clone)]
pub struct CodeGenOptions {
    pub target: yaboc_target::Target,
    pub asan: bool,
    pub msan: bool,
}

impl<'llvm, 'comp> CodeGenCtx<'llvm, 'comp> {
    pub fn new(
        llvm_context: &'llvm Context,
        compiler_database: &'comp yaboc_base::Context<YabocDatabase>,
        layouts: &'comp mut AbsLayoutCtx<'comp>,
        options: CodeGenOptions,
    ) -> Result<Self, String> {
        let pds = compiler_database.db.all_exported_parserdefs();
        let collected_layouts =
            Rc::new(yaboc_layout::collect::collected_layouts(layouts, &pds).unwrap());
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
            .map_err(|e| e.to_string())?
            .create_target_machine(
                &triple,
                &options.target.cpu,
                &options.target.features,
                OptimizationLevel::Aggressive,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .ok_or_else(|| String::from("Could not create target machine"))?;
        let target_data = target.get_target_data();
        module.set_data_layout(&target.get_target_data().get_data_layout());
        module.set_triple(&triple);
        let pbo = PassBuilderOptions::create();
        pbo.set_loop_interleaving(true);
        pbo.set_loop_unrolling(true);
        pbo.set_merge_functions(true);
        pbo.set_loop_vectorization(true);
        pbo.set_loop_slp_vectorization(true);
        Ok(CodeGenCtx {
            llvm: llvm_context,
            target,
            options,
            builder,
            pass_options: pbo,
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

    fn resize_struct_end_array(&self, field_types: &mut [BasicTypeEnum<'llvm>], size: u32) {
        if let Some(BasicTypeEnum::ArrayType(array)) = field_types.last_mut() {
            *array = array.get_element_type().array_type(size);
        } else {
            panic!("last field of struct is not an array")
        };
    }

    fn resize_struct_start_array(&self, field_types: &mut [BasicTypeEnum<'llvm>], size: u32) {
        if let Some(BasicTypeEnum::ArrayType(array)) = field_types.first_mut() {
            *array = array.get_element_type().array_type(size);
        } else {
            panic!("first field of struct is not an array")
        };
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
        if fun.count_basic_blocks() > 0 {
            panic!("function {:?} already has basic blocks", fun.get_name())
        }
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
        // parsers and function have an extra array at the front of the vtable, which we will have to skip
        let vtable_ty_name = self.sym(layout, LayoutPart::VTableTy);
        let vtable_ty = self.llvm.get_struct_type(&vtable_ty_name).unwrap();
        let header = if matches!(
            vtable_ty.get_field_type_at_index(0),
            Some(BasicTypeEnum::ArrayType(_))
        ) {
            unsafe {
                vtable.const_in_bounds_gep(
                    vtable_ty,
                    &[
                        self.llvm.i32_type().const_int(0, false),
                        self.llvm.i32_type().const_int(1, false),
                    ],
                )
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
    ) -> FunctionValue<'llvm> {
        let sym = self.sym(layout, part);
        let Some(fun) = self.module.get_function(&sym) else {
            dbpanic!(
                &self.compiler_database.db,
                "could not find symbol {sym} of layout {}",
                &layout
            );
        };
        fun
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
        self.llvm.ptr_type(AddressSpace::default())
    }

    fn invalid_ptr(&self) -> PointerValue<'llvm> {
        self.any_ptr().const_null()
    }

    fn word_size(&self) -> u64 {
        <*const u8>::tsize(&self.options.target.data).after
    }

    fn sa_type(&mut self, sa: SizeAlign) -> ArrayType<'llvm> {
        self.llvm.i8_type().array_type(sa.allocation_size() as u32)
    }

    fn set_last_instr_align(&mut self, sa: SizeAlign) -> Option<()> {
        let instr = self.builder.get_insert_block()?.get_last_instruction()?;
        instr.set_alignment(sa.align() as u32).ok()
    }

    fn undef_ret(&self) -> CgReturnValue<'llvm> {
        let undef_ptr = self.invalid_ptr();
        let head = self.const_i64(DerefLevel::max().into_shifted_runtime_value() as i64);
        CgReturnValue::new(head, undef_ptr)
    }

    fn undef_val(&mut self) -> CgValue<'comp, 'llvm> {
        let undef_ptr = self.invalid_ptr();
        let layout = ILayout::bottom(&mut self.layouts.dcx);
        CgValue::new(layout, undef_ptr)
    }

    fn build_center_gep(
        &mut self,
        outer_ptr: PointerValue<'llvm>,
        layout: ILayout<'comp>,
    ) -> IResult<PointerValue<'llvm>> {
        let sa = layout.size_align(self.layouts).unwrap();
        let offset = sa.allocation_center_offset();
        if offset == 0 {
            return Ok(outer_ptr);
        }
        self.build_byte_gep(outer_ptr, self.const_i64(offset as i64), "alloc_center")
    }

    fn build_sa_alloca(&mut self, sa: SizeAlign, name: &str) -> IResult<PointerValue<'llvm>> {
        let ty = self.sa_type(sa);
        let ptr = self.builder.build_alloca(ty, name)?;
        self.set_last_instr_align(sa).unwrap();
        let cast_ptr = self.build_cast::<*mut u8, _>(ptr)?;
        let offset = sa.allocation_center_offset();
        if offset == 0 {
            return Ok(cast_ptr);
        }
        self.build_byte_gep(cast_ptr, self.const_i64(offset as i64), name)
    }

    fn build_layout_alloca(
        &mut self,
        layout: ILayout<'comp>,
        name: &str,
    ) -> IResult<PointerValue<'llvm>> {
        let sa = layout
            .size_align(self.layouts)
            .expect("Could not get size/alignment of layout");
        self.build_sa_alloca(sa, name)
    }

    fn build_alloca_value(
        &mut self,
        layout: ILayout<'comp>,
        name: &str,
    ) -> IResult<CgValue<'comp, 'llvm>> {
        let ptr = self.build_layout_alloca(layout, name)?;
        Ok(CgValue::new(layout, ptr))
    }

    fn build_alloca_mono_value(
        &mut self,
        layout: IMonoLayout<'comp>,
        name: &str,
    ) -> IResult<CgMonoValue<'comp, 'llvm>> {
        let ptr = self.build_layout_alloca(layout.inner(), name)?;
        Ok(CgMonoValue::new(layout, ptr))
    }

    fn build_alloca_int(&mut self, name: &str) -> IResult<CgMonoValue<'comp, 'llvm>> {
        let int_layout = self.layouts.dcx.int(self.layouts.db);
        let ptr = self.build_layout_alloca(int_layout, name)?;
        Ok(CgMonoValue::new(int_layout.maybe_mono().unwrap(), ptr))
    }

    fn build_call_with_int_ret(
        &mut self,
        call_fun: Callable<'llvm>,
        args: &[BasicMetadataValueEnum<'llvm>],
    ) -> IResult<IntValue<'llvm>> {
        let call = match call_fun {
            Callable::Pointer(ptr, ty) => self
                .builder
                .build_indirect_call(ty, ptr, args, "call")?
                .try_as_basic_value(),
            Callable::Function(fun) => self
                .builder
                .build_call(fun, args, "call")?
                .try_as_basic_value(),
        };
        Ok(call
            .left()
            .expect("function shuold not return void")
            .into_int_value())
    }

    fn build_tailcc_call_with_int_ret(
        &mut self,
        call_fun: FunctionValue<'llvm>,
        args: &[BasicMetadataValueEnum<'llvm>],
    ) -> IResult<IntValue<'llvm>> {
        let call = self.builder.build_call(call_fun, args, "call")?;
        call.set_call_convention(self.tailcc());
        Ok(call
            .try_as_basic_value()
            .left()
            .expect("function shuold not return void")
            .into_int_value())
    }

    fn build_parser_call(
        &mut self,
        ret: CgReturnValue<'llvm>,
        fun: CgValue<'comp, 'llvm>,
        from: CgValue<'comp, 'llvm>,
        call_kind: CallMeta,
    ) -> IResult<IntValue<'llvm>> {
        self.call_parser_fun_wrapper(ret, fun, from, call_kind.req)
    }

    fn build_byte_load(
        &mut self,
        ptr: PointerValue<'llvm>,
        name: &str,
    ) -> IResult<IntValue<'llvm>> {
        let ty = self.llvm.i8_type();
        Ok(self.builder.build_load(ty, ptr, name)?.into_int_value())
    }

    fn build_char_load(
        &mut self,
        ptr: PointerValue<'llvm>,
        name: &str,
    ) -> IResult<IntValue<'llvm>> {
        let ptr = self.build_cast::<*const char, _>(ptr)?;
        let ty = self.llvm.i32_type();
        Ok(self.builder.build_load(ty, ptr, name)?.into_int_value())
    }

    fn build_i64_load(&mut self, ptr: PointerValue<'llvm>, name: &str) -> IResult<IntValue<'llvm>> {
        let ptr = self.build_cast::<*const i64, _>(ptr)?;
        let ty = self.llvm.i64_type();
        Ok(self.builder.build_load(ty, ptr, name)?.into_int_value())
    }

    fn build_size_load(
        &mut self,
        ptr: PointerValue<'llvm>,
        name: &str,
    ) -> IResult<IntValue<'llvm>> {
        let ptr = self.build_cast::<*const usize, _>(ptr)?;
        let ty = self.llvm.ptr_sized_int_type(&self.target_data, None);
        Ok(self.builder.build_load(ty, ptr, name)?.into_int_value())
    }

    fn build_ptr_load(
        &mut self,
        ptr: PointerValue<'llvm>,
        name: &str,
    ) -> IResult<PointerValue<'llvm>> {
        let ptr = self.build_cast::<*const *mut u8, _>(ptr)?;
        let ty = self.any_ptr();
        Ok(self.builder.build_load(ty, ptr, name)?.into_pointer_value())
    }

    fn module_string(&mut self, s: &str) -> PointerValue<'llvm> {
        let sym_name = format!("s${s}");
        if let Some(sym) = self.module.get_global(&sym_name) {
            return sym.as_pointer_value().const_cast(self.any_ptr());
        }
        let cstr = self.llvm.const_string(s.as_bytes(), true);
        let global_value = self.module.add_global(cstr.get_type(), None, &sym_name);
        global_value.set_visibility(GlobalVisibility::Hidden);
        global_value.set_initializer(&cstr);
        global_value.as_pointer_value().const_cast(self.any_ptr())
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
            return val.as_pointer_value();
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

        let mut info_fields = vtable::BlockFields::struct_type(self).get_field_types();
        self.resize_struct_end_array(&mut info_fields, field_info.len() as u32);
        let info_type = self.llvm.struct_type(&info_fields, false);
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
        global.as_pointer_value()
    }

    fn build_discriminant_info(
        &mut self,
        block_id: BlockId,
        block: CgValue<'comp, 'llvm>,
        field: FieldName,
    ) -> IResult<Option<(PointerValue<'llvm>, IntValue<'llvm>)>> {
        let manifestation = self.layouts.dcx.manifestation(block.layout);
        let disc_offset = manifestation.discriminant_offset;
        let root_ctx = block_id
            .lookup(&self.compiler_database.db)
            .unwrap()
            .root_context;
        let def_id = root_ctx.0.child_field(&self.compiler_database.db, field);
        let Some(inner_offset) = manifestation.discriminant_mapping.get(&def_id) else {
            return Ok(None);
        };
        let byte_offset = self
            .llvm
            .i32_type()
            .const_int(disc_offset + inner_offset / 8, false);
        let bit_offset = inner_offset % 8;
        let shifted_bit = self.llvm.i8_type().const_int(1 << bit_offset, false);
        let byte_ptr = self.build_byte_gep(block.ptr, byte_offset, "gepdisc")?;
        Ok(Some((byte_ptr, shifted_bit)))
    }

    fn build_discriminant_check(
        &mut self,
        byte_ptr: PointerValue<'llvm>,
        shifted_bit: IntValue<'llvm>,
    ) -> IResult<IntValue<'llvm>> {
        let i8_ty = self.llvm.i8_type();
        let byte = self
            .builder
            .build_load(i8_ty, byte_ptr, "ld_assert_field")?
            .into_int_value();
        let and_byte = self
            .builder
            .build_and(byte, shifted_bit, "cmp_assert_field")?;
        let comp = self.builder.build_int_compare(
            IntPredicate::NE,
            and_byte,
            self.llvm.i8_type().const_zero(),
            "and_assert_not_zero",
        )?;
        Ok(comp)
    }

    fn branch<T, E>(
        &mut self,
        cond: IntValue<'llvm>,
        then: impl FnOnce(&mut Self) -> IResult<T>,
        else_: impl FnOnce(&mut Self) -> IResult<E>,
    ) -> IResult<(T, E)> {
        let fun = self.current_function();
        let then_bb = self.llvm.append_basic_block(fun, "then");
        let else_bb = self.llvm.append_basic_block(fun, "else");
        self.builder
            .build_conditional_branch(cond, then_bb, else_bb)?;
        self.builder.position_at_end(then_bb);
        let t = then(self)?;
        let then_cont_bb = self.builder.get_insert_block().unwrap();
        let then_unterminated = then_cont_bb.get_terminator().is_none();

        self.builder.position_at_end(else_bb);
        let e = else_(self)?;
        let else_cont_bb = self.builder.get_insert_block().unwrap();
        let else_unterminated = else_cont_bb.get_terminator().is_none();
        match (then_unterminated, else_unterminated) {
            // both branches need a terminator, so we need to add a branch to a common continuation block
            (true, true) => {
                let cont_bb = self.llvm.append_basic_block(fun, "cont");
                self.builder.build_unconditional_branch(cont_bb)?;
                self.builder.position_at_end(then_cont_bb);
                self.builder.build_unconditional_branch(cont_bb)?;
                self.builder.position_at_end(cont_bb);
            }
            // only the `then` branch is unterminated, so we can just continue there
            (true, false) => {
                self.builder.position_at_end(then_cont_bb);
            }
            // only the `else` branch is unterminated, and we are already there
            (false, _) => {}
        }
        Ok((t, e))
    }

    fn build_cast<T: TargetSized, R: TryFrom<BasicValueEnum<'llvm>>>(
        &mut self,
        v: impl BasicValue<'llvm>,
    ) -> IResult<R>
    where
        R::Error: Debug,
    {
        let ty = T::codegen_ty(self);
        let casted = self.builder.build_bit_cast(v, ty, "cast")?;
        Ok(R::try_from(casted).expect("could not cast"))
    }

    fn build_byte_gep(
        &mut self,
        ptr: PointerValue<'llvm>,
        int: IntValue<'llvm>,
        name: &str,
    ) -> IResult<PointerValue<'llvm>> {
        assert!(ptr.get_type() == self.any_ptr());
        let ty = self.llvm.i8_type();
        let byte = unsafe { self.builder.build_in_bounds_gep(ty, ptr, &[int], name) }?;
        Ok(byte)
    }

    fn const_byte_gep(&mut self, ptr: PointerValue<'llvm>, int: i64) -> PointerValue<'llvm> {
        assert!(ptr.get_type() == self.any_ptr());
        if int == 0 {
            return ptr;
        }
        let ty = self.llvm.i8_type();
        let int = self.llvm.i32_type().const_int(int as u64, false);
        unsafe { ptr.const_gep(ty, &[int]) }
    }

    fn build_field_gep(
        &mut self,
        field: DefId,
        val: CgValue<'comp, 'llvm>,
        field_layout: ILayout<'comp>,
    ) -> IResult<CgValue<'comp, 'llvm>> {
        let offset = self.layouts.dcx.manifestation(val.layout).field_offsets[&field];
        let offset_llvm_int = self.llvm.i64_type().const_int(offset, false);

        let field_ptr = self.build_byte_gep(val.ptr, offset_llvm_int, "gepfield")?;
        Ok(CgValue::new(field_layout, field_ptr))
    }

    fn build_nominal_components(
        &mut self,
        val: CgMonoValue<'comp, 'llvm>,
    ) -> IResult<(CgValue<'comp, 'llvm>, CgMonoValue<'comp, 'llvm>)> {
        let MonoLayout::Nominal(pd, _, args) = val.layout.mono_layout().0 else {
            panic!("build_nominal_components has to be called with a nominal parser layout");
        };

        let parserdef = pd.lookup(&self.compiler_database.db).unwrap();
        let layout_sa = val.layout.inner().size_align(self.layouts).unwrap();
        let (from_layout, parser) = val.layout.unapply_nominal(self.layouts);
        let from_val = self.build_field_gep(parserdef.from.unwrap().0, val.into(), from_layout)?;
        let arg_ptr = if !args.is_empty() {
            let mut from_sa = from_layout.size_align(self.layouts).unwrap();
            from_sa.align_mask |= layout_sa.align_mask;
            self.build_byte_gep(val.ptr, self.const_i64(from_sa.stride() as i64), "arg_ptr")?
        } else {
            self.invalid_ptr()
        };
        let arg = CgMonoValue::new(parser, arg_ptr);
        Ok((from_val, arg))
    }

    fn build_copy_invariant(
        &mut self,
        dest: CgValue<'comp, 'llvm>,
        src: CgValue<'comp, 'llvm>,
    ) -> IResult<PointerValue<'llvm>> {
        let sa = dest.layout.size_align(self.layouts).unwrap();
        let src_ptr = self.get_object_start(src)?;
        let dest_ptr = self.get_object_start(dest)?;
        let size = self.const_i64(sa.total_size() as i64);
        let align = sa.start_alignment() as u32;
        self.builder
            .build_memcpy(dest_ptr, align, src_ptr, align, size)?;
        Ok(dest_ptr)
    }

    fn build_vtable_store(
        &mut self,
        dest: PointerValue<'llvm>,
        vtable: PointerValue<'llvm>,
    ) -> IResult<()> {
        let vtable_offset = self.const_i64(-(self.word_size() as i64));
        let before_ptr = self.build_byte_gep(dest, vtable_offset, "vtable_ptr_skip")?;
        let ret_vtable_ptr = self.build_cast::<*mut *const u8, _>(before_ptr)?;
        self.builder.build_store(ret_vtable_ptr, vtable)?;
        Ok(())
    }

    fn create_pd_export(
        &mut self,
        pd: ParserDefId,
        layout: IMonoLayout<'comp>,
        from: ILayout<'comp>,
    ) {
        let name = match self.compiler_database.db.def_name(pd.0).unwrap() {
            FieldName::Return => unreachable!(),
            FieldName::Ident(d) => self.compiler_database.db.lookup_intern_identifier(d).name,
        };
        let val = self.parser_impl_struct_val(layout, from, root_req());
        let global_ty = ParserFun::codegen_ty(self);
        let global = self.module.add_global(global_ty, None, &name);
        global.set_initializer(&val);
        global.set_linkage(Linkage::External);
        global.set_visibility(GlobalVisibility::Default);
    }

    pub fn create_pd_exports(&mut self) -> IResult<()> {
        let collected_layouts = self.collected_layouts.clone();
        let from = IMonoLayout::u8_array(self.layouts);
        for (layout, _) in collected_layouts.root.iter() {
            if let MonoLayout::NominalParser(pd, _, _) = layout.mono_layout().0 {
                self.create_pd_export(*pd, *layout, from.inner());
            }
        }
        Ok(())
    }

    pub fn create_max_buf_size(&mut self) {
        let size = self.const_size_t(self.collected_layouts.max_sa.total_size() as i64);
        let buf_size = self
            .module
            .add_global(size.get_type(), None, "yabo_max_buf_size");
        buf_size.set_initializer(&size);
        buf_size.set_linkage(Linkage::External);
        buf_size.set_constant(true);
    }

    pub fn run_codegen(&mut self) {
        self.create_all_vtables();
        self.create_all_statics().expect("Codegen failed");
        self.create_all_funs().expect("Codegen failed");
        self.create_pd_exports().expect("Codegen failed");
        self.create_max_buf_size();
    }

    pub fn llvm_code(self, outfile: &OsStr) -> Result<(), LLVMString> {
        //self.module.verify()?;
        //self.module
        //    .run_passes("always-inline,default<O3>", &self.target, self.pass_options)?;
        self.module.print_to_file(outfile)?;
        Ok(())
    }

    pub fn object_file(self, outfile: &OsStr) -> Result<(), LLVMString> {
        self.module.verify()?;
        let mut passes = String::from("always-inline,default<O3>");
        if self.options.asan {
            passes.push_str(",asan");
        }
        if self.options.msan {
            passes.push_str(",msan");
        }
        self.module
            .run_passes(&passes, &self.target, self.pass_options)?;
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

fn eval_fun_args(fun: FunctionValue) -> (PointerValue, PointerValue, IntValue) {
    let [ret, fun, head] = get_fun_args(fun);
    (
        ret.into_pointer_value(),
        fun.into_pointer_value(),
        head.into_int_value(),
    )
}

fn eval_fun_values<'comp, 'llvm>(
    fun: FunctionValue<'llvm>,
    fun_layout: IMonoLayout<'comp>,
) -> (CgReturnValue<'llvm>, CgMonoValue<'comp, 'llvm>) {
    let (ret, fun, head) = eval_fun_args(fun);
    let ret = CgReturnValue::new(head, ret);
    let fun = CgMonoValue::new(fun_layout, fun);
    (ret, fun)
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

    fn ptr(&mut self, _: Self::Type) -> Self::Type {
        self.llvm.ptr_type(AddressSpace::default()).into()
    }

    fn zst(&mut self) -> Self::Type {
        self.llvm.i8_type().array_type(0).into()
    }

    fn array(&mut self, ty: Self::Type, size: yaboc_target::layout::PSize) -> Self::Type {
        ty.array_type(size.try_into().expect("array size too big"))
            .into()
    }

    type StructType = inkwell::types::StructType<'llvm>;

    fn tuple(&mut self, fields: &[Self::Type]) -> Self::StructType {
        self.llvm.struct_type(fields, false)
    }

    fn fun_ptr(&mut self, _: &[Self::Type], _: Self::Type) -> Self::Type {
        self.llvm.ptr_type(AddressSpace::default()).into()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Value<'llvm, 'comp> {
    pub ptr: PointerValue<'llvm>,
    pub layout: ILayout<'comp>,
}
