use std::{collections::hash_map::Entry, env::current_dir};

const YABO_LANG_ID: u32 = 0x8051;
const LLVM_DEBUG_METADATA_VERSION: u64 = 3;

use fxhash::FxHashMap;
use inkwell::{
    AddressSpace,
    context::Context,
    debug_info::{
        AsDIScope, DIBasicType, DIFile, DILocation, DIScope, DISubroutineType, DIType,
        DebugInfoBuilder, LLVMDWARFTypeEncoding,
    },
    llvm_sys::debuginfo::LLVMDIFlagPrototyped,
    module::Module,
};
use yaboc_base::{
    dbformat, dbpanic,
    error::SResult,
    source::{FileId, IndirectSpan},
};
use yaboc_layout::{
    AbsLayoutCtx, ILayout, IMonoLayout, Layouts, MonoLayout, represent::truncated_hex,
};
use yaboc_target::layout::{SizeAlign, TargetLayoutData};
use yaboc_types::{DefId, PrimitiveType};

use crate::CodeGenCtx;

type InkResult<T> = Result<T, inkwell::Error>;

#[derive(Clone, Copy, Debug)]
pub struct DebugLocation<'llvm> {
    pub line: u32,
    pub col: u32,
    pub file: DIFile<'llvm>,
}

pub struct DebugBuilder<'llvm, 'comp> {
    pub(crate) builder: DebugInfoBuilder<'llvm>,
    type_memo: FxHashMap<ILayout<'comp>, DIType<'llvm>>,
    files: FxHashMap<FileId, DIFile<'llvm>>,
    cwd: String,
}

type DwEnc = LLVMDWARFTypeEncoding;
#[allow(unused)]
enum DwarfEncoding {
    Address = 0x01,
    Boolean = 0x02,
    ComplexFloat = 0x03,
    Float = 0x04,
    Signed = 0x05,
    SignedChar = 0x06,
    Unsigned = 0x07,
    UnsignedChar = 0x08,
    Ucs = 0x11,
}

impl<'llvm, 'comp> DebugBuilder<'llvm, 'comp> {
    pub fn new(module: &mut Module<'llvm>, db: &dyn Layouts) -> Result<Self, std::io::Error> {
        let cwd = current_dir()?;
        let ctx = module.get_context();
        let dwarf_version = ctx.metadata_string("Dwarf Version");
        let debug_info_version = ctx.metadata_string("Debug Info Version");
        let i32_ty = ctx.i32_type();
        let dwarf_version_md = ctx.metadata_node(&[
            i32_ty.const_int(7, false).into(),
            dwarf_version.into(),
            i32_ty.const_int(5, false).into(),
        ]);
        let debug_version_md = ctx.metadata_node(&[
            i32_ty.const_int(7, false).into(),
            debug_info_version.into(),
            i32_ty.const_int(LLVM_DEBUG_METADATA_VERSION, false).into(),
        ]);
        module
            .add_global_metadata("llvm.module.flags", &dwarf_version_md)
            .unwrap();
        module
            .add_global_metadata("llvm.module.flags", &debug_version_md)
            .unwrap();
        let cwd = cwd.to_string_lossy();
        let filename = db
            .path(FileId::default())
            .unwrap_or(format!("<file {}>", 0));
        let (builder, _) = module.create_debug_info_builder(
            true,
            inkwell::debug_info::DWARFSourceLanguage::C,
            &filename,
            &cwd,
            "yaboc",
            true,
            "",
            0,
            "",
            inkwell::debug_info::DWARFEmissionKind::Full,
            0,
            false,
            false,
            "",
            "",
        );
        Ok(DebugBuilder {
            builder,
            type_memo: Default::default(),
            files: Default::default(),
            cwd: cwd.to_string(),
        })
    }
    fn file(&mut self, file: FileId, db: &dyn Layouts) -> DIFile<'llvm> {
        match self.files.entry(file) {
            Entry::Occupied(occupied_entry) => return *occupied_entry.get(),
            Entry::Vacant(vacant_entry) => {
                let filename = db
                    .path(file)
                    .unwrap_or(format!("<file {}>", file.to_usize()));
                *vacant_entry.insert(self.builder.create_file(&filename, &self.cwd))
            }
        }
    }

    fn default_file(&mut self, db: &dyn Layouts) -> DIFile<'llvm> {
        self.file(FileId::default(), db)
    }

    pub fn default_loc(&mut self, db: &dyn Layouts) -> DebugLocation<'llvm> {
        DebugLocation {
            line: 0,
            col: 0,
            file: self.default_file(db),
        }
    }

    pub fn location_data(
        &mut self,
        layout: IMonoLayout<'comp>,
        db: &dyn Layouts,
        ctx: &'llvm Context,
    ) -> Option<DebugLocation<'llvm>> {
        let def = match layout.mono_layout() {
            MonoLayout::IfParser(_, cid) => {
                let constraint = db.lookup_intern_hir_constraint(*cid);
                return Some(self.span_info(db, constraint.span));
            }
            MonoLayout::Array { parser: inner, .. }
            | MonoLayout::ArrayParser(Some((inner, _)))
            | MonoLayout::ArrayFillParser(Some((inner, _))) => {
                return self.multi_location_data(*inner, db, ctx);
            }
            MonoLayout::Nominal(parser_def_id, ..)
            | MonoLayout::NominalParser(parser_def_id, ..) => parser_def_id.0,
            MonoLayout::Block(block_id, _) | MonoLayout::BlockParser(block_id, _) => block_id.0,
            MonoLayout::Lambda(lambda_id, ..) => lambda_id.0,
            MonoLayout::Ptr
            | MonoLayout::SlicePtr
            | MonoLayout::Primitive(_)
            | MonoLayout::Range
            | MonoLayout::Single
            | MonoLayout::Regex(_)
            | MonoLayout::ArrayParser(None)
            | MonoLayout::ArrayFillParser(None) => return None,
        };
        Some(self.span_info(db, IndirectSpan::default_span(def)))
    }

    pub fn location_data_or_default(
        &mut self,
        layout: IMonoLayout<'comp>,
        db: &dyn Layouts,
        ctx: &'llvm Context,
    ) -> DebugLocation<'llvm> {
        self.location_data(layout, db, ctx)
            .unwrap_or_else(|| self.default_loc(db))
    }

    fn multi_location_data(
        &mut self,
        layout: ILayout<'comp>,
        db: &dyn Layouts,
        ctx: &'llvm Context,
    ) -> Option<DebugLocation<'llvm>> {
        for mono in &layout {
            if let Some(loc) = self.location_data(mono, db, ctx) {
                return Some(loc);
            }
        }
        return None;
    }

    fn span_info(&mut self, db: &dyn Layouts, span: IndirectSpan) -> DebugLocation<'llvm> {
        let span = db.indirect_span(span).unwrap();
        let file = self.file(span.file, db);
        let (start, _) = db.file_offset_pos(span.file, (span.lo as usize, span.hi as usize));
        DebugLocation {
            line: (start.line + 1) as u32,
            col: (start.column + 1) as u32,
            file,
        }
    }

    pub fn location(
        &mut self,
        layout: IMonoLayout<'comp>,
        db: &dyn Layouts,
        ctx: &'llvm Context,
        scope: DIScope<'llvm>,
    ) -> SResult<DILocation<'llvm>> {
        let DebugLocation { line, col, .. } = self.location_data_or_default(layout, db, ctx);
        let loc = self
            .builder
            .create_debug_location(ctx, line, col, scope, None);
        Ok(loc)
    }

    fn ptr_type(&mut self, target_info: &TargetLayoutData) -> InkResult<DIType<'llvm>> {
        let byte =
            self.builder
                .create_basic_type("byte", 8, DwarfEncoding::UnsignedChar as DwEnc, 0)?;
        Ok(self
            .builder
            .create_pointer_type(
                "ptr",
                byte.as_type(),
                target_info.pointer_sa.total_size() * 8,
                (target_info.pointer_sa.align() * 8) as u32,
                AddressSpace::default(),
            )
            .as_type())
    }

    pub fn primitive_type(
        &mut self,
        target_info: &TargetLayoutData,
        primitve: PrimitiveType,
    ) -> InkResult<DIBasicType<'llvm>> {
        match primitve {
            PrimitiveType::Bit => self.builder.create_basic_type(
                "bit",
                target_info.bit_sa.after_bits(),
                DwarfEncoding::Boolean as DwEnc,
                0,
            ),
            PrimitiveType::Int => self.builder.create_basic_type(
                "int",
                target_info.int_sa.after_bits(),
                DwarfEncoding::Signed as DwEnc,
                0,
            ),

            PrimitiveType::Char => self.builder.create_basic_type(
                "char",
                target_info.char_sa.after_bits(),
                DwarfEncoding::Ucs as DwEnc,
                0,
            ),
            PrimitiveType::Unit => self.builder.create_basic_type("unit", 0, 0, 0),
        }
    }

    fn builtin_struct<const N: usize>(
        &mut self,
        layout: IMonoLayout<'comp>,
        db: &dyn Layouts,
        llvm_ctx: &'llvm Context,
        layout_ctx: &mut yaboc_absint::AbsIntCtx<'comp, ILayout<'comp>>,
        inner: [ILayout<'comp>; N],
        field_names: [&str; N],
        name: &str,
    ) -> InkResult<DIType<'llvm>> {
        let inner_sa = inner.map(|x| x.size_align(layout_ctx).unwrap());
        let offset = SizeAlign::offsets_bits(inner_sa);
        let self_sa = layout.inner().size_align(layout_ctx).unwrap();
        let mut inner_members = Vec::with_capacity(N);
        let DebugLocation { line, file, .. } = self.location_data_or_default(layout, db, llvm_ctx);
        for i in 0..N {
            let inner_ty = self.debug_type_no_vtable(inner[i], db, llvm_ctx, layout_ctx)?;
            let inner_member = self.builder.create_member_type(
                file.as_debug_info_scope(),
                field_names[i],
                file,
                line,
                inner_sa[i].after_bits(),
                inner_sa[i].align_bits(),
                offset[i],
                0,
                inner_ty,
            );
            inner_members.push(inner_member.as_type());
        }
        let unique = layout.mangled_name(layout_ctx, db);
        let struct_ty = self.builder.create_struct_type(
            file.as_debug_info_scope(),
            name,
            file,
            line,
            self_sa.after_bits(),
            self_sa.align_bits(),
            0,
            None,
            &inner_members,
            YABO_LANG_ID,
            None,
            &unique,
        );
        Ok(struct_ty.as_type())
    }

    fn manifestation_type(
        &mut self,
        layout: IMonoLayout<'comp>,
        db: &dyn Layouts,
        llvm_ctx: &'llvm Context,
        layout_ctx: &mut yaboc_absint::AbsIntCtx<'comp, ILayout<'comp>>,
        name: &str,
        field_layout: impl Fn(DefId) -> ILayout<'comp>,
    ) -> InkResult<DIType<'llvm>> {
        let manifestation = layout_ctx.dcx.manifestation(layout.inner());
        let mut fields = vec![];
        for (field, offset) in manifestation.field_offsets.iter() {
            let field = field_layout(*field);
            let bit_offset = offset * 8;
            let field_type = self.debug_type_no_vtable(field, db, llvm_ctx, layout_ctx)?;
            let field_name = dbformat!(db, "{}", &field);
            let DebugLocation {
                line: field_line,
                col: _,
                file: field_file,
            } = self
                .multi_location_data(field, db, llvm_ctx)
                .unwrap_or_else(|| self.default_loc(db));
            let member_type = self.builder.create_member_type(
                field_file.as_debug_info_scope(),
                &field_name,
                field_file,
                field_line,
                field_type.get_size_in_bits(),
                field_type.get_align_in_bits(),
                bit_offset,
                0,
                field_type,
            );
            fields.push(member_type.as_type());
        }
        let unique = layout.mangled_name(layout_ctx, db);
        let layout_sa = layout.inner().size_align(layout_ctx).unwrap();
        let DebugLocation { line, file, .. } = self.location_data_or_default(layout, db, llvm_ctx);
        let struct_ty = self.builder.create_struct_type(
            file.as_debug_info_scope(),
            name,
            file,
            line,
            layout_sa.after_bits(),
            layout_sa.align_bits(),
            0,
            None,
            &fields,
            YABO_LANG_ID,
            None,
            &unique,
        );
        Ok(struct_ty.as_type())
    }

    fn mono_debug_type(
        &mut self,
        layout: IMonoLayout<'comp>,
        db: &dyn Layouts,
        llvm_ctx: &'llvm Context,
        layout_ctx: &mut AbsLayoutCtx<'comp>,
    ) -> InkResult<DIType<'llvm>> {
        let target_info = layout_ctx.dcx.target_data();
        let name = dbformat!(db, "{}", &layout);
        match layout.mono_layout() {
            MonoLayout::Primitive(primitive_type) => {
                let primitive = self.primitive_type(target_info, *primitive_type)?;
                Ok(primitive.as_type())
            }
            MonoLayout::Ptr => self.ptr_type(target_info),
            MonoLayout::SlicePtr => {
                let ptr = self.ptr_type(target_info)?;
                let arr = self.builder.create_array_type(
                    ptr,
                    2 * ptr.get_size_in_bits(),
                    ptr.get_align_in_bits(),
                    &[0..2],
                );
                Ok(arr.as_type())
            }
            MonoLayout::Range => {
                let int = self
                    .primitive_type(target_info, PrimitiveType::Int)?
                    .as_type();
                let arr = self.builder.create_array_type(
                    int,
                    2 * int.get_size_in_bits(),
                    int.get_align_in_bits(),
                    &[0..2],
                );
                Ok(arr.as_type())
            }
            MonoLayout::Single => {
                let single = self.builder.create_basic_type(&name, 0, 0, 0)?;
                Ok(single.as_type())
            }
            MonoLayout::Regex(_) => {
                let regex_ty = self.builder.create_basic_type(&name, 0, 0, 0)?;
                Ok(regex_ty.as_type())
            }
            MonoLayout::IfParser(inner, _) => self.builtin_struct(
                layout,
                db,
                llvm_ctx,
                layout_ctx,
                [*inner],
                ["parser"],
                &name,
            ),
            MonoLayout::ArrayParser(None) => {
                let array = self.builder.create_basic_type(&name, 0, 0, 0)?;
                Ok(array.as_type())
            }
            MonoLayout::ArrayParser(Some((parser, None))) => self.builtin_struct(
                layout,
                db,
                llvm_ctx,
                layout_ctx,
                [*parser],
                ["parser"],
                &name,
            ),
            MonoLayout::ArrayParser(Some((parser, Some((length, _))))) => self.builtin_struct(
                layout,
                db,
                llvm_ctx,
                layout_ctx,
                [*parser, *length],
                ["parser", "length"],
                &name,
            ),
            MonoLayout::ArrayFillParser(None) => {
                let array_fill = self.builder.create_basic_type(&name, 0, 0, 0)?;
                Ok(array_fill.as_type())
            }
            MonoLayout::ArrayFillParser(Some((parser, _))) => self.builtin_struct(
                layout,
                db,
                llvm_ctx,
                layout_ctx,
                [*parser],
                ["parser"],
                &name,
            ),
            MonoLayout::Array { parser, slice } => self.builtin_struct(
                layout,
                db,
                llvm_ctx,
                layout_ctx,
                [*parser, *slice],
                ["parser", "slice"],
                &name,
            ),

            MonoLayout::Nominal(pd, from, args) => {
                self.manifestation_type(layout, db, llvm_ctx, layout_ctx, &name, |def| {
                    let Some(idx) = db.parserdef_arg_index(*pd, def).ok().flatten() else {
                        return from.unwrap();
                    };
                    args[idx]
                })
            }
            MonoLayout::NominalParser(pd, args, _) => {
                self.manifestation_type(layout, db, llvm_ctx, layout_ctx, &name, |def| {
                    let Some(idx) = db.parserdef_arg_index(*pd, def).ok().flatten() else {
                        dbpanic!(db, "unexpected lambda def: {}", &def);
                    };
                    args[idx]
                })
            }
            MonoLayout::Block(_, btree_map) => {
                self.manifestation_type(layout, db, llvm_ctx, layout_ctx, &name, |def| {
                    let yaboc_base::interner::PathComponent::Named(field_name) =
                        def.unwrap_path_end(db)
                    else {
                        dbpanic!(db, "unexpected block field {}", &def)
                    };
                    btree_map[&field_name]
                })
            }
            MonoLayout::BlockParser(_, btree_map) => {
                self.manifestation_type(layout, db, llvm_ctx, layout_ctx, &name, |def| {
                    btree_map[&def]
                })
            }
            MonoLayout::Lambda(lambda_id, btree_map, args) => {
                self.manifestation_type(layout, db, llvm_ctx, layout_ctx, &name, |def| {
                    if let Some(inner) = btree_map.get(&def) {
                        return *inner;
                    }
                    let Some(idx) = db.lambda_arg_index(*lambda_id, def).ok().flatten() else {
                        dbpanic!(db, "unexpected lambda def: {}", &def);
                    };
                    args[idx]
                })
            }
        }
    }

    fn debug_type_no_vtable(
        &mut self,
        layout: ILayout<'comp>,
        db: &dyn Layouts,
        llvm_ctx: &'llvm Context,
        layout_ctx: &mut AbsLayoutCtx<'comp>,
    ) -> InkResult<DIType<'llvm>> {
        if let Some(ty) = self.type_memo.get(&layout) {
            return Ok(*ty);
        }
        let res = match &layout.layout.1 {
            yaboc_layout::Layout::None => {
                self.builder.create_basic_type("none", 0, 0, 0)?.as_type()
            }
            yaboc_layout::Layout::Mono(_) => {
                self.mono_debug_type(layout.maybe_mono().unwrap(), db, llvm_ctx, layout_ctx)?
            }
            yaboc_layout::Layout::Multi(_) => {
                let name = dbformat!(db, "{}", &layout);
                let default_file = self.default_file(db);
                let sa = layout.size_align_without_vtable(layout_ctx).unwrap();
                let mut subtypes = vec![];
                for mono in &layout {
                    subtypes.push(self.mono_debug_type(mono, db, llvm_ctx, layout_ctx)?);
                }
                let unique = format!(
                    "multi_{}",
                    truncated_hex(&layout_ctx.dcx.layout_hash(db, layout))
                );
                let aggregate = self.builder.create_union_type(
                    default_file.as_debug_info_scope(),
                    &name,
                    default_file,
                    0,
                    sa.after_bits(),
                    sa.align_bits(),
                    0,
                    &subtypes,
                    YABO_LANG_ID,
                    &unique,
                );
                aggregate.as_type()
            }
        };
        self.type_memo.insert(layout, res);
        Ok(res)
    }

    pub fn debug_type(
        &mut self,
        layout: ILayout<'comp>,
        db: &dyn Layouts,
        llvm_ctx: &'llvm Context,
        layout_ctx: &mut AbsLayoutCtx<'comp>,
    ) -> InkResult<DIType<'llvm>> {
        if !layout.is_multi() {
            return self.debug_type_no_vtable(layout, db, llvm_ctx, layout_ctx);
        }

        let name = dbformat!(db, "alloc of {}", &layout);
        let default_file = self.default_file(db);
        let sa = layout.size_align(layout_ctx).unwrap();
        let center_offset = sa.allocation_center_bits();
        let ptr = self.ptr_type(layout_ctx.dcx.target_data())?;
        let ptr = self.builder.create_member_type(
            default_file.as_debug_info_scope(),
            "vtable",
            default_file,
            0,
            ptr.get_size_in_bits(),
            ptr.get_align_in_bits(),
            center_offset - ptr.get_size_in_bits(),
            0,
            ptr,
        );
        let val = self.debug_type_no_vtable(layout, db, llvm_ctx, layout_ctx)?;
        let val = self.builder.create_member_type(
            default_file.as_debug_info_scope(),
            "value",
            default_file,
            0,
            val.get_size_in_bits(),
            val.get_align_in_bits(),
            center_offset,
            0,
            val,
        );
        let unique = format!(
            "multi_alloc_{}",
            truncated_hex(&layout_ctx.dcx.layout_hash(db, layout))
        );
        let aggregate = self.builder.create_struct_type(
            default_file.as_debug_info_scope(),
            &name,
            default_file,
            0,
            sa.total_size() * 8,
            sa.align_bits(),
            0,
            None,
            &[ptr.as_type(), val.as_type()],
            YABO_LANG_ID,
            None,
            &unique,
        );
        Ok(aggregate.as_type())
    }
}

trait DebugType {
    fn debug_ty<'llvm, 'comp>(ctx: &mut CodeGenCtx<'llvm, 'comp>) -> InkResult<DIType<'llvm>>;
}

impl DebugType for u8 {
    fn debug_ty<'llvm, 'comp>(ctx: &mut CodeGenCtx<'llvm, 'comp>) -> InkResult<DIType<'llvm>> {
        Ok(ctx
            .debug
            .builder
            .create_basic_type("byte", 8, DwarfEncoding::UnsignedChar as DwEnc, 0)?
            .as_type())
    }
}

impl DebugType for i64 {
    fn debug_ty<'llvm, 'comp>(ctx: &mut CodeGenCtx<'llvm, 'comp>) -> InkResult<DIType<'llvm>> {
        Ok(ctx
            .debug
            .primitive_type(ctx.layouts.dcx.target_data(), PrimitiveType::Int)?
            .as_type())
    }
}

impl DebugType for u64 {
    fn debug_ty<'llvm, 'comp>(ctx: &mut CodeGenCtx<'llvm, 'comp>) -> InkResult<DIType<'llvm>> {
        Ok(ctx
            .debug
            .builder
            .create_basic_type("u64", 64, DwarfEncoding::Unsigned as DwEnc, 0)?
            .as_type())
    }
}

impl DebugType for usize {
    fn debug_ty<'llvm, 'comp>(ctx: &mut CodeGenCtx<'llvm, 'comp>) -> InkResult<DIType<'llvm>> {
        let size = ctx.layouts.dcx.target_data().pointer_sa.after_bits();
        Ok(ctx
            .debug
            .builder
            .create_basic_type("usize", size, DwarfEncoding::Unsigned as DwEnc, 0)?
            .as_type())
    }
}

impl<T: DebugType> DebugType for *const T {
    fn debug_ty<'llvm, 'comp>(ctx: &mut CodeGenCtx<'llvm, 'comp>) -> InkResult<DIType<'llvm>> {
        let inner = T::debug_ty(ctx)?;
        let target_info = ctx.layouts.dcx.target_data();
        Ok(ctx
            .debug
            .builder
            .create_pointer_type(
                "const ptr",
                inner,
                target_info.pointer_sa.after_bits(),
                target_info.pointer_sa.align_bits(),
                AddressSpace::default(),
            )
            .as_type())
    }
}

impl<T: DebugType> DebugType for *mut T {
    fn debug_ty<'llvm, 'comp>(ctx: &mut CodeGenCtx<'llvm, 'comp>) -> InkResult<DIType<'llvm>> {
        let inner = T::debug_ty(ctx)?;
        let target_info = ctx.layouts.dcx.target_data();
        Ok(ctx
            .debug
            .builder
            .create_pointer_type(
                "mut ptr",
                inner,
                target_info.pointer_sa.after_bits(),
                target_info.pointer_sa.align_bits(),
                AddressSpace::default(),
            )
            .as_type())
    }
}

pub struct RoutineType<'llvm, const ARITY: usize> {
    pub args: [DIType<'llvm>; ARITY],
    pub ret: DIType<'llvm>,
}

impl<'llvm, const ARITY: usize> RoutineType<'llvm, ARITY> {
    pub fn as_subprogram_type<'comp>(
        &self,
        ctx: &mut CodeGenCtx<'llvm, 'comp>,
        file: DIFile<'llvm>,
    ) -> InkResult<DISubroutineType<'llvm>> {
        let res = ctx.debug.builder.create_subroutine_type(
            file,
            Some(self.ret),
            &self.args,
            LLVMDIFlagPrototyped,
        );
        Ok(res)
    }
}

pub trait SubroutineDebugType<const ARITY: usize> {
    fn subroutine_ty<'llvm, 'comp>(
        ctx: &mut CodeGenCtx<'llvm, 'comp>,
    ) -> InkResult<RoutineType<'llvm, ARITY>>;
}

macro_rules! subroutine_impl {
    (fn($($n:ident: $t:ident),*) -> $ret:ident, $arity:expr) => {
        impl<$ret: DebugType, $($t: DebugType),*> SubroutineDebugType<$arity> for fn($($n: $t),*) -> $ret {
            fn subroutine_ty<'llvm, 'comp>(
                ctx: &mut CodeGenCtx<'llvm, 'comp>,
            ) -> InkResult<RoutineType<'llvm, $arity>> {
                let ret = $ret::debug_ty(ctx)?;
                let args = [
                    $($t::debug_ty(ctx)?),*
                ];
                Ok(RoutineType {
                    args,
                    ret,
                })
            }
        }
    };
}

subroutine_impl!(fn(a: A) -> R, 1);
subroutine_impl!(fn(a: A, b: B) -> R, 2);
subroutine_impl!(fn(a: A, b: B, c: C) -> R, 3);
subroutine_impl!(fn(a: A, b: B, c: C, d: D) -> R, 4);
