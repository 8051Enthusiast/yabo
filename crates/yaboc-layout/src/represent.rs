use fxhash::FxHashMap;
use sha2::Digest;
use std::{collections::BTreeMap, fmt::Write};
use yaboc_req::{NeededBy, RequirementSet};

use yaboc_base::{
    databased_display::DatabasedDisplay,
    dbformat, dbwrite,
    hash::StableHash,
    interner::{DefId, FieldName, Identifier, RegexKind},
};
use yaboc_target::layout::PSize;

use crate::ILayout;

use super::{IMonoLayout, Layout, Layouts, MonoLayout};
use yaboc_absint::AbsInt;

impl<DB: AbsInt + ?Sized> DatabasedDisplay<DB> for ILayout<'_> {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match &self.layout.1 {
            Layout::None => write!(f, "<null>"),
            Layout::Mono(d, ty) => match d {
                MonoLayout::Primitive(p) => p.db_fmt(f, db),
                MonoLayout::SlicePtr => write!(f, "sliceptr"),
                MonoLayout::Range => write!(f, "range"),
                MonoLayout::Single => write!(f, "single"),
                MonoLayout::Nominal(_, from, args) => {
                    dbwrite!(f, db, "nominal[{}](", ty)?;
                    if let Some((inner_layout, _)) = from {
                        dbwrite!(f, db, "from: {}", inner_layout)?;
                    }
                    for (i, (_, arg)) in args.iter().enumerate() {
                        if i > 0 || from.is_some() {
                            write!(f, ", ")?;
                        }
                        dbwrite!(f, db, "{}", arg)?;
                    }
                    write!(f, ")")
                }
                MonoLayout::NominalParser(_, args, backtracks) => {
                    write!(f, "nominal-parser")?;
                    if *backtracks {
                        write!(f, "?")?;
                    }
                    dbwrite!(f, db, "[{}](", ty)?;
                    for (i, (layout, ty)) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        dbwrite!(f, db, "{}: {}", layout, ty)?;
                    }
                    write!(f, ")")
                }
                MonoLayout::Block(_, vars) => {
                    dbwrite!(f, db, "block[{}]{{", ty)?;
                    for (i, (var, layout)) in vars.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        dbwrite!(f, db, "{}: {}", var, layout)?;
                    }
                    write!(f, "}}")
                }
                MonoLayout::BlockParser(bd, captures, tysubs, bt) => {
                    write!(f, "block-parser")?;
                    if *bt {
                        write!(f, "?")?;
                    }
                    dbwrite!(f, db, "[{}, {}, [", &bd.0, ty)?;
                    for (i, ty) in tysubs.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        dbwrite!(f, db, "{}", ty)?;
                    }
                    write!(f, "]]{{")?;
                    for (i, (capture, layout)) in captures.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        dbwrite!(f, db, "{}: {}", capture, layout)?;
                    }
                    write!(f, "}}")
                }
                MonoLayout::Lambda(ld, captures, args, tysubs, bt) => {
                    write!(f, "lambda")?;
                    if *bt {
                        write!(f, "?")?;
                    }
                    dbwrite!(f, db, "[{}, [", &ld.0)?;
                    for (i, ty) in tysubs.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        dbwrite!(f, db, "{}", ty)?;
                    }
                    write!(f, "]]{{")?;
                    for (i, (capture, layout)) in captures.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        dbwrite!(f, db, "{}: {}", capture, layout)?;
                    }
                    write!(f, "}}(")?;
                    for (i, (layout, ty)) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        dbwrite!(f, db, "{}: {}", layout, ty)?;
                    }
                    write!(f, ")")
                }
                MonoLayout::Regex(regex, bt) => {
                    regex.db_fmt(f, db)?;
                    if *bt {
                        write!(f, "?")?;
                    }
                    Ok(())
                }
                MonoLayout::IfParser(inner, referenced, wiggle) => {
                    dbwrite!(
                        f,
                        db,
                        "if-parser-{}[{}]({}, {})",
                        wiggle,
                        ty,
                        inner,
                        referenced
                    )?;
                    Ok(())
                }
                MonoLayout::ArrayParser(Some((parser, inner))) => {
                    if let Some(inner) = inner {
                        dbwrite!(f, db, "array-parser[{}]({}, {})", ty, parser, inner)
                    } else {
                        dbwrite!(f, db, "array-parser[{}]({})", ty, parser)
                    }
                }
                MonoLayout::ArrayParser(None) => dbwrite!(f, db, "array-parser[{}]()", ty),
                MonoLayout::ArrayFillParser(Some(inner)) => {
                    dbwrite!(f, db, "array-fill-parser[{}]({})", ty, inner)
                }
                MonoLayout::ArrayFillParser(None) => dbwrite!(f, db, "array-fill-parser[{}]()", ty),
                MonoLayout::Array { parser, slice } => {
                    dbwrite!(f, db, "array[{}]({}, {})", ty, parser, slice)
                }
            },
            Layout::Multi(subs) => {
                for (i, layout) in subs.layouts.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    dbwrite!(f, db, "{}", layout)?;
                }
                Ok(())
            }
        }
    }
}

impl<DB: AbsInt + ?Sized> DatabasedDisplay<DB> for IMonoLayout<'_> {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        self.0.db_fmt(f, db)
    }
}

#[derive(Default)]
pub struct LayoutHasher<'a> {
    map: FxHashMap<ILayout<'a>, [u8; 32]>,
}

impl<'a> LayoutHasher<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn hash<DB: Layouts + ?Sized>(&mut self, layout: ILayout<'a>, db: &DB) -> [u8; 32] {
        if let Some(x) = self.map.get(&layout) {
            return *x;
        }
        let mut hasher = Default::default();
        self.hash_layout(&mut hasher, layout, db);
        let res = hasher.finalize().into();
        self.map.insert(layout, res);
        res
    }
    fn hash_layout<DB: Layouts + ?Sized>(
        &mut self,
        state: &mut sha2::Sha256,
        layout: ILayout<'a>,
        db: &DB,
    ) {
        match &layout.layout.1 {
            Layout::None => state.update([0]),
            Layout::Mono(mono, ty) => {
                state.update([1]);
                state.update(db.type_hash(*ty));
                self.hash_mono(state, mono, db);
            }
            Layout::Multi(ml) => {
                state.update([2]);
                ml.layouts.len().update_hash(state, db);
                for layout in &layout {
                    state.update(db.type_hash(layout.mono_layout().1));
                    self.hash_mono(state, layout.mono_layout().0, db);
                }
            }
        }
    }
    fn hash_captures<DB: Layouts + ?Sized>(
        &mut self,
        state: &mut sha2::Sha256,
        captures: &BTreeMap<DefId, ILayout<'a>>,
        db: &DB,
    ) {
        let mut capture_hashes: Vec<_> = captures
            .iter()
            .map(|(id, layout)| {
                let named = db.def_hash(*id);
                let layout_hash = self.hash(*layout, db);
                (named, layout_hash)
            })
            .collect();
        capture_hashes.sort_unstable_by_key(|x| x.0);
        capture_hashes.len().update_hash(state, db);
        for (id_hash, hash) in capture_hashes.iter() {
            state.update(id_hash);
            state.update(hash);
        }
    }
    fn hash_mono<DB: Layouts + ?Sized>(
        &mut self,
        state: &mut sha2::Sha256,
        mono: &MonoLayout<ILayout<'a>>,
        db: &DB,
    ) {
        match &mono {
            MonoLayout::Primitive(p) => {
                state.update([0]);
                p.update_hash(state, db);
            }
            MonoLayout::SlicePtr => {
                state.update([1]);
            }
            MonoLayout::Single => {
                state.update([2]);
            }
            MonoLayout::Nominal(def, from, args) => {
                state.update([3]);
                def.0.update_hash(state, db);
                if let Some((from, ty)) = from {
                    state.update([1]);
                    let from_hash = self.hash(*from, db);
                    state.update(from_hash);
                    state.update(db.type_hash(*ty));
                } else {
                    state.update([0]);
                }
                args.len().update_hash(state, db);
                for (layout, ty) in args.iter() {
                    state.update(self.hash(*layout, db));
                    state.update(db.type_hash(*ty));
                }
            }
            MonoLayout::NominalParser(def, args, bt) => {
                state.update([4]);
                def.0.update_hash(state, db);
                args.len().update_hash(state, db);
                for (layout, ty) in args.iter() {
                    state.update(self.hash(*layout, db));
                    state.update(db.type_hash(*ty));
                }
                state.update([*bt as u8]);
            }
            MonoLayout::Block(def, map) => {
                state.update([5]);
                def.0.update_hash(state, db);
                let mut field_hashes: Vec<_> = map
                    .iter()
                    .map(|(field, layout)| {
                        let named = match field {
                            FieldName::Return => None,
                            FieldName::Ident(ident) => {
                                Some(db.lookup_intern_identifier(*ident).name)
                            }
                        };
                        let layout_hash = self.hash(*layout, db);
                        (named, layout_hash)
                    })
                    .collect();
                field_hashes.sort_unstable_by(|a, b| a.0.cmp(&b.0));
                field_hashes.len().update_hash(state, db);
                for (field_name, hash) in field_hashes.iter() {
                    field_name.update_hash(state, db);
                    state.update(hash);
                }
            }
            MonoLayout::BlockParser(def, map, tysubs, bt) => {
                state.update([6]);
                def.0.update_hash(state, db);
                self.hash_captures(state, map, db);
                tysubs.len().update_hash(state, db);
                for ty in tysubs.iter() {
                    state.update(db.type_hash(*ty));
                }
                state.update([*bt as u8]);
            }
            MonoLayout::Lambda(def, map, args, tysubs, bt) => {
                state.update([7]);
                def.0.update_hash(state, db);
                self.hash_captures(state, map, db);
                tysubs.len().update_hash(state, db);
                for ty in tysubs.iter() {
                    state.update(db.type_hash(*ty));
                }
                args.len().update_hash(state, db);
                for (layout, ty) in args.iter() {
                    state.update(self.hash(*layout, db));
                    state.update(db.type_hash(*ty));
                }
                state.update([*bt as u8]);
            }
            MonoLayout::Range => {
                state.update([9]);
            }
            MonoLayout::Regex(regex, bt) => {
                state.update([10]);
                let regex_data = db.lookup_intern_regex(*regex);
                match regex_data.kind {
                    RegexKind::Regular => {
                        state.update([0]);
                    }
                    RegexKind::Hexagex => {
                        state.update([1]);
                    }
                }
                regex_data.regex.update_hash(state, db);
                state.update([*bt as u8]);
            }
            MonoLayout::IfParser(inner, id, wiggle) => {
                state.update([11]);
                state.update(self.hash(*inner, db));
                // TODO(8051): ideally, we'd hash the content here in a way that
                // is reproducible, but for now we just hash the id.
                id.as_u32().update_hash(state, db);
                (*wiggle as u8).update_hash(state, db);
            }
            MonoLayout::ArrayParser(Some((parser, inner))) => {
                state.update([12, 1]);
                state.update(self.hash(*parser, db));
                if let Some(inner) = inner {
                    state.update([1]);
                    state.update(self.hash(*inner, db));
                } else {
                    state.update([0]);
                }
            }
            MonoLayout::ArrayParser(None) => {
                state.update([12, 0]);
            }
            MonoLayout::Array { parser, slice } => {
                state.update([13]);
                state.update(self.hash(*parser, db));
                state.update(self.hash(*slice, db));
            }
            MonoLayout::ArrayFillParser(Some(inner)) => {
                state.update([13, 1]);
                state.update(self.hash(*inner, db));
            }
            MonoLayout::ArrayFillParser(None) => {
                state.update([13, 0]);
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ParserFunKind {
    Wrapper,
    TailWrapper,
    Worker,
}

#[derive(Clone, Copy)]
pub enum LayoutPart {
    Parse(RequirementSet, ParserFunKind, [u8; TRUNCATION_LENGTH]),
    Field(Identifier),
    VTable,
    VTableTy,
    Start,
    End,
    Typecast,
    SingleForward,
    CurrentElement,
    ArrayLen,
    Skip,
    Span,
    InnerArray,
    CreateArgs(PSize),
    SetArg(PSize),
    Len,
    Mask,
    EvalFun(ParserFunKind),
}

impl<DB: Layouts + ?Sized> DatabasedDisplay<DB> for LayoutPart {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            LayoutPart::Parse(reqs, kind, from) => {
                write!(f, "parse_{}_", &truncated_hex(&from[..]))?;
                if reqs.contains(NeededBy::Val) {
                    write!(f, "v")?;
                }
                if reqs.contains(NeededBy::Len) {
                    write!(f, "l")?;
                }
                if reqs.contains(NeededBy::Backtrack) {
                    write!(f, "b")?;
                }
                match kind {
                    ParserFunKind::Wrapper => {}
                    ParserFunKind::TailWrapper => write!(f, "_tail")?,
                    ParserFunKind::Worker => write!(f, "_worker")?,
                }
                Ok(())
            }
            LayoutPart::Field(n) => dbwrite!(f, db, "field_{}", n),
            LayoutPart::VTable => write!(f, "vtable"),
            LayoutPart::VTableTy => write!(f, "vtable_ty"),
            LayoutPart::Start => write!(f, "start"),
            LayoutPart::End => write!(f, "end"),
            LayoutPart::Typecast => write!(f, "typecast"),
            LayoutPart::SingleForward => write!(f, "single_forward"),
            LayoutPart::CurrentElement => write!(f, "current_element"),
            LayoutPart::ArrayLen => write!(f, "array_len"),
            LayoutPart::Skip => write!(f, "skip"),
            LayoutPart::Span => write!(f, "span"),
            LayoutPart::InnerArray => write!(f, "inner_array"),
            LayoutPart::CreateArgs(p) => write!(f, "create_args_{p}"),
            LayoutPart::SetArg(idx) => write!(f, "set_arg_{idx}"),
            LayoutPart::Len => write!(f, "len"),
            LayoutPart::Mask => write!(f, "mask"),
            LayoutPart::EvalFun(kind) => {
                write!(f, "eval_fun")?;
                match kind {
                    ParserFunKind::Wrapper => {}
                    ParserFunKind::TailWrapper => write!(f, "_tail")?,
                    ParserFunKind::Worker => write!(f, "_worker")?,
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone, Copy)]
pub struct LayoutSymbol<'a> {
    pub layout: IMonoLayout<'a>,
    pub part: LayoutPart,
}

const TRUNCATION_LENGTH: usize = 8;

pub fn truncated_hex(array: &[u8]) -> String {
    let mut ret = String::new();
    for i in &array[0..TRUNCATION_LENGTH] {
        write!(ret, "{i:02x}").unwrap();
    }
    ret
}

impl<'a> LayoutSymbol<'a> {
    pub fn symbol<DB: Layouts + ?Sized>(&self, hasher: &mut LayoutHasher<'a>, db: &DB) -> String {
        let name_prefix = match self.layout.mono_layout().0 {
            MonoLayout::BlockParser(def, _, _, backtracks) => {
                if *backtracks {
                    format!("parse_block_{}_b", &truncated_hex(&db.def_hash(def.0)))
                } else {
                    format!("parse_block_{}", &truncated_hex(&db.def_hash(def.0)))
                }
            }
            MonoLayout::Block(def, _) => {
                format!("block_{}", &truncated_hex(&db.def_hash(def.0)))
            }
            MonoLayout::Nominal(id, _, _) => {
                dbformat!(db, "{}", &db.def_name(id.0).unwrap())
            }
            MonoLayout::NominalParser(id, _, backtracks) => {
                if *backtracks {
                    dbformat!(db, "parse_{}_b", &db.def_name(id.0).unwrap())
                } else {
                    dbformat!(db, "parse_{}", &db.def_name(id.0).unwrap())
                }
            }
            MonoLayout::Lambda(id, .., backtracks) => {
                if *backtracks {
                    dbformat!(db, "lambda_{}_b", &truncated_hex(&db.def_hash(id.0)))
                } else {
                    dbformat!(db, "lambda_{}", &truncated_hex(&db.def_hash(id.0)))
                }
            }
            MonoLayout::Regex(re, backtracks) => {
                let re_str = db.lookup_intern_regex(*re);
                let prefix = match re_str.kind {
                    RegexKind::Regular => "",
                    RegexKind::Hexagex => "h_",
                };
                let ident_str = re_str
                    .regex
                    .replace(|c: char| !c.is_ascii_alphanumeric(), "_");
                if *backtracks {
                    format!("parse_regex_{prefix}{ident_str}_b")
                } else {
                    format!("parse_regex_{prefix}{ident_str}")
                }
            }
            MonoLayout::IfParser(..) => String::from("parser_if"),
            MonoLayout::Array { .. } => String::from("array"),
            MonoLayout::ArrayParser(Some((_, Some(_)))) => String::from("parse_array"),
            MonoLayout::ArrayParser(Some((_, None)) | None) => String::from("fun_parse_array"),
            MonoLayout::ArrayFillParser(Some(_)) => String::from("parse_array_fill"),
            MonoLayout::ArrayFillParser(None) => String::from("fun_parse_array_fill"),
            MonoLayout::Primitive(_)
            | MonoLayout::SlicePtr
            | MonoLayout::Range
            | MonoLayout::Single => {
                dbformat!(db, "{}", &self.layout.0)
            }
        };
        let layout_hex = truncated_hex(&hasher.hash(self.layout.0, db));
        dbformat!(db, "{}${}${}", &name_prefix, &layout_hex, &self.part)
    }
}
