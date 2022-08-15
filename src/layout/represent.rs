use fxhash::FxHashMap;
use sha2::Digest;
use std::fmt::Write;

use crate::{
    databased_display::DatabasedDisplay,
    dbformat, dbwrite,
    hash::StableHash,
    interner::{FieldName, Identifier},
};

use super::{flat_layouts, prop::PSize, ILayout, IMonoLayout, Layout, Layouts, MonoLayout};
use crate::absint::{AbsInt, Arg};

impl<'a, DB: AbsInt + ?Sized> DatabasedDisplay<DB> for ILayout<'a> {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match &self.layout {
            Layout::None => write!(f, "<null>"),
            Layout::Mono(d, ty) => match d {
                MonoLayout::Primitive(p) => p.db_fmt(f, db),
                MonoLayout::Pointer => write!(f, "ptr"),
                MonoLayout::Single => write!(f, "single"),
                MonoLayout::Nominal(_, args) => {
                    dbwrite!(f, db, "nominal[{}](", ty)?;
                    if let Some(inner_layout) = args {
                        dbwrite!(f, db, "from: {}", inner_layout)?;
                    }
                    write!(f, ")")
                }
                MonoLayout::NominalParser(_) => {
                    dbwrite!(f, db, "nominal-parser[{}]", ty)
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
                MonoLayout::BlockParser(_, captures) => {
                    dbwrite!(f, db, "block-parser[{}]{{", ty)?;
                    for (i, (capture, layout)) in captures.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        dbwrite!(f, db, "{}: {}", capture, layout)?;
                    }
                    write!(f, "}}")
                }
                MonoLayout::ComposedParser(left, _, right) => {
                    dbwrite!(f, db, "composed[{}]({} |> {})", ty, left, right)
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

impl<DB: AbsInt + ?Sized> DatabasedDisplay<DB> for Arg {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            Arg::Named(n) => n.db_fmt(f, db),
            Arg::From => write!(f, "<from>"),
        }
    }
}

pub struct LayoutHasher<'a> {
    map: FxHashMap<ILayout<'a>, [u8; 32]>,
}

impl<'a> LayoutHasher<'a> {
    pub fn new() -> Self {
        Self {
            map: Default::default(),
        }
    }

    pub fn hash<DB: Layouts + ?Sized>(&mut self, layout: ILayout<'a>, db: &DB) -> [u8; 32] {
        if let Some(x) = self.map.get(layout) {
            return *x;
        }
        let mut hasher = sha2::Sha256::new();
        self.hash_layout(&mut hasher, layout, db);
        let res = hasher.finalize().try_into().unwrap();
        self.map.insert(layout, res);
        res
    }
    fn hash_layout<DB: Layouts + ?Sized>(
        &mut self,
        state: &mut sha2::Sha256,
        layout: ILayout<'a>,
        db: &DB,
    ) {
        match &layout.layout {
            Layout::None => state.update([0]),
            Layout::Mono(mono, ty) => {
                state.update([1]);
                state.update(db.type_hash(*ty));
                self.hash_mono(state, mono, db);
            }
            Layout::Multi(ml) => {
                state.update([2]);
                ml.layouts.len().update_hash(state, db);
                for layout in flat_layouts(&layout) {
                    state.update(db.type_hash(layout.mono_layout().1));
                    self.hash_mono(state, layout.mono_layout().0, db);
                }
            }
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
            MonoLayout::Pointer => {
                state.update([1]);
            }
            MonoLayout::Single => {
                state.update([2]);
            }
            MonoLayout::Nominal(def, from) => {
                state.update([3]);
                def.0.update_hash(state, db);
                match from {
                    Some(layout) => {
                        state.update([0]);
                        state.update(self.hash(layout, db));
                    }
                    None => {
                        state.update([1]);
                    }
                }
            }
            MonoLayout::NominalParser(def) => {
                state.update([4]);
                def.0.update_hash(state, db);
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
                        let layout_hash = self.hash(layout, db);
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
            MonoLayout::BlockParser(def, map) => {
                state.update([6]);
                def.0.update_hash(state, db);
                let mut capture_hashes: Vec<_> = map
                    .iter()
                    .map(|(id, layout)| {
                        let named = db.def_hash(*id);
                        let layout_hash = self.hash(layout, db);
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
            MonoLayout::ComposedParser(left, inner_ty, right) => {
                state.update([7]);
                state.update(self.hash(left, db));
                state.update(db.type_hash(*inner_ty));
                state.update(self.hash(right, db));
            }
        }
    }
}

#[derive(Clone, Copy)]
pub enum LayoutPart {
    LenImpl(PSize),
    ValImpl(PSize, bool),
    Field(Identifier),
    VTable,
    Start,
    End,
    Typecast,
    Deref,
    SingleForward,
    CurrentElement,
    Skip,
}

impl<DB: Layouts + ?Sized> DatabasedDisplay<DB> for LayoutPart {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            LayoutPart::LenImpl(p) => write!(f, "len_{}", *p),
            LayoutPart::ValImpl(p, true) => write!(f, "val_{}", *p),
            LayoutPart::ValImpl(p, false) => write!(f, "val_impl_{}", *p),
            LayoutPart::Field(n) => dbwrite!(f, db, "field_{}", n),
            LayoutPart::VTable => write!(f, "vtable"),
            LayoutPart::Start => write!(f, "start"),
            LayoutPart::End => write!(f, "end"),
            LayoutPart::Typecast => write!(f, "typecast"),
            LayoutPart::Deref => write!(f, "deref"),
            LayoutPart::SingleForward => write!(f, "single_forward"),
            LayoutPart::CurrentElement => write!(f, "current_element"),
            LayoutPart::Skip => write!(f, "skip"),
            
        }
    }
}

#[derive(Clone, Copy)]
pub struct LayoutSymbol<'a> {
    pub layout: IMonoLayout<'a>,
    pub part: LayoutPart,
}

const TRUNCATION_LENGTH: usize = 8;

impl<'a> LayoutSymbol<'a> {
    pub fn symbol<DB: Layouts + ?Sized>(&self, hasher: &mut LayoutHasher<'a>, db: &DB) -> String {
        let truncated_hex = |array: &[u8]| {
            let mut ret = String::new();
            for i in &array[0..TRUNCATION_LENGTH] {
                write!(ret, "{:02x}", i).unwrap();
            }
            ret
        };
        let name_prefix = match self.layout.mono_layout().0 {
            MonoLayout::BlockParser(def, _) => {
                format!("parse_block_{}", &truncated_hex(&db.def_hash(def.0)))
            }
            MonoLayout::Block(def, _) => {
                format!("block_{}", &truncated_hex(&db.def_hash(def.0)))
            }
            MonoLayout::ComposedParser(_, _, _) => String::from("composed"),
            MonoLayout::Nominal(id, _) => {
                dbformat!(db, "{}", &db.def_name(id.0).unwrap())
            }
            MonoLayout::NominalParser(id) => {
                dbformat!(db, "parse_{}", &db.def_name(id.0).unwrap())
            }
            MonoLayout::Primitive(_) | MonoLayout::Pointer | MonoLayout::Single => {
                dbformat!(db, "{}", &self.layout.0)
            }
        };
        let layout_hex = truncated_hex(&hasher.hash(&self.layout.0, db));
        dbformat!(db, "{}${}${}", &name_prefix, &layout_hex, &self.part)
    }
}
