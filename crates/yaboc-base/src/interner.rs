use salsa::InternId;
use sha2::{Digest, Sha256};

use crate::{
    databased_display::DatabasedDisplay,
    dbwrite,
    hash::{hash_array, StableHash},
    source::FileId,
};

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Identifier(InternId);

impl salsa::InternKey for Identifier {
    fn from_intern_id(v: InternId) -> Self {
        Identifier(v)
    }

    fn as_intern_id(&self) -> InternId {
        self.0
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct IdentifierName {
    pub name: String,
}

impl IdentifierName {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

impl<DB: Interner + ?Sized> DatabasedDisplay<DB> for Identifier {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        write!(f, "{}", db.lookup_intern_identifier(*self).name)
    }
}

impl<DB: Interner + ?Sized> StableHash<DB> for Identifier {
    fn update_hash(&self, state: &mut sha2::Sha256, db: &DB) {
        db.lookup_intern_identifier(*self)
            .name
            .update_hash(state, db)
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TypeVar(InternId);

impl salsa::InternKey for TypeVar {
    fn from_intern_id(v: InternId) -> Self {
        TypeVar(v)
    }

    fn as_intern_id(&self) -> InternId {
        self.0
    }
}

impl<DB: Interner + ?Sized> DatabasedDisplay<DB> for TypeVar {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        dbwrite!(f, db, "{}", &db.lookup_intern_type_var(*self).name)
    }
}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct TypeVarName {
    pub name: String,
}

impl TypeVarName {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum FieldName {
    Return,
    Ident(Identifier),
}

impl FieldName {
    pub fn unwrap_ident(self) -> Identifier {
        match self {
            FieldName::Ident(id) => id,
            _ => panic!("not an identifier"),
        }
    }
}

impl<DB: Interner + ?Sized> StableHash<DB> for FieldName {
    fn update_hash(&self, state: &mut sha2::Sha256, db: &DB) {
        match self {
            FieldName::Return => state.update(&[0]),
            FieldName::Ident(id) => {
                state.update(&[1]);
                id.update_hash(state, db)
            }
        }
    }
}

impl From<Identifier> for FieldName {
    fn from(ident: Identifier) -> Self {
        FieldName::Ident(ident)
    }
}

impl<DB: Interner + ?Sized> DatabasedDisplay<DB> for FieldName {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            FieldName::Ident(ident) => ident.db_fmt(f, db),
            FieldName::Return => write!(f, "return"),
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum PathComponent {
    Named(FieldName),
    Unnamed(u32),
    File(FileId),
}

impl<DB: Interner + ?Sized> StableHash<DB> for PathComponent {
    fn update_hash(&self, state: &mut sha2::Sha256, db: &DB) {
        match self {
            PathComponent::Named(n) => {
                state.update([0]);
                n.update_hash(state, db);
            }
            PathComponent::Unnamed(n) => {
                state.update([1]);
                n.update_hash(state, db);
            }
            PathComponent::File(fd) => {
                state.update([2]);
                fd.update_hash(state, db);
            }
        }
    }
}

impl PathComponent {
    pub fn unwrap_file(self) -> FileId {
        match self {
            PathComponent::File(f) => f,
            _ => panic!("Path component should have been file"),
        }
    }
    pub fn unwrap_named(self) -> FieldName {
        match self {
            PathComponent::Named(name) => name,
            _ => panic!("Path component should have been identifier"),
        }
    }
    pub fn unwrap_ident(self) -> Identifier {
        match self {
            PathComponent::Named(FieldName::Ident(ident)) => ident,
            _ => panic!("Path component should have been identifier"),
        }
    }
}

impl<DB: Interner + ?Sized> DatabasedDisplay<DB> for PathComponent {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            PathComponent::Named(FieldName::Ident(n)) => n.db_fmt(f, db),
            PathComponent::Named(FieldName::Return) => write!(f, "return"),
            PathComponent::Unnamed(x) => write!(f, "{}", x),
            PathComponent::File(fid) => dbwrite!(f, db, "{}", fid),
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct DefId(InternId);

impl DefId {
    pub fn graphviz_name(&self) -> String {
        format!("HirNode{}", self.0.as_u32())
    }
    pub fn parent<DB: Interner + ?Sized>(self, db: &DB) -> DefId {
        let mut path = db.lookup_intern_hir_path(self);
        path.pop();
        db.intern_hir_path(path)
    }
    pub fn child<DB: Interner + ?Sized>(self, db: &DB, name: PathComponent) -> DefId {
        let mut path = db.lookup_intern_hir_path(self);
        path.push(name);
        db.intern_hir_path(path)
    }
    pub fn child_field<DB: Interner + ?Sized>(self, db: &DB, name: FieldName) -> DefId {
        self.child(db, PathComponent::Named(name))
    }
    pub fn is_ancestor_of<DB: Interner + ?Sized>(self, db: &DB, other: DefId) -> bool {
        let other_path = db.lookup_intern_hir_path(other);
        let self_path = db.lookup_intern_hir_path(self);
        other_path.0.starts_with(&self_path.0)
    }
}

impl salsa::InternKey for DefId {
    fn from_intern_id(v: InternId) -> Self {
        DefId(v)
    }

    fn as_intern_id(&self) -> InternId {
        self.0
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct HirPath(Vec<PathComponent>);

impl HirPath {
    pub fn new_file(f: FileId) -> Self {
        HirPath(vec![PathComponent::File(f)])
    }
    pub fn new_fid(f: FileId, n: FieldName) -> Self {
        HirPath(vec![PathComponent::File(f), PathComponent::Named(n)])
    }
    pub fn path(&self) -> &[PathComponent] {
        &self.0
    }
    pub fn push(&mut self, comp: PathComponent) {
        self.0.push(comp)
    }
    pub fn pop(&mut self) -> Option<PathComponent> {
        self.0.pop()
    }
}

impl<DB: Interner + ?Sized> DatabasedDisplay<DB> for HirPath {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        for (i, component) in self.path().iter().enumerate() {
            if i > 0 {
                write!(f, ".")?;
            }
            dbwrite!(f, db, "{}", component)?;
        }
        Ok(())
    }
}

impl<DB: Interner + ?Sized> DatabasedDisplay<DB> for DefId {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        dbwrite!(f, db, "{}", &db.lookup_intern_hir_path(*self))
    }
}

impl<DB: Interner + ?Sized> StableHash<DB> for DefId {
    fn update_hash(&self, state: &mut sha2::Sha256, db: &DB) {
        let path = db.lookup_intern_hir_path(*self);
        hash_array(path.path(), state, db)
    }
}

fn def_name(db: &dyn Interner, defid: DefId) -> Option<FieldName> {
    db.lookup_intern_hir_path(defid)
        .path()
        .last()
        .and_then(|x| match x {
            PathComponent::File(_) | PathComponent::Unnamed(_) => None,
            PathComponent::Named(f) => Some(*f),
        })
}

fn def_hash(db: &dyn Interner, defid: DefId) -> [u8; 32] {
    let mut hasher = Sha256::new();
    defid.update_hash(&mut hasher, db);
    hasher.finalize().try_into().unwrap()
}

#[salsa::query_group(InternerDatabase)]
pub trait Interner: crate::source::Files {
    #[salsa::interned]
    fn intern_identifier(&self, identifier: IdentifierName) -> Identifier;
    #[salsa::interned]
    fn intern_type_var(&self, identifier: TypeVarName) -> TypeVar;
    #[salsa::interned]
    fn intern_hir_path(&self, path: HirPath) -> DefId;

    fn def_name(&self, defid: DefId) -> Option<FieldName>;
    fn def_hash(&self, defid: DefId) -> [u8; 32];
}