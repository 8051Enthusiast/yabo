use salsa::InternId;
use sha2::{Digest, Sha256};

use crate::{databased_display::DatabasedDisplay, dbwrite, hash::StableHash, source::FileId};

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
            FieldName::Return => state.update([0]),
            FieldName::Ident(id) => {
                state.update([1]);
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
        }
    }
}

impl PathComponent {
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
    pub fn unwrap_unnamed(self) -> u32 {
        match self {
            PathComponent::Unnamed(n) => n,
            _ => panic!("Path component should have been unnamed"),
        }
    }
}

impl<DB: Interner + ?Sized> DatabasedDisplay<DB> for PathComponent {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            PathComponent::Named(FieldName::Ident(n)) => n.db_fmt(f, db),
            PathComponent::Named(FieldName::Return) => write!(f, "return"),
            PathComponent::Unnamed(x) => write!(f, "{x}"),
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum DefinitionPath {
    Module(FileId),
    Path(PathComponent, DefId),
}

impl DefinitionPath {
    fn parent(&self) -> Option<DefId> {
        match self {
            DefinitionPath::Module(_) => None,
            DefinitionPath::Path(_, id) => Some(*id),
        }
    }
    fn component(&self) -> Option<PathComponent> {
        match self {
            DefinitionPath::Module(_) => None,
            DefinitionPath::Path(component, _) => Some(*component),
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct DefId(InternId);

impl DefId {
    pub fn graphviz_name(&self) -> String {
        format!("HirNode{}", self.0.as_u32())
    }
    pub fn parent<DB: Interner + ?Sized>(self, db: &DB) -> Option<DefId> {
        db.lookup_intern_hir_path(self).parent()
    }
    pub fn child<DB: Interner + ?Sized>(self, db: &DB, name: PathComponent) -> DefId {
        let cons = DefinitionPath::Path(name, self);
        db.intern_hir_path(cons)
    }
    pub fn child_field<DB: Interner + ?Sized>(self, db: &DB, name: FieldName) -> DefId {
        self.child(db, PathComponent::Named(name))
    }
    pub fn is_ancestor_of<DB: Interner + ?Sized>(self, db: &DB, other: DefId) -> bool {
        let mut id = Some(other);
        while let Some(i) = id {
            if i == self {
                return true;
            }
            id = i.parent(db);
        }
        false
    }
    pub fn unwrap_path_end<DB: Interner + ?Sized>(self, db: &DB) -> PathComponent {
        db.lookup_intern_hir_path(self).component().unwrap()
    }
    pub fn unwrap_name<DB: Interner + ?Sized>(self, db: &DB) -> Identifier {
        db.lookup_intern_hir_path(self)
            .component()
            .unwrap()
            .unwrap_ident()
    }
    pub fn unwrap_unnamed_id<DB: Interner + ?Sized>(self, db: &DB) -> u32 {
        db.lookup_intern_hir_path(self)
            .component()
            .unwrap()
            .unwrap_unnamed()
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
impl<DB: Interner + ?Sized> DatabasedDisplay<DB> for DefId {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match db.lookup_intern_hir_path(*self) {
            DefinitionPath::Module(fid) => dbwrite!(f, db, "{}", &fid),
            DefinitionPath::Path(component, id) => dbwrite!(f, db, "{}.{}", &id, &component),
        }
    }
}

impl<DB: Interner + ?Sized> StableHash<DB> for DefId {
    fn update_hash(&self, state: &mut sha2::Sha256, db: &DB) {
        match db.lookup_intern_hir_path(*self) {
            DefinitionPath::Module(fid) => {
                state.update([0]);
                fid.update_hash(state, db);
            }
            DefinitionPath::Path(component, id) => {
                state.update([1]);
                component.update_hash(state, db);
                id.update_hash(state, db);
            }
        }
    }
}

fn def_name(db: &dyn Interner, defid: DefId) -> Option<FieldName> {
    match db.lookup_intern_hir_path(defid) {
        DefinitionPath::Path(PathComponent::Named(f), _) => Some(f),
        _ => None,
    }
}

fn def_hash(db: &dyn Interner, defid: DefId) -> [u8; 32] {
    let mut hasher = Sha256::new();
    defid.update_hash(&mut hasher, db);
    hasher.finalize().into()
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Regex(InternId);

impl salsa::InternKey for Regex {
    fn from_intern_id(v: InternId) -> Self {
        Regex(v)
    }

    fn as_intern_id(&self) -> InternId {
        self.0
    }
}

#[salsa::query_group(InternerDatabase)]
pub trait Interner: crate::source::Files {
    #[salsa::interned]
    fn intern_identifier(&self, identifier: IdentifierName) -> Identifier;
    #[salsa::interned]
    fn intern_type_var(&self, identifier: TypeVarName) -> TypeVar;
    #[salsa::interned]
    fn intern_hir_path(&self, path: DefinitionPath) -> DefId;
    #[salsa::interned]
    fn intern_regex(&self, regex: String) -> Regex;

    fn def_name(&self, defid: DefId) -> Option<FieldName>;
    fn def_hash(&self, defid: DefId) -> [u8; 32];
}
