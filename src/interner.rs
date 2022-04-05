use salsa::InternId;

use crate::source::FileId;

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
    Ident(Identifier),
    Return,
    Next,
    Prev,
}

impl From<Identifier> for FieldName {
    fn from(ident: Identifier) -> Self {
        FieldName::Ident(ident)
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum PathComponent {
    Named(FieldName),
    Unnamed(u32),
    File(FileId),
}

impl PathComponent {
    pub fn to_name<DB: Interner + ?Sized>(&self, db: &DB) -> String {
        match self {
            PathComponent::Named(FieldName::Ident(n)) => db.lookup_intern_identifier(*n).name,
            PathComponent::Named(FieldName::Return) => String::from("return"),
            PathComponent::Named(FieldName::Next) => String::from("next"),
            PathComponent::Named(FieldName::Prev) => String::from("prev"),
            PathComponent::Unnamed(x) => format!("{}", x),
            PathComponent::File(f) => match db.path(*f) {
                Some(p) => p.to_string_lossy().to_string(),
                None => String::from("file[anonymous]"),
            },
        }
    }
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

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct HirId(InternId);

impl HirId {
    pub fn graphviz_name(&self) -> String {
        format!("HirNode{}", self.0.as_u32())
    }
    pub fn parent<DB: Interner + ?Sized>(self, db: &DB) -> HirId {
        let mut path = db.lookup_intern_hir_path(self);
        path.pop();
        db.intern_hir_path(path)
    }
    pub fn child<DB: Interner + ?Sized>(self, db: &DB, name: PathComponent) -> HirId {
        let mut path = db.lookup_intern_hir_path(self);
        path.push(name);
        db.intern_hir_path(path)
    }
    pub fn child_field<DB: Interner + ?Sized>(self, db: &DB, name: FieldName) -> HirId {
        self.child(db, PathComponent::Named(name))
    }
}
impl salsa::InternKey for HirId {
    fn from_intern_id(v: InternId) -> Self {
        HirId(v)
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
    pub fn to_name<DB: Interner + ?Sized>(&self, db: &DB) -> String {
        self.path()
            .iter()
            .map(|x| x.to_name(db))
            .collect::<Vec<_>>()
            .join(".")
    }
}

fn path_name(db: &dyn Interner, id: HirId) -> String {
    let path = db.lookup_intern_hir_path(id);
    path.path()
        .iter()
        .map(|x| x.to_name(db))
        .collect::<Vec<_>>()
        .join(".")
}
#[salsa::query_group(InternerDatabase)]
pub trait Interner: crate::source::Files {
    #[salsa::interned]
    fn intern_identifier(&self, identifier: IdentifierName) -> Identifier;
    #[salsa::interned]
    fn intern_type_var(&self, identifier: TypeVarName) -> TypeVar;
    #[salsa::interned]
    fn intern_hir_path(&self, path: HirPath) -> HirId;

    fn path_name(&self, id: HirId) -> String;
}
