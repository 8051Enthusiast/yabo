use std::num::NonZeroU16;

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

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct StructChoiceIdx(NonZeroU16);
impl StructChoiceIdx {
    pub fn idx(&self) -> usize {
        usize::from(u16::from(self.0)) - 1
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct IdentifierName {
    pub name: String,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum PathComponent {
    Named(Identifier),
    Unnamed(u32),
    Choice(StructChoiceIdx, u32),
    File(FileId),
}

impl PathComponent {
    pub fn to_name(&self, db: &dyn Interner) -> String {
        match self {
            PathComponent::Named(n) => db.lookup_intern_identifier(*n).name,
            PathComponent::Unnamed(x) => format!("{}", x),
            PathComponent::Choice(_, _) => todo!(),
            PathComponent::File(f) => match db.path(*f) {
                Some(p) => p.to_string_lossy().to_string(),
                None => String::from("file[anonymous]"),
            },
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct HirId(InternId);
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
    pub fn new(f: FileId, n: Identifier) -> Self {
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
    pub fn to_name(&self, db: &dyn Interner) -> String {
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
    fn intern_hir_path(&self, path: HirPath) -> HirId;

    fn path_name(&self, id: HirId) -> String;
}
