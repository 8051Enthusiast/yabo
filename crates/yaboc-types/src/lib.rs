mod connections;
mod deref_levels;
pub mod inference;
pub mod represent;
pub mod to_type;

use std::{collections::HashSet, hash::Hash, sync::Arc};

use inference::InfTypeHead;
use salsa::InternId;

use sha2::Digest;
use yaboc_ast::{ArrayKind, Asts};
pub use yaboc_base::interner::DefId;
use yaboc_base::{
    dbeprintln, dbformat,
    error::{Silencable, SilencedError},
    hash::StableHash,
    interner::{FieldName, Identifier, Interner},
};

use self::{
    inference::InfTypeId,
    to_type::{TyVars, TypeConvertMemo},
};

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TypeId(InternId);
impl salsa::InternKey for TypeId {
    fn from_intern_id(v: InternId) -> Self {
        TypeId(v)
    }

    fn as_intern_id(&self) -> InternId {
        self.0
    }
}

#[salsa::query_group(TypeInternerDatabase)]
pub trait TypeInterner: yaboc_base::source::Files + yaboc_base::interner::Interner + Asts {
    #[salsa::interned]
    fn intern_type(&self, ty: Type) -> TypeId;
    fn type_contains_unknown(&self, ty: TypeId) -> bool;
    fn type_contains_typevar(&self, ty: TypeId) -> bool;
    fn substitute_typevar(&self, ty: TypeId, replacements: Arc<Vec<TypeId>>) -> TypeId;
    fn parser_result(&self, ty: TypeId) -> Option<TypeId>;
    fn type_hash(&self, ty: TypeId) -> [u8; 32];
    fn int(&self) -> TypeId;
}

fn int(db: &dyn TypeInterner) -> TypeId {
    db.intern_type(Type::Primitive(PrimitiveType::Int))
}

fn type_hash(db: &dyn TypeInterner, id: TypeId) -> [u8; 32] {
    let mut hasher = sha2::Sha256::new();
    id.update_hash(&mut hasher, db);
    hasher.finalize().into()
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NominalTypeHead {
    pub kind: NominalKind,
    pub def: DefId,
    pub parse_arg: Option<TypeId>,
    pub fun_args: Arc<Vec<TypeId>>,
    pub ty_args: Arc<Vec<TypeId>>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeVarRef(pub DefId, pub u32);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Unknown,
    Primitive(PrimitiveType),
    TypeVarRef(TypeVarRef),
    Nominal(NominalTypeHead),
    Loop(ArrayKind, TypeId),
    ParserArg { result: TypeId, arg: TypeId },
    FunctionArg(TypeId, Arc<Vec<TypeId>>),
}

pub fn type_contains_unknown(db: &dyn TypeInterner, id: TypeId) -> bool {
    match db.lookup_intern_type(id) {
        Type::Primitive(_) | Type::TypeVarRef(_) => false,
        Type::Unknown => true,
        Type::Nominal(NominalTypeHead {
            parse_arg,
            fun_args,
            ..
        }) => {
            parse_arg.is_some_and(|x| db.type_contains_unknown(x))
                || fun_args.iter().any(|x| db.type_contains_unknown(*x))
        }
        Type::Loop(_, inner) => db.type_contains_unknown(inner),
        Type::ParserArg { result, arg } => {
            db.type_contains_unknown(result) || db.type_contains_unknown(arg)
        }
        Type::FunctionArg(a, b) => {
            db.type_contains_unknown(a) || b.iter().any(|x| db.type_contains_unknown(*x))
        }
    }
}

pub fn type_contains_typevar(db: &dyn TypeInterner, id: TypeId) -> bool {
    match db.lookup_intern_type(id) {
        Type::Unknown | Type::Primitive(_) => false,
        Type::TypeVarRef(_) => true,
        Type::Nominal(NominalTypeHead {
            parse_arg,
            fun_args,
            ..
        }) => {
            parse_arg.is_some_and(|x| db.type_contains_typevar(x))
                || fun_args.iter().any(|x| db.type_contains_typevar(*x))
        }
        Type::Loop(_, inner) => db.type_contains_typevar(inner),
        Type::ParserArg { result, arg } => {
            db.type_contains_typevar(result) || db.type_contains_typevar(arg)
        }
        Type::FunctionArg(a, b) => {
            db.type_contains_typevar(a) || b.iter().any(|x| db.type_contains_typevar(*x))
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeHead {
    Primitive(PrimitiveType),
    TypeVarRef(TypeVarRef),
    Nominal(DefId),
    Loop(ArrayKind),
    ParserArg,
    FunctionArgs(usize),
    Unknown,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrimitiveType {
    Bit,
    Int,
    Char,
    Unit,
    U8,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum NominalKind {
    Fun,
    Def,
    Static,
    Block,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Signature {
    pub ty_args: Arc<Vec<Identifier>>,
    pub from: Option<TypeId>,
    pub args: Option<Arc<Vec<TypeId>>>,
    pub thunk: NominalTypeHead,
    pub thunky: bool,
}

#[derive(Clone, Hash, Debug, PartialEq, Eq)]
pub enum TypeError {
    HeadMismatch(InfTypeHead, InfTypeHead),
    HeadIncompatible(InfTypeHead, InfTypeHead),
    RecursiveType,
    UnknownField(FieldName),
    UnknownName(Identifier),
    ParseDefFromMismatch,
    ParserDefArgCountMismatch(usize, usize),
    NonThunkReference(Identifier),
    NonInferTypeVar(TypeVarRef),
    NonInfer,
    UnsupportedExportArgument {
        def_id: DefId,
        arg_name: Identifier,
    },
    Silenced(SilencedError),
}

impl Silencable for TypeError {
    type Out = SilencedError;

    fn silence(self) -> Self::Out {
        match self {
            TypeError::Silenced(e) => e,
            _ => SilencedError::new(),
        }
    }
}

impl From<SilencedError> for TypeError {
    fn from(x: SilencedError) -> Self {
        TypeError::Silenced(x)
    }
}

#[derive(Clone, Hash, Debug, PartialEq, Eq)]
pub enum TypeConvError {
    TypeVarReturn(DefId, TypeVarRef),
    CyclicReturnThunks(Vec<DefId>),
    PolymorphicRecursion(TypeVarRef, TypeVarRef),
    Silenced(SilencedError),
}

impl Silencable for TypeConvError {
    type Out = SilencedError;

    fn silence(self) -> Self::Out {
        match self {
            TypeConvError::Silenced(e) => e,
            _ => SilencedError::new(),
        }
    }
}

impl From<SilencedError> for TypeConvError {
    fn from(x: SilencedError) -> Self {
        TypeConvError::Silenced(x)
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum EitherType<'a> {
    Regular(TypeId),
    Inference(InfTypeId<'a>),
}

impl<'a> From<InfTypeId<'a>> for EitherType<'a> {
    fn from(x: InfTypeId<'a>) -> Self {
        EitherType::Inference(x)
    }
}

impl From<TypeId> for EitherType<'_> {
    fn from(x: TypeId) -> Self {
        EitherType::Regular(x)
    }
}

pub fn substitute_typevar(
    db: &dyn TypeInterner,
    ty: TypeId,
    replacements: Arc<Vec<TypeId>>,
) -> TypeId {
    match db.lookup_intern_type(ty) {
        Type::TypeVarRef(TypeVarRef(_, i)) => replacements[i as usize],
        Type::Nominal(mut nom) => {
            let parse_arg = nom
                .parse_arg
                .map(|x| substitute_typevar(db, x, replacements.clone()));
            let fun_args = Arc::new(
                nom.fun_args
                    .iter()
                    .map(|x| substitute_typevar(db, *x, replacements.clone()))
                    .collect::<Vec<_>>(),
            );
            let ty_args = Arc::new(
                nom.ty_args
                    .iter()
                    .map(|x| substitute_typevar(db, *x, replacements.clone()))
                    .collect::<Vec<_>>(),
            );
            nom.parse_arg = parse_arg;
            nom.fun_args = fun_args;
            nom.ty_args = ty_args;
            db.intern_type(Type::Nominal(nom))
        }
        Type::Loop(kind, inner) => {
            let inner = substitute_typevar(db, inner, replacements);
            db.intern_type(Type::Loop(kind, inner))
        }
        Type::ParserArg { result, arg } => {
            let result = substitute_typevar(db, result, replacements.clone());
            let arg = substitute_typevar(db, arg, replacements);
            db.intern_type(Type::ParserArg { result, arg })
        }
        Type::FunctionArg(result, args) => {
            let result = substitute_typevar(db, result, replacements.clone());
            let args = Arc::new(
                args.iter()
                    .map(|x| substitute_typevar(db, *x, replacements.clone()))
                    .collect::<Vec<_>>(),
            );
            db.intern_type(Type::FunctionArg(result, args))
        }
        Type::Unknown | Type::Primitive(_) => ty,
    }
}

fn parser_result(db: &dyn TypeInterner, ty: TypeId) -> Option<TypeId> {
    match db.lookup_intern_type(ty) {
        Type::ParserArg { result, .. } => Some(result),
        _ => None,
    }
}
