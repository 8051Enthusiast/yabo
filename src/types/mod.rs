pub mod inference;
pub mod represent;
mod to_type;

use std::{collections::HashSet, hash::Hash, sync::Arc};

use salsa::InternId;

use crate::{
    ast::ArrayKind,
    dbeprintln, dbformat,
    error::{Silencable, SilencedError},
    interner::{FieldName, HirId, Interner, TypeVar},
};

use self::{
    inference::{InfTypeId, InferenceType},
    to_type::{TypeConvertMemo, VarStack},
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
pub trait TypeInterner: crate::source::Files {
    #[salsa::interned]
    fn intern_type(&self, ty: Type) -> TypeId;
    #[salsa::interned]
    fn intern_inference_type(&self, ty: InferenceType) -> InfTypeId;
    fn type_contains_unknown(&self, ty: TypeId) -> bool;
    fn type_contains_typevar(&self, ty: TypeId) -> bool;
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NominalTypeHead {
    pub kind: NominalKind,
    pub def: HirId,
    pub parse_arg: Option<TypeId>,
    pub fun_args: Arc<Vec<TypeId>>,
    pub ty_args: Arc<Vec<TypeId>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Any,
    Bot,
    Unknown,
    Primitive(PrimitiveType),
    TypeVarRef(HirId, u32, u32),
    ForAll(TypeId, Arc<Vec<TypeVar>>),
    Nominal(NominalTypeHead),
    Loop(ArrayKind, TypeId),
    ParserArg { result: TypeId, arg: TypeId },
    FunctionArg(TypeId, Arc<Vec<TypeId>>),
}

pub fn type_contains_unknown(db: &dyn TypeInterner, id: TypeId) -> bool {
    match db.lookup_intern_type(id) {
        Type::Any | Type::Bot | Type::Primitive(_) | Type::TypeVarRef(_, _, _) => false,
        Type::Unknown => true,
        Type::ForAll(inner, _) => db.type_contains_unknown(inner),
        Type::Nominal(NominalTypeHead {
            parse_arg,
            fun_args,
            ..
        }) => {
            parse_arg.map_or(false, |x| db.type_contains_unknown(x))
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
        Type::Any | Type::Bot | Type::Unknown | Type::Primitive(_) => false,
        Type::TypeVarRef(_, _, _) => true,
        Type::ForAll(inner, _) => db.type_contains_typevar(inner),
        Type::Nominal(NominalTypeHead {
            parse_arg,
            fun_args,
            ..
        }) => {
            parse_arg.map_or(false, |x| db.type_contains_typevar(x))
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
    Any,
    Bot,
    Primitive(PrimitiveType),
    TypeVarRef,
    Nominal(HirId),
    Loop(ArrayKind),
    ParserArg,
    FunctionArgs,
    Unknown,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum PrimitiveType {
    Bit,
    Int,
    Char,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum NominalKind {
    Def,
    Block,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Signature {
    pub ty_args: Arc<Vec<TypeVar>>,
    pub from: Option<TypeId>,
    pub args: Arc<Vec<TypeId>>,
    pub thunk: TypeId,
}

#[derive(Clone, Hash, Debug, PartialEq, Eq)]
pub struct TypeError;

impl Silencable for TypeError {
    type Out = SilencedError;

    fn silence(self) -> Self::Out {
        SilencedError
    }
}

impl From<SilencedError> for TypeError {
    fn from(_: SilencedError) -> Self {
        TypeError
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum EitherType {
    Regular(TypeId),
    Inference(InfTypeId),
}

impl From<InfTypeId> for EitherType {
    fn from(x: InfTypeId) -> Self {
        EitherType::Inference(x)
    }
}

impl From<TypeId> for EitherType {
    fn from(x: TypeId) -> Self {
        EitherType::Regular(x)
    }
}
