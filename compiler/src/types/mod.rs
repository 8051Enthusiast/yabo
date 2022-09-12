pub mod inference;
pub mod represent;
mod to_type;

use std::{collections::HashSet, hash::Hash, sync::Arc};

use salsa::InternId;

use crate::{
    ast::ArrayKind,
    dbeprintln, dbformat,
    error::{Silencable, SilencedError},
    interner::{DefId, FieldName, Interner, TypeVar},
};

use self::{
    inference::InfTypeId,
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
    fn type_contains_unknown(&self, ty: TypeId) -> bool;
    fn type_contains_typevar(&self, ty: TypeId) -> bool;
    fn substitute_typevar(&self, ty: TypeId, replacements: Arc<Vec<TypeId>>) -> TypeId;
    fn parser_result(&self, ty: TypeId) -> Option<TypeId>;
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NominalTypeHead {
    pub kind: NominalKind,
    pub def: DefId,
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
    TypeVarRef(DefId, u32, u32),
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
    TypeVarRef(DefId, u32, u32),
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
pub enum TypeError {
    HeadMismatch(TypeHead, TypeHead),
    HeadIncompatible(TypeHead, TypeHead),
    RecursiveType,
    UnknownField(FieldName),
    UnknownParserdefName(crate::interner::Identifier),
    ParseDefFromMismatch,
    ParserDefArgCountMismatch(usize, usize),
    UnknownTypeVar(TypeVar),
    Silenced,
}

impl Silencable for TypeError {
    type Out = SilencedError;

    fn silence(self) -> Self::Out {
        SilencedError
    }
}

impl From<SilencedError> for TypeError {
    fn from(_: SilencedError) -> Self {
        TypeError::Silenced
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

impl<'a> From<TypeId> for EitherType<'a> {
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
        Type::TypeVarRef(_, _, i) => replacements[i as usize],
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
            let inner = substitute_typevar(db, inner, replacements.clone());
            db.intern_type(Type::Loop(kind, inner))
        }
        Type::ParserArg { result, arg } => {
            let result = substitute_typevar(db, result, replacements.clone());
            let arg = substitute_typevar(db, arg, replacements.clone());
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
        Type::ForAll(inner, _) => return inner,
        Type::Any | Type::Bot | Type::Unknown | Type::Primitive(_) => return ty,
    }
}

fn parser_result(db: &dyn TypeInterner, ty: TypeId) -> Option<TypeId> {
    match db.lookup_intern_type(ty) {
        Type::ParserArg { result, .. } => Some(result),
        _ => None,
    }
}
