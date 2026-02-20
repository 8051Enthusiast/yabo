use sha2::Digest;
use yaboc_ast::ArrayKind;
use yaboc_base::{
    databased_display::DatabasedDisplay, hash::StableHash, low_effort_interner::Uniq,
};

use crate::{inference::InfTypeHead, BlockTypeHead, Type, TypeId, TypeVarRef};

use super::{inference::InferenceType, InfTypeId, NominalKind, PrimitiveType, TypeInterner};

use yaboc_base::dbwrite;

impl<DB: TypeInterner + ?Sized> DatabasedDisplay<DB> for InfTypeId<'_> {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self.value() {
            InferenceType::Primitive(p) => write!(f, "{}", &p),
            InferenceType::TypeVarRef(TypeVarRef(loc, index)) => {
                dbwrite!(f, db, "<Var Ref ({}, {})>", loc, index)
            }
            InferenceType::Block(n) => {
                dbwrite!(f, db, "<anonymous block {}", &n.def)?;
                if !n.ty_args.is_empty() {
                    write!(f, "[")?;
                    for (i, arg) in n.ty_args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        dbwrite!(f, db, "{}", arg)?;
                    }
                    write!(f, "]")?;
                }
                write!(f, ">")?;
                Ok(())
            }
            InferenceType::Loop(ArrayKind::Each, inner) => {
                dbwrite!(f, db, "[{}]", inner)
            }
            InferenceType::ParserArg { result, arg } => {
                dbwrite!(f, db, "{} ~> {}", arg, result)
            }
            InferenceType::FunctionArgs { result, args } => {
                dbwrite!(f, db, "{}({}", result, args)
            }
            InferenceType::Unknown => write!(f, "<unknown>"),
            InferenceType::Var(var_id) => {
                write!(f, "<Var {}>", var_id.0)
            }
            InferenceType::FunArgCons(
                head,
                tail @ InfTypeId(Uniq(_, InferenceType::FunArgCons(..))),
            ) => {
                dbwrite!(f, db, "{}, {}", head, tail)
            }
            InferenceType::FunArgCons(head, InfTypeId(Uniq(_, InferenceType::FunArgNil))) => {
                dbwrite!(f, db, "{})", head)
            }
            InferenceType::FunArgCons(head, tail) => {
                dbwrite!(f, db, "{}, ..{})", head, tail)
            }
            InferenceType::FunArgNil => {
                write!(f, ")")
            }
        }
    }
}

impl<DB: TypeInterner + ?Sized> StableHash<DB> for PrimitiveType {
    fn update_hash(&self, state: &mut sha2::Sha256, db: &DB) {
        match self {
            PrimitiveType::Bit => 0u8,
            PrimitiveType::Int => 1,
            PrimitiveType::Char => 2,
            PrimitiveType::Unit => 3,
        }
        .update_hash(state, db)
    }
}

impl<DB: TypeInterner + ?Sized> StableHash<DB> for NominalKind {
    fn update_hash(&self, state: &mut sha2::Sha256, db: &DB) {
        match self {
            NominalKind::Def => 0u8,
            NominalKind::Block => 1,
        }
        .update_hash(state, db)
    }
}

impl<DB: TypeInterner + ?Sized> StableHash<DB> for TypeId {
    fn update_hash(&self, state: &mut sha2::Sha256, db: &DB) {
        let sub_update =
            |id: TypeId, state: &mut sha2::Sha256| db.type_hash(id).update_hash(state, db);
        match db.lookup_intern_type(*self) {
            Type::Unknown => state.update([2]),
            Type::Primitive(p) => {
                state.update([3]);
                p.update_hash(state, db)
            }
            Type::TypeVarRef(TypeVarRef(def, index)) => {
                state.update([4]);
                def.update_hash(state, db);
                index.update_hash(state, db);
            }
            Type::Block(BlockTypeHead { def, ty_args, .. }) => {
                state.update([6]);
                def.update_hash(state, db);
                ty_args.len().update_hash(state, db);
                for arg in ty_args.iter() {
                    sub_update(*arg, state);
                }
            }
            Type::Loop(kind, inner) => {
                state.update([7]);
                kind.update_hash(state, db);
                sub_update(inner, state);
            }
            Type::ParserArg { result, arg } => {
                state.update([8]);
                sub_update(arg, state);
                sub_update(result, state);
            }
            Type::FunctionArg(inner, args) => {
                state.update([9]);
                sub_update(inner, state);
                args.len().update_hash(state, db);
                for arg in args.iter() {
                    sub_update(*arg, state);
                }
            }
        }
    }
}

impl std::fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveType::Int => write!(f, "int"),
            PrimitiveType::Bit => write!(f, "bit"),
            PrimitiveType::Char => write!(f, "char"),
            PrimitiveType::Unit => write!(f, "unit"),
        }
    }
}

impl<DB: TypeInterner + ?Sized> DatabasedDisplay<DB> for TypeVarRef {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, _: &DB) -> std::fmt::Result {
        write!(f, "'{}", &self.1)
    }
}

impl<DB: TypeInterner + ?Sized> DatabasedDisplay<DB> for InfTypeHead {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            InfTypeHead::Primitive(p) => p.db_fmt(f, db),
            InfTypeHead::TypeVarRef(var) => var.db_fmt(f, db),
            InfTypeHead::Block(def, ..) => {
                dbwrite!(f, db, "{}", def)
            }
            InfTypeHead::Loop(ArrayKind::Each) => write!(f, "array"),
            InfTypeHead::ParserArg => write!(f, "a parser"),
            InfTypeHead::FunctionArgs => {
                write!(f, "a function")
            }
            InfTypeHead::Unknown => write!(f, "unknown"),
            InfTypeHead::FunctionArgCons => {
                write!(f, "a function argument")
            }
            InfTypeHead::FunctionArgNil => {
                write!(f, "no function argument")
            }
            InfTypeHead::Var(_) => write!(f, "an inference var"),
        }
    }
}

impl<DB: TypeInterner + ?Sized> DatabasedDisplay<DB> for TypeId {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        let ty = db.lookup_intern_type(*self);
        match ty {
            Type::Primitive(p) => p.db_fmt(f, db),
            Type::TypeVarRef(var) => var.db_fmt(f, db),
            Type::Block(n) => {
                dbwrite!(f, db, "<anonymous block {}", &n.def)?;
                if !n.ty_args.is_empty() {
                    write!(f, "[")?;
                    for (i, arg) in n.ty_args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        dbwrite!(f, db, "{}", arg)?;
                    }
                    write!(f, "]")?;
                }
                write!(f, ">")?;
                Ok(())
            }
            Type::Loop(ArrayKind::Each, inner) => {
                dbwrite!(f, db, "[{}]", &inner)
            }
            Type::ParserArg { result, arg } => {
                dbwrite!(f, db, "{} ~> {}", &arg, &result)
            }
            Type::FunctionArg(result, args) => {
                dbwrite!(f, db, "{}(", &result)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    dbwrite!(f, db, "{}", arg)?;
                }
                write!(f, ")")
            }
            Type::Unknown => write!(f, "<unknown>"),
        }
    }
}
