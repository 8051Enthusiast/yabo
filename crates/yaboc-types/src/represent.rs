use sha2::Digest;
use yaboc_ast::ArrayKind;
use yaboc_base::{databased_display::DatabasedDisplay, hash::StableHash, interner::FieldName};

use crate::{
    inference::{Application, InfTypeHead},
    NominalTypeHead, Type, TypeId, TypeVarRef,
};

use super::{inference::InferenceType, InfTypeId, NominalKind, PrimitiveType, TypeInterner};

use yaboc_base::dbwrite;

impl<'a, DB: TypeInterner + ?Sized> DatabasedDisplay<DB> for InfTypeId<'a> {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self.value() {
            InferenceType::Any => write!(f, "any"),
            InferenceType::Bot => write!(f, "bot"),
            InferenceType::Primitive(p) => write!(f, "{}", &p),
            InferenceType::TypeVarRef(TypeVarRef(loc, index)) => {
                dbwrite!(f, db, "<Var Ref ({}, {})>", loc, index)
            }
            InferenceType::Nominal(n) => {
                if let NominalKind::Block = n.kind {
                    write!(f, "<anonymous block ")?;
                }
                if let Some(x) = n.parse_arg {
                    dbwrite!(f, db, "{} &> {}", &x, &n.def)?;
                } else {
                    dbwrite!(f, db, "{}", &n.def)?;
                }
                if let NominalKind::Block = n.kind {
                    write!(f, ">")?;
                }
                Ok(())
            }
            InferenceType::Loop(ArrayKind::Each, inner) => {
                dbwrite!(f, db, "[{}]", inner)
            }
            InferenceType::ParserArg { result, arg } => {
                dbwrite!(f, db, "{} *> {}", arg, result)
            }
            InferenceType::FunctionArgs {
                result,
                args,
                partial,
            } => {
                dbwrite!(f, db, "{}(", result)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    dbwrite!(f, db, "{}", arg)?;
                }
                if *partial == Application::Partial {
                    write!(f, ", ..")?;
                }
                write!(f, ")")
            }
            InferenceType::Unknown => write!(f, "<unknown>"),
            InferenceType::Var(var_id) => {
                write!(f, "<Var {}>", var_id.0)
            }
            InferenceType::InferField(field_name, inner_type) => {
                write!(f, "<InferField {field_name:?}: ")?;
                dbwrite!(f, db, "{}>", inner_type)
            }
            InferenceType::InferIfResult(non_parser, inner_type, cont) => {
                write!(f, "<InferIfResult")?;
                if let Some(non_parser) = non_parser {
                    dbwrite!(f, db, " {}", non_parser)?;
                }
                write!(f, ": ")?;
                dbwrite!(f, db, "{}, {}>", inner_type, cont)
            }
            InferenceType::SizeOf => write!(f, "<SizeOf>"),
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
            PrimitiveType::U8 => 4,
        }
        .update_hash(state, db)
    }
}

impl<DB: TypeInterner + ?Sized> StableHash<DB> for NominalKind {
    fn update_hash(&self, state: &mut sha2::Sha256, db: &DB) {
        match self {
            NominalKind::Def => 0u8,
            NominalKind::Block => 1,
            NominalKind::Fun => 2,
            NominalKind::Static => 3,
        }
        .update_hash(state, db)
    }
}

impl<DB: TypeInterner + ?Sized> StableHash<DB> for TypeId {
    fn update_hash(&self, state: &mut sha2::Sha256, db: &DB) {
        let sub_update =
            |id: TypeId, state: &mut sha2::Sha256| db.type_hash(id).update_hash(state, db);
        match db.lookup_intern_type(*self) {
            Type::Any => state.update([0]),
            Type::Bot => state.update([1]),
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
            Type::Nominal(NominalTypeHead {
                kind, def, ty_args, ..
            }) => {
                state.update([6]);
                kind.update_hash(state, db);
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
            PrimitiveType::U8 => write!(f, "u8"),
        }
    }
}
impl<DB: TypeInterner + ?Sized> DatabasedDisplay<DB> for InfTypeHead {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            InfTypeHead::Any => write!(f, "any type"),
            InfTypeHead::Bot => write!(f, "bottom type"),
            InfTypeHead::Primitive(p) => p.db_fmt(f, db),
            InfTypeHead::TypeVarRef(TypeVarRef(_, index)) => {
                dbwrite!(f, db, "'{}", &index)
            }
            InfTypeHead::Nominal(def) => {
                dbwrite!(f, db, "{}", def)
            }
            InfTypeHead::Loop(ArrayKind::Each) => write!(f, "array"),
            InfTypeHead::ParserArg => write!(f, "a parser"),
            InfTypeHead::FunctionArgs(arg, Application::Full) => {
                write!(f, "a function with {arg} arguments")
            }
            InfTypeHead::FunctionArgs(arg, Application::Partial) => {
                write!(f, "a function with at least {arg} arguments")
            }
            InfTypeHead::Unknown => write!(f, "unknon"),
            InfTypeHead::InferField(name) => match name {
                FieldName::Return => write!(f, "field return"),
                FieldName::Ident(name) => {
                    dbwrite!(f, db, "field {}", name)
                }
            },
            InfTypeHead::InferIfResult(_) => write!(f, "if result"),
            InfTypeHead::SizeOf => write!(f, "array or parser"),
            InfTypeHead::Var(_) => todo!(),
        }
    }
}

impl<DB: TypeInterner + ?Sized> DatabasedDisplay<DB> for TypeId {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        let ty = db.lookup_intern_type(*self);
        match ty {
            Type::Any => write!(f, "any"),
            Type::Bot => write!(f, "bot"),
            Type::Primitive(p) => p.db_fmt(f, db),
            Type::TypeVarRef(TypeVarRef(_, index)) => {
                dbwrite!(f, db, "'{}", &index)
            }
            Type::Nominal(n) => {
                if let NominalKind::Block = n.kind {
                    write!(f, "<anonymous block ")?;
                }
                if let Some(x) = n.parse_arg {
                    dbwrite!(f, db, "{} &> {}", &x, &n.def)?;
                } else {
                    dbwrite!(f, db, "{}", &n.def)?;
                }
                if let NominalKind::Block = n.kind {
                    write!(f, ">")?;
                }
                Ok(())
            }
            Type::Loop(ArrayKind::Each, inner) => {
                dbwrite!(f, db, "[{}]", &inner)
            }
            Type::ParserArg { result, arg } => {
                dbwrite!(f, db, "{} *> {}", &arg, &result)
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
