use sha2::Digest;
use yaboc_ast::ArrayKind;
use yaboc_base::{databased_display::DatabasedDisplay, hash::StableHash};

use crate::{NominalTypeHead, Type, TypeHead, TypeId};

use super::{inference::InferenceType, InfTypeId, NominalKind, PrimitiveType, TypeInterner};

use yaboc_base::dbwrite;

impl<'a, DB: TypeInterner + ?Sized> DatabasedDisplay<DB> for InfTypeId<'a> {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self.value() {
            InferenceType::Any => write!(f, "any"),
            InferenceType::Bot => write!(f, "bot"),
            InferenceType::Primitive(p) => match p {
                super::PrimitiveType::Bit => write!(f, "bit"),
                super::PrimitiveType::Int => write!(f, "int"),
                super::PrimitiveType::Char => write!(f, "char"),
                super::PrimitiveType::Unit => write!(f, "unit"),
            },
            InferenceType::TypeVarRef(loc, index) => {
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
            InferenceType::Loop(k, inner) => {
                match k {
                    ArrayKind::For => write!(f, "for")?,
                    ArrayKind::Each => write!(f, "each")?,
                };
                dbwrite!(f, db, "[{}]", inner)
            }
            InferenceType::ParserArg { result, arg } => {
                dbwrite!(f, db, "{} *> {}", arg, result)
            }
            InferenceType::FunctionArgs { result, args } => {
                dbwrite!(f, db, "{}(", result)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    dbwrite!(f, db, "{}", arg)?;
                }
                write!(f, ")")
            }
            InferenceType::Unknown => write!(f, "<unknown>"),
            InferenceType::Var(var_id) => {
                write!(f, "<Var {}>", var_id.0)
            }
            InferenceType::InferField(field_name, inner_type) => {
                write!(f, "<InferField {field_name:?}: ")?;
                dbwrite!(f, db, "{}", inner_type)
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
            Type::Any => state.update([0]),
            Type::Bot => state.update([1]),
            Type::Unknown => state.update([2]),
            Type::Primitive(p) => {
                state.update([3]);
                p.update_hash(state, db)
            }
            Type::TypeVarRef(def, index) => {
                state.update([4]);
                def.update_hash(state, db);
                index.update_hash(state, db);
            }
            Type::ForAll(inner, x) => {
                state.update([5]);
                x.len().update_hash(state, db);
                sub_update(inner, state);
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

impl<DB: ?Sized> DatabasedDisplay<DB> for PrimitiveType {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, _: &DB) -> std::fmt::Result {
        match self {
            PrimitiveType::Int => write!(f, "int"),
            PrimitiveType::Bit => write!(f, "bit"),
            PrimitiveType::Char => write!(f, "char"),
            PrimitiveType::Unit => write!(f, "unit"),
        }
    }
}
impl<DB: TypeInterner + ?Sized> DatabasedDisplay<DB> for TypeHead {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            TypeHead::Any => write!(f, "any type"),
            TypeHead::Bot => write!(f, "bottom type"),
            TypeHead::Primitive(p) => p.db_fmt(f, db),
            TypeHead::TypeVarRef(_, index) => {
                dbwrite!(f, db, "'{}", &index)
            }
            TypeHead::Nominal(def) => {
                dbwrite!(f, db, "{}", def)
            }
            TypeHead::Loop(kind) => match kind {
                ArrayKind::For => write!(f, "for array"),
                ArrayKind::Each => write!(f, "each array"),
            },
            TypeHead::ParserArg => write!(f, "a parser"),
            TypeHead::FunctionArgs(arg) => write!(f, "a function with {arg} arguments"),
            TypeHead::Unknown => write!(f, "unknon"),
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
            Type::TypeVarRef(_, index) => {
                dbwrite!(f, db, "'{}", &index)
            }
            Type::ForAll(inner, vars) => {
                write!(f, "forall ")?;
                for (i, arg) in vars.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    dbwrite!(f, db, "{}", arg)?;
                }
                dbwrite!(f, db, ". {}", &inner)
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
            Type::Loop(k, inner) => {
                match k {
                    ArrayKind::For => write!(f, "for")?,
                    ArrayKind::Each => write!(f, "each")?,
                };
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
