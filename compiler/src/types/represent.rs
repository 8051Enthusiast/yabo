use crate::{
    ast::ArrayKind, databased_display::DatabasedDisplay, hash::StableHash, interner::Interner,
};

use super::{inference::InferenceType, InfTypeId, NominalKind, PrimitiveType, TypeInterner};

use crate::dbwrite;

impl<'a, DB: TypeInterner + Interner + ?Sized> DatabasedDisplay<DB> for InfTypeId<'a> {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self.value() {
            InferenceType::Any => write!(f, "any"),
            InferenceType::Bot => write!(f, "bot"),
            InferenceType::Primitive(p) => match p {
                super::PrimitiveType::Bit => write!(f, "bit"),
                super::PrimitiveType::Int => write!(f, "int"),
                super::PrimitiveType::Char => write!(f, "char"),
            },
            InferenceType::TypeVarRef(loc, level, index) => {
                dbwrite!(f, db, "<Var Ref ({}, {}, {})>", loc, level, index)
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
                write!(f, "<InferField {:?}: ", field_name)?;
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

impl<DB: TypeInterner + ?Sized> StableHash<DB> for ArrayKind {
    fn update_hash(&self, state: &mut sha2::Sha256, db: &DB) {
        match self {
            ArrayKind::For => 0u8,
            ArrayKind::Each => 1,
        }
        .update_hash(state, db)
    }
}
