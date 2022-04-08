use crate::{
    ast::ArrayKind,
    databased_display::DatabasedDisplay,
    dbwrite,
    hir::ParserDefId,
    hir_types::TypeVarCollection,
    types::{NominalKind, PrimitiveType, Type, TypeId},
};

use super::TyHirs;

impl<DB: TyHirs + ?Sized> DatabasedDisplay<DB> for TypeId {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        let ty = db.lookup_intern_type(*self);
        match ty {
            Type::Any => write!(f, "any"),
            Type::Bot => write!(f, "bot"),
            Type::Primitive(p) => p.db_fmt(f, db),
            Type::TypeVarRef(loc, level, index) => {
                let vars = TypeVarCollection::at_id(db, ParserDefId(loc));
                if let Ok(v) = vars {
                    if let Some(x) = v.defs.get(index as usize) {
                        return dbwrite!(f, db, "{}", x);
                    }
                }
                dbwrite!(f, db, "<Var Ref ({}, {}, {})>", &loc, &level, &index)
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

impl<DB: ?Sized> DatabasedDisplay<DB> for PrimitiveType {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, _: &DB) -> std::fmt::Result {
        match self {
            PrimitiveType::Int => write!(f, "int"),
            PrimitiveType::Bit => write!(f, "bit"),
            PrimitiveType::Char => write!(f, "char"),
        }
    }
}
