use crate::{
    ast::ArrayKind,
    databased_display::DatabasedDisplay,
    hir::ParserDefId,
    hir_types::TypeVarCollection,
    types::{NominalKind, PrimitiveType, Type, TypeId},
};

use super::TyHirs;

impl<DB: TyHirs + ?Sized> DatabasedDisplay<DB> for TypeId {
    fn to_db_string(&self, db: &DB) -> String {
        type_to_string(*self, db)
    }
}

fn type_to_string(ty: TypeId, db: &(impl TyHirs + ?Sized)) -> String {
    let ty = db.lookup_intern_type(ty);
    match ty {
        Type::Any => String::from("any"),
        Type::Bot => String::from("bot"),
        Type::Primitive(p) => p.to_db_string(db),
        Type::TypeVarRef(loc, level, index) => {
            let vars = TypeVarCollection::at_id(db, ParserDefId(loc));
            if let Ok(v) = vars {
                if let Some(x) = v.defs[index as usize].name {
                    return db.lookup_intern_type_var(x).name;
                }
            }
            format!(
                "<Var Ref ({}, {}, {})>",
                db.lookup_intern_hir_path(loc).to_name(db),
                level,
                index
            )
        }
        Type::ForAll(inner, vars) => {
            let args = vars
                .iter()
                .map(|x| {
                    x.name
                        .map(|y| db.lookup_intern_type_var(y).name)
                        .unwrap_or(String::from("'_"))
                })
                .collect::<Vec<String>>()
                .join(", ");
            format!("forall {args}. {}", type_to_string(inner, db))
        }
        Type::Nominal(n) => {
            let path = db.lookup_intern_hir_path(n.def).to_name(db);
            let from = n
                .parse_arg
                .map(|x| format!("{} &> ", type_to_string(x, db)))
                .unwrap_or_else(String::new);
            match n.kind {
                NominalKind::Def => {
                    format!("{from}{path}")
                }
                NominalKind::Block => {
                    format!("<anonymous block {from}{path}>")
                }
            }
        }
        Type::Loop(k, inner) => match k {
            ArrayKind::For => format!("for[{}]", type_to_string(inner, db)),
            ArrayKind::Each => format!("each[{}]", type_to_string(inner, db)),
        },
        Type::ParserArg { result, arg } => {
            format!(
                "{} *> {}",
                type_to_string(arg, db),
                type_to_string(result, db)
            )
        }
        Type::FunctionArg(res, args) => {
            let args = args
                .iter()
                .map(|x| type_to_string(*x, db))
                .collect::<Vec<String>>()
                .join(", ");
            format!("{}({})", type_to_string(res, db), args)
        }
        Type::Unknown => String::from("<unknown>"),
    }
}

impl<DB: ?Sized> DatabasedDisplay<DB> for PrimitiveType {
    fn to_db_string(&self, _: &DB) -> String {
        match self {
            PrimitiveType::Int => String::from("int"),
            PrimitiveType::Bit => String::from("bit"),
            PrimitiveType::Char => String::from("char"),
        }
    }
}
