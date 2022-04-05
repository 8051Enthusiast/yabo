use crate::{ast::ArrayKind, interner::Interner, databased_display::DatabasedDisplay};
use std::fmt::Write;

use super::{InfTypeId, InferenceType, NominalKind, TypeInterner};

pub fn print_inftype<DB: TypeInterner + Interner + ?Sized>(
    db: &DB,
    inftype: InfTypeId,
    out: &mut String,
) {
    match db.lookup_intern_inference_type(inftype) {
        InferenceType::Any => out.push_str("any"),
        InferenceType::Bot => out.push_str("bot"),
        InferenceType::Primitive(p) => match p {
            super::PrimitiveType::Bit => out.push_str("bit"),
            super::PrimitiveType::Int => out.push_str("int"),
            super::PrimitiveType::Char => out.push_str("char"),
        },
        InferenceType::TypeVarRef(loc, level, index) => write!(
            out,
            "<Var Ref ({}, {}, {})>",
            db.lookup_intern_hir_path(loc).to_name(db),
            level,
            index
        )
        .unwrap(),
        InferenceType::Nominal(n) => {
            let path = db.lookup_intern_hir_path(n.def).to_name(db);
            match n.kind {
                NominalKind::Block => {
                    out.push_str("<anonymous block ");
                }
                NominalKind::Def => (),
            }
            n.parse_arg.map(|x| {
                print_inftype(db, x, out);
                out.push_str(" &> ")
            });
            write!(out, "{}", path).unwrap();
            match n.kind {
                NominalKind::Block => {
                    out.push_str(">");
                }
                _ => (),
            }
        }
        InferenceType::Loop(k, inner) => {
            match k {
                ArrayKind::For => out.push_str("for"),
                ArrayKind::Each => out.push_str("each"),
            };
            out.push_str("[");
            print_inftype(db, inner, out);
            out.push_str("]");
        }
        InferenceType::ParserArg { result, arg } => {
            print_inftype(db, arg, out);
            out.push_str(" *> ");
            print_inftype(db, result, out);
        }
        InferenceType::FunctionArgs { result, args } => {
            print_inftype(db, result, out);
            out.push_str("(");
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                print_inftype(db, *arg, out);
            }
            out.push_str(")");
        }
        InferenceType::Unknown => out.push_str("<unknown>"),
        InferenceType::Var(var_id) => {
            write!(out, "<Var {}>", var_id.0).unwrap();
        }
        InferenceType::InferField(field_name, inner_type) => {
            write!(out, "<InferField {:?}: ", field_name).unwrap();
            print_inftype(db, inner_type, out);
            out.push_str(">");
        }
    }
}

impl<DB: TypeInterner + Interner + ?Sized> DatabasedDisplay<DB> for InfTypeId {
    fn to_db_string(&self, db: &DB) -> String {
        let mut out = String::new();
        print_inftype(db, *self, &mut out);
        out
    }
}