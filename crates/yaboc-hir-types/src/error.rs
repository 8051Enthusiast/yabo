use fxhash::FxHashSet;

use yaboc_base::{
    dbformat,
    error::{
        diagnostic::{DiagnosticKind, Label},
        Report, SResult,
    },
    interner::{DefId, Identifier},
    source::{IndirectSpan, Span},
};
use yaboc_hir::HirNodeKind;
use yaboc_resolve::parserdef_ssc::FunctionSscId;
use yaboc_types::{TypeConvError, TypeError, TypeVarRef};

use crate::TypeVarCollection;

use super::{SpannedTypeError, TyHirs};

pub fn errors(db: &(impl TyHirs + ?Sized)) -> Vec<Report> {
    let modules = db.all_modules();
    let parsers = db.all_parserdefs();
    let mut sscs: FxHashSet<FunctionSscId> = Default::default();
    for module in modules {
        if let Ok(x) = db.mod_parser_ssc(module) {
            sscs.extend(x.sscs.values());
        }
    }
    let mut errors = Vec::new();
    for ssc in sscs {
        if let Err(e) = db.ssc_types(ssc) {
            errors.push(e)
        }
    }
    for parser in parsers {
        if let Err(e) = db.parser_args_error(parser) {
            errors.push(e);
        }
    }
    if let Ok(errs) = db.validate_export_arguments() {
        errors.extend(errs);
    }
    errors
        .into_iter()
        .flat_map(|x| make_report(db, x))
        .collect()
}

fn make_report(db: &(impl TyHirs + ?Sized), error: SpannedTypeError) -> Option<Report> {
    match error {
        SpannedTypeError::Spanned(err, span) => {
            let spans = spans(db, span).expect("error getting error spans");
            make_type_error(err, db, spans)
        }
        SpannedTypeError::Conv(err) => make_type_conv_error(err, db),
        SpannedTypeError::Silenced(_) => None,
    }
}

fn get_var_name(db: &(impl TyHirs + ?Sized), var: TypeVarRef) -> Identifier {
    TypeVarCollection::at_id(db, yaboc_hir::ParserDefId(var.0))
        .unwrap()
        .defs[var.1 as usize]
}

fn make_type_conv_error(err: TypeConvError, db: &(impl TyHirs + ?Sized)) -> Option<Report> {
    let def_span = |id: DefId| spans(db, IndirectSpan::default_span(id)).unwrap();
    let (code, spans, message) = match err {
        TypeConvError::TypeVarReturn(def, var) => {
            let var_name = get_var_name(db, var);
            (
                509,
                def_span(def),
                dbformat!(
                    db,
                    "cannot have a type variable {} as return type of {}",
                    &var_name,
                    &def
                ),
            )
        }
        TypeConvError::CyclicReturnThunks(cyclic_ids) => {
            let mut spans = Vec::new();
            for id in cyclic_ids.iter() {
                spans.extend(def_span(*id));
            }
            let mut msg = String::from("cyclic return that can never terminate: ");
            for (i, id) in cyclic_ids.iter().enumerate() {
                if i != 0 {
                    msg.push_str(" -> ");
                }
                msg.push_str(&dbformat!(db, "{}", id));
            }
            (510, spans, msg)
        }

        TypeConvError::PolymorphicRecursion(var1, var2) => {
            let def = var1.0;
            let spans = def_span(def);
            let name1 = get_var_name(db, var1);
            let name2 = get_var_name(db, var2);
            (
                512,
                spans,
                dbformat!(
                    db,
                    "polymorphic recursion between {} and {}",
                    &name1,
                    &name2
                ),
            )
        }
        TypeConvError::Silenced(_) => return None,
    };
    let mut rbuild = Report::new(DiagnosticKind::Error, spans[0].file, &message).with_code(code);
    for (i, span) in spans.iter().enumerate() {
        let label = Label::new(*span);
        if i == spans.len() - 1 {
            rbuild.add_label(label.with_message("error introduced here"));
        } else {
            rbuild.add_label(label);
        }
    }
    Some(rbuild)
}
fn make_type_error(
    err: TypeError,
    db: &(impl TyHirs + ?Sized),
    spans: Vec<Span>,
) -> Option<Report> {
    let (code, message) = match err {
        TypeError::Silenced(_) => return None,
        TypeError::UnknownField(field) => (502, dbformat!(db, "unknown field {}", &field)),
        TypeError::UnknownName(name) => (503, dbformat!(db, "unknown type name {}", &name)),
        TypeError::ParserDefArgCountMismatch(first, second) => (
            504,
            format!("cannot unify functions with different argument counts: {first} and {second}"),
        ),
        TypeError::ParseDefFromMismatch => (
            505,
            String::from(
                "parserdef with 'from' argument, but definition does not have a from argument",
            ),
        ),
        TypeError::RecursiveType => (506, String::from("recursive types are not supported")),
        TypeError::HeadMismatch(gotten, expected) => (
            507,
            dbformat!(db, "expected {}, but got {}", &expected, &gotten),
        ),
        TypeError::HeadIncompatible(first, second) => (
            508,
            dbformat!(db, "cannot unify {} and {}", &first, &second),
        ),
        TypeError::NonThunkReference(name) => (
            511,
            dbformat!(db, "typename reference to non-thunk function {}", &name),
        ),
        TypeError::NonInferTypeVar(var) => {
            let varname = get_var_name(db, var);
            (
                513,
                dbformat!(
                    db,
                    "could not find type variable of current function corresponding to {} of {}",
                    &varname,
                    &var.0
                ),
            )
        }
        TypeError::UnsupportedExportArgument { def_id: _, arg_name } => (
            515,
            dbformat!(
                db,
                "exported function has unsupported argument type: argument '{}' has unsupported type. Only integer arguments are supported for exported functions.",
                &arg_name
            ),
        ),
        TypeError::NonInfer => (514, String::from("could not infer type")),
    };
    let mut rbuild = Report::new(DiagnosticKind::Error, spans[0].file, &message).with_code(code);
    for (i, span) in spans.iter().enumerate() {
        let label = Label::new(*span);
        if i == spans.len() - 1 {
            rbuild.add_label(label.with_message("error introduced here"));
        } else {
            rbuild.add_label(label);
        }
    }
    Some(rbuild)
}

fn spans(db: &(impl TyHirs + ?Sized), span: IndirectSpan) -> SResult<Vec<Span>> {
    let node = db.hir_node(span.0)?;
    if node.is_kind(HirNodeKind::ChoiceIndirection.into()) {
        let mut ret = Vec::new();
        for child in db.indirection_targets(span.0)?.iter() {
            ret.push(db.indirect_span(IndirectSpan::default_span(*child))?);
        }
        return Ok(ret);
    }
    let span = db.indirect_span(span).ok().unwrap();
    Ok(vec![span])
}
