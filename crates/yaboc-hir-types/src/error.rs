use fxhash::FxHashSet;

use yaboc_base::{
    dbformat,
    error::{
        diagnostic::{DiagnosticKind, Label},
        Report, SResult,
    },
    source::{IndirectSpan, Span},
};
use yaboc_hir::HirNodeKind;
use yaboc_resolve::parserdef_ssc::FunctionSscId;
use yaboc_types::TypeError;

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
    errors
        .into_iter()
        .flat_map(|x| make_report(db, x))
        .collect()
}

fn make_report(db: &(impl TyHirs + ?Sized), error: SpannedTypeError) -> Option<Report> {
    let (err, span) = match error {
        SpannedTypeError::Spanned(err, span) => (err, span),
        SpannedTypeError::Silenced(_) => return None,
    };
    let spans = spans(db, span).expect("error getting error spans");
    let (code, message) = match err {
        TypeError::Silenced(_) => return None,
        TypeError::UnknownTypeVar(var) => (501, dbformat!(db, "unknown type variable {}", &var)),
        TypeError::UnknownField(field) => (502, dbformat!(db, "unknown field {}", &field)),
        TypeError::UnknownParserdefName(name) => {
            (503, dbformat!(db, "unknown definition {}", &name))
        }
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
        TypeError::TypeVarReturn(var) => (
            509,
            dbformat!(db, "cannot have a type variable {} as return type", &var),
        ),
        TypeError::CyclicReturnThunks(cyclic_ids) => (510, {
            let mut ret = String::from("cyclic return that can never terminate: ");
            for (i, id) in cyclic_ids.iter().enumerate() {
                if i != 0 {
                    ret.push_str(" -> ");
                }
                ret.push_str(&dbformat!(db, "{}", id));
            }
            ret
        }),
        TypeError::NonThunkReference(name) => (
            511,
            dbformat!(db, "typename reference to non-thunk function {}", &name),
        ),
        TypeError::PolymorphicRecursion(var1, var2) => (
            512,
            dbformat!(db, "polymorphic recursion between {} and {}", &var1, &var2),
        ),
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
