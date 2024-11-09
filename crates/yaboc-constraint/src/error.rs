use fxhash::FxHashSet;
use yaboc_backtrack::EffectError;
use yaboc_base::{
    error::{
        diagnostic::{Diagnostic, DiagnosticKind, Label},
        Report, SResult,
    },
    source::IndirectSpan,
};
use yaboc_resolve::parserdef_ssc::FunctionSscId;
use yaboc_types::DefId;

use crate::{Constraints, LenError, Origin};

pub fn errors(db: &(impl Constraints + ?Sized)) -> Vec<Report> {
    let parserdefs = db.all_parserdefs();
    let mut reports = Vec::new();
    for pd in parserdefs {
        for err in db.len_errors(pd).into_iter().flatten() {
            if let Some(report) = make_len_report(db, err) {
                reports.push(report);
            }
        }
    }
    let mut sscs: FxHashSet<FunctionSscId> = Default::default();
    for module in db.all_modules() {
        if let Ok(x) = db.mod_parser_ssc(module) {
            sscs.extend(x.sscs.values());
        }
    }
    for ssc in sscs {
        let Ok(res) = db.ssc_bt_vals(ssc) else {
            continue;
        };
        for (def, err) in res.errors.iter() {
            if let Ok(err) = make_bt_report(db, *err, *def) {
                reports.push(err);
            }
        }
    }
    reports
}

fn make_bt_report(
    db: &(impl Constraints + ?Sized),
    err: EffectError,
    def: DefId,
) -> SResult<Diagnostic> {
    let pd = db.hir_parent_parserdef(def)?;
    let terms = db.bt_term(pd)?;
    let idx = err.index();
    let origin = terms.origin[idx];
    let span = match origin {
        Origin::Expr(expr, idx) => {
            let span_idx = db.resolve_expr(expr)?.data[idx];
            IndirectSpan::new(expr.0, span_idx)
        }
        Origin::Node(id) => IndirectSpan::default_span(id),
    };
    let span = db.indirect_span(span)?;
    match err {
        EffectError::UnscopedEffect(_) => {
            let report = Report::new(
                DiagnosticKind::Error,
                span.file,
                "Backtracking invocation outside function context",
            )
            .with_code(603)
            .with_label(Label::new(span).with_message("invocation here"));
            Ok(report)
        }
        EffectError::EffectOnNonEffectfulInvocation(_) => {
            let report = Report::new(
                DiagnosticKind::Error,
                span.file,
                "Invocation that could backtrack is not marked as such",
            )
            .with_code(604)
            .with_label(Label::new(span).with_message(
                "invocation here needs to be marked with a question or exclamation mark",
            ));
            Ok(report)
        }
        EffectError::ForbiddenParametersOnArg(_) => {
            let report = Report::new(
                DiagnosticKind::Error,
                span.file,
                "Argument given to function has parameters that are not allowed to backtrack",
            )
            .with_code(605)
            .with_label(Label::new(span).with_message("call using the argument is here"));
            Ok(report)
        }
        EffectError::DisallowedBacktrackingArg(_) => {
            let report = Report::new(
                DiagnosticKind::Error,
                span.file,
                "Argument may backtrack, but invoked function does not allow backtracking",
            )
            .with_code(606)
            .with_label(Label::new(span).with_message("call using the argument is here"));
            Ok(report)
        }
    }
}

fn make_len_report(db: &(impl Constraints + ?Sized), err: LenError) -> Option<Report> {
    let report = match err {
        LenError::NonsizedInArray(span) => {
            let span = db.indirect_span(span).ok()?;
            Report::new(
                DiagnosticKind::Error,
                span.file,
                "nonsized parser where sized parser is expected",
            )
            .with_code(601)
            .with_label(Label::new(span).with_message("nonsized parser argument here"))
        }
        LenError::SizedWithNonsizedArg { loc, arg_loc } => {
            let loc = db.indirect_span(loc).ok()?;
            let arg_loc = db.indirect_span(arg_loc).ok()?;
            Report::new(
                DiagnosticKind::Error,
                loc.file,
                "sized parser depending on nonsized argument",
            )
            .with_code(602)
            .with_label(Label::new(loc).with_message("sized parser required here"))
            .with_label(
                Label::new(arg_loc)
                    .with_message("depends on nonsized parser argument defined here"),
            )
        }
        LenError::Silenced(_) => return None,
    };
    Some(report)
}
