use yaboc_base::error::{
    diagnostic::{DiagnosticKind, Label},
    Report,
};

use crate::{Constraints, LenError};

pub fn errors(db: &(impl Constraints + ?Sized)) -> Vec<Report> {
    let parserdefs = db.all_parserdefs();
    let mut reports = Vec::new();
    for pd in parserdefs {
        for err in db.len_errors(pd).into_iter().flatten() {
            if let Some(report) = make_report(db, err) {
                reports.push(report);
            }
        }
    }
    reports
}

fn make_report(db: &(impl Constraints + ?Sized), err: LenError) -> Option<Report> {
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
