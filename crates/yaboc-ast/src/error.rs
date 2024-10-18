use yaboc_base::error::{
    diagnostic::{DiagnosticKind, Label},
    Report,
};

use super::{convert::ParseError, Asts};

pub fn errors(db: &(impl Asts + ?Sized)) -> Vec<Report> {
    let mut ret = Vec::new();
    for fid in db.all_files().iter() {
        let errors = match db.ast(*fid) {
            Ok(_) => continue,
            Err(e) => e,
        };
        ret.extend(errors.into_iter().map(parse_error_report))
    }
    ret
}

fn parse_error_report(f: ParseError) -> Report {
    match f {
        ParseError::Generic(span) => Report::new(DiagnosticKind::Error, span.file, "Parse error")
            .with_code(100)
            .with_label(Label::new(span).with_message("error occured here")),
        ParseError::InvalidChar(span) => {
            Report::new(DiagnosticKind::Error, span.file, "Invalid char literal")
                .with_code(101)
                .with_label(Label::new(span).with_message("error occured here"))
        }
        ParseError::NumberTooBig(span) => {
            Report::new(DiagnosticKind::Error, span.file, "Number literal too big")
                .with_code(103)
                .with_label(Label::new(span).with_message("error occured here"))
        }
    }
}
