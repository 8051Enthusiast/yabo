use yaboc_base::error::{
    diagnostic::{DiagnosticKind, Label},
    Report,
};

use super::{convert::HirConversionError, Hirs};

pub fn errors(db: &(impl Hirs + ?Sized)) -> Vec<Report> {
    let mut ret = Vec::new();
    for parserdef in db.all_parserdefs() {
        let collection = match db.hir_parser_collection(parserdef.0) {
            Err(_) | Ok(None) => continue,
            Ok(Some(x)) => x,
        };
        let errors = collection.errors.use_error();
        for error in errors {
            ret.extend(conversion_report(error))
        }
    }
    ret
}

fn conversion_report(error: HirConversionError) -> Option<Report> {
    match error {
        HirConversionError::DuplicateField {
            first, duplicate, ..
        } => Some(
            Report::new(
                DiagnosticKind::Error,
                duplicate.file,
                "Duplicate field in block",
            )
            .with_code(201)
            .with_label(
                Label::new(duplicate).with_message("This field already appeared in this block"),
            )
            .with_label(Label::new(first).with_message("the previous appearnce of the field")),
        ),
        HirConversionError::Silenced => None,
        HirConversionError::DuplicateArg { place, .. } => Some(
            Report::new(DiagnosticKind::Error, place.file, "Duplicate argument name")
                .with_code(202)
                .with_label(Label::new(place).with_message("this argument is duplicate")),
        ),
        HirConversionError::EofInconsistentConjunction { span } => Some(
            Report::new(
                DiagnosticKind::Error,
                span.file,
                "!eof inconsistently applied to conjunctions in constraint",
            )
            .with_code(203)
            .with_label(
                Label::new(span).with_message("all conjunctions must be either !eof or not"),
            ),
        ),
    }
}
