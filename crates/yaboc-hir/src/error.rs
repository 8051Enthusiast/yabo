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
        HirConversionError::NakedDef { span }  => Some(
            Report::new(
                DiagnosticKind::Error,
                span.file,
                "Naked definition",
            )
            .with_code(204)
            .with_label(
                Label::new(span).with_message("definitions must have function arguments or be a parser"),
            ),
        ),
        HirConversionError::NonParserDef { span } => Some(
            Report::new(
                DiagnosticKind::Error,
                span.file,
                "Non-parser definition",
            )
            .with_code(205)
            .with_label(
                Label::new(span).with_message("a definition must either be declared with `fun` or be a parser"),
            ),
        ),
        HirConversionError::ParseInNonParserBlock { span } => Some(
            Report::new(
                DiagnosticKind::Error,
                span.file,
                "Parse in non-parser block",
            )
            .with_code(206)
            .with_label(
                Label::new(span).with_message("only parser blocks (`{...}`) can contain parse statements"),
            ),
        ),
        HirConversionError::Silenced => None,
    }
}
