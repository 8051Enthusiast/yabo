use crate::error::Report;
use ariadne::{Label, ReportKind};

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
    let (first, duplicate) = match error {
        HirConversionError::DuplicateField {
            first, duplicate, ..
        } => (first, duplicate),
        HirConversionError::Silenced => return None,
    };
    Some(
        Report::build(ReportKind::Error, duplicate.file, 0)
            .with_code(2)
            .with_message("Duplicate field in block")
            .with_label(
                Label::new(duplicate).with_message("This field already appeared in this block"),
            )
            .with_label(Label::new(first).with_message("the previous appearnce of the field"))
            .with_note("Each field name may only appear at most once in a block")
            .finish(),
    )
}
