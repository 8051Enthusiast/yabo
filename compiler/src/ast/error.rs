use ariadne::{Label, ReportKind};

use crate::{error::Report, source::FileId};

use super::{convert::GenericParseError, Asts};

pub fn errors(db: &(impl Asts + ?Sized)) -> Vec<Report> {
    let fid = FileId::default();
    let errors = match db.ast(fid) {
        Ok(_) => return vec![],
        Err(e) => e,
    };
    errors.into_iter().map(|x| parse_error_report(x)).collect()
}

fn parse_error_report(f: GenericParseError) -> Report {
    Report::build(ReportKind::Error, f.loc.file, 0)
        .with_code(100)
        .with_message("Parse error")
        .with_label(Label::new(f.loc).with_message("error occured here"))
        .finish()
}
