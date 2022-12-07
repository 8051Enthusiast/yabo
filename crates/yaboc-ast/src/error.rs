use yaboc_base::error::{
    diagnostic::{DiagnosticKind, Label},
    Report,
};

use super::{convert::GenericParseError, Asts};

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

fn parse_error_report(f: GenericParseError) -> Report {
    Report::new(DiagnosticKind::Error, f.loc.file, "Parse error")
        .with_code(100)
        .with_label(Label::new(f.loc).with_message("error occured here"))
}
