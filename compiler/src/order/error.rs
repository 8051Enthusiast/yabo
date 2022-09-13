use fxhash::FxHashSet;

use crate::{
    error::{
        diagnostic::{DiagnosticKind, Label},
        Report,
    },
    hir,
    source::Span,
};

use super::{Orders, SubValue};

pub fn errors(db: &(impl Orders + ?Sized)) -> Vec<Report> {
    let mut reports = Vec::new();
    for node in hir::walk::ChildIter::new(db.root_id().0, db) {
        let block = match node {
            hir::HirNode::Block(b) => b,
            _ => continue,
        };
        let serial_error = match db.block_serialization(block.id) {
            Err(e) => e,
            Ok(_) => continue,
        };
        for error in serial_error.use_error().iter() {
            reports.extend(block_serialization_error(db, error))
        }
    }
    reports
}

fn block_serialization_error(db: &(impl Orders + ?Sized), error: &[SubValue]) -> Option<Report> {
    if error.is_empty() {
        return None;
    }
    let spans: Vec<Span> = error
        .iter()
        .map(|x| x.id)
        .collect::<FxHashSet<_>>()
        .into_iter()
        .flat_map(|x| hir_span(db, &db.hir_node(x).ok()?))
        .collect();
    let file = if let Some(span) = spans.first() {
        span.file
    } else {
        panic!("found no useful span in cycle error");
    };
    let mut report = Report::new(
        DiagnosticKind::Error,
        file,
        "cycle between the following fields",
    )
    .with_code(401);
    for span in spans {
        report = report.with_label(Label::new(span));
    }
    Some(report)
}

fn hir_span(db: &(impl Orders + ?Sized), node: &hir::HirNode) -> Option<Span> {
    if node.is_kind(hir::HirNodeKind::Expr | hir::HirNodeKind::Choice) {
        return None;
    }
    db.hir_parser_collection(db.hir_parent_parserdef(node.id()).ok()?.0)
        .ok()??
        .default_span(node.id())
}
