use ariadne::{Label, ReportKind};

use crate::{
    dbformat,
    error::Report,
    hir::{self, walk::ChildIter, ExprId},
    source::{Span, SpanIndex},
};

use super::{ResolveError, Resolves};

pub fn errors(db: &(impl Resolves + ?Sized)) -> Vec<Report> {
    let mut errors = Vec::new();
    for node in ChildIter::new(db.root_id().0, db) {
        // let chains would make this so much prettier...
        if let hir::HirNode::Expr(expr) = &node {
            if let Err(e) = db.resolve_expr_error(expr.id) {
                if let Some(rep) = make_report(db, &e) {
                    errors.push(rep);
                }
            }
        }
    }
    errors
}

fn make_report(db: &(impl Resolves + ?Sized), err: &ResolveError) -> Option<Report> {
    match err {
        ResolveError::Unresolved(expr, span, field) => {
            let span = hir_span(db, *expr, *span).expect("could not find span for unresolved expr");
            let file = span.file;
            let error_msg = dbformat!(db, "could not resolve name {}", field);
            let report = Report::build(ReportKind::Error, file, 0)
                .with_code(301)
                .with_message(error_msg)
                .with_label(Label::new(span).with_message("not found here"))
                .finish();
            Some(report)
        }
        ResolveError::Silenced => None,
    }
}

fn hir_span(db: &(impl Resolves + ?Sized), expr: ExprId, span: SpanIndex) -> Option<Span> {
    db.hir_parser_collection(db.hir_parent_parserdef(expr.0).ok()?.0)
        .ok()??
        .span_with_index(expr.0, span.as_usize())
}
