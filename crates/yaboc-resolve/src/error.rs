use yaboc_base::{
    dbformat,
    error::{
        diagnostic::{DiagnosticKind, Label},
        Report,
    },
    source::{Span, SpanIndex},
};
use yaboc_hir::{self as hir, walk::ChildIter, ExprId};

use super::{ResolveError, Resolves};

pub fn errors(db: &(impl Resolves + ?Sized)) -> Vec<Report> {
    let mut errors = Vec::new();
    for module in db.all_modules() {
        for node in ChildIter::new(module.0, db) {
            // let chains would make this so much prettier...
            if let hir::HirNode::Expr(expr) = &node {
                if let Err(e) = db.resolve_expr_error(expr.id) {
                    if let Some(rep) = make_report(db, &e) {
                        errors.push(rep);
                    }
                }
            }
        }
    }
    errors.extend(
        db.cyclic_import()
            .iter()
            .flat_map(|x| x.iter())
            .flat_map(|x| make_report(db, x)),
    );
    errors
}

fn make_report(db: &(impl Resolves + ?Sized), err: &ResolveError) -> Option<Report> {
    match err {
        ResolveError::Unresolved(expr, span, field) => {
            let span = hir_span(db, *expr, *span).expect("could not find span for unresolved expr");
            let file = span.file;
            let error_msg = dbformat!(db, "could not resolve name {}", field);
            let report = Report::new(DiagnosticKind::Error, file, &error_msg)
                .with_code(301)
                .with_label(Label::new(span).with_message("not found here"));
            Some(report)
        }
        ResolveError::CyclicImport(files) => {
            let filenames = files
                .iter()
                .map(|file| db.path(*file).unwrap_or_else(|| "<anon>".to_string()))
                .collect::<Vec<_>>();
            let list = filenames.join("\n");
            let example_file = files[0];
            let report = Report::new(
                DiagnosticKind::Error,
                example_file,
                &format!("cyclic import between following files:\n{list}"),
            )
            .with_code(302);
            Some(report)
        }
        ResolveError::ModuleInExpression(expr, span) => {
            let span = hir_span(db, *expr, *span).expect("could not find span for unresolved expr");
            let file = span.file;
            let report = Report::new(
                DiagnosticKind::Error,
                file,
                "modules cannot be used as values in expressions",
            )
            .with_code(303)
            .with_label(Label::new(span).with_message("module used here"));
            Some(report)
        }
        ResolveError::Silenced(_) => None,
    }
}

fn hir_span(db: &(impl Resolves + ?Sized), expr: ExprId, span: SpanIndex) -> Option<Span> {
    db.hir_parser_collection(db.hir_parent_parserdef(expr.0).ok()?.0)
        .ok()??
        .span_with_index(expr.0, span.as_usize())
}
