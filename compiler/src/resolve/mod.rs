pub mod error;
pub mod expr;
pub mod parserdef_ssc;
mod refs;

use std::collections::BTreeSet;
use std::{collections::BTreeMap, sync::Arc};

use parserdef_ssc::{mod_parser_ssc, parser_ssc};

use crate::error::{Silencable, SilencedError};
use crate::expr::{ExprIter, ExpressionHead, OpWithData};
use crate::hir::walk::ChildIter;
use crate::hir::{ExprId, HirIdWrapper};
use crate::interner::{DefId, FieldName, Identifier};
use crate::source::SpanIndex;
use crate::{error::SResult, hir};

use self::expr::resolve_expr_error;
pub use self::expr::ResolvedExpr;
use self::parserdef_ssc::FunctionSscId;
use self::refs::parserdef_ref;

#[salsa::query_group(ResolveDatabase)]
pub trait Resolves: crate::hir::Hirs {
    #[salsa::interned]
    fn intern_recursion_scc(&self, functions: Vec<hir::ParserDefId>) -> FunctionSscId;
    fn mod_parser_ssc(
        &self,
        module: hir::ModuleId,
    ) -> SResult<Arc<BTreeMap<hir::ParserDefId, FunctionSscId>>>;
    fn parser_ssc(&self, parser: hir::ParserDefId) -> SResult<FunctionSscId>;
    fn resolve_expr_error(&self, expr_id: hir::ExprId) -> Result<Arc<ResolvedExpr>, ResolveError>;
    fn resolve_expr(&self, expr_id: hir::ExprId) -> SResult<Arc<ResolvedExpr>>;
    fn captures(&self, id: hir::BlockId) -> Arc<BTreeSet<DefId>>;
    fn parserdef_ref(&self, loc: DefId, name: Identifier) -> SResult<Option<hir::ParserDefId>>;
}

fn resolve_expr(db: &dyn Resolves, expr_id: hir::ExprId) -> SResult<Arc<ResolvedExpr>> {
    db.resolve_expr_error(expr_id).silence()
}

pub fn captures(db: &dyn Resolves, id: hir::BlockId) -> Arc<BTreeSet<DefId>> {
    let mut ret = BTreeSet::new();
    let root_context = match id.lookup(db) {
        Ok(x) => x,
        Err(_) => return Default::default(),
    }
    .root_context;
    for i in ChildIter::new(root_context.0, db).without_kinds(hir::HirNodeKind::Block) {
        if let hir::HirNode::Expr(expr) = i {
            let resolved_expr = if let Ok(x) = db.resolve_expr(expr.id) {
                x
            } else {
                continue;
            };
            ret.extend(ExprIter::new(&resolved_expr).filter_map(|subexpr| {
                if let ExpressionHead::Niladic(OpWithData {
                    inner: expr::ResolvedAtom::Captured(capture),
                    ..
                }) = subexpr.0
                {
                    Some(capture)
                } else {
                    None
                }
            }));
        }
    }
    Arc::new(ret)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolveError {
    Unresolved(ExprId, SpanIndex, FieldName),
    Silenced,
}

impl From<SilencedError> for ResolveError {
    fn from(_: SilencedError) -> Self {
        ResolveError::Silenced
    }
}

impl Silencable for ResolveError {
    type Out = SilencedError;

    fn silence(self) -> Self::Out {
        SilencedError
    }
}

#[cfg(test)]
mod tests {
    use crate::{context::Context, resolve::Resolves};

    #[test]
    fn recursion_ssc() {
        let ctx = Context::mock(
            r#"
def for [u8] *> a = {x: c, y: {b, z: d}}
def for [u8] *> b = {x: a, y: c}
def for [u8] *> c = {x: c}
def for [u8] *> d = {let a: u64 = 1, let b: u64 = a + 1}
def for [u8] *> e = {}
            "#,
        );
        let a = ctx.parser("a");
        let b = ctx.parser("b");
        let c = ctx.parser("c");
        let d = ctx.parser("d");
        let e = ctx.parser("d");
        let get_ssc = |x| ctx.db.parser_ssc(x).unwrap();
        let ssc_a = get_ssc(a);
        let ssc_b = get_ssc(b);
        let ssc_c = get_ssc(c);
        let ssc_d = get_ssc(d);
        let ssc_e = get_ssc(e);
        assert!(ssc_a == ssc_b);
        assert!(ssc_b != ssc_c);
        assert!(ssc_c != ssc_d);
        assert!(ssc_b != ssc_d);
        assert!(ssc_b != ssc_e);
    }
}
