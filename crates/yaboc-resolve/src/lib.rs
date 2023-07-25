pub mod error;
pub mod expr;
pub mod parserdef_ssc;
mod refs;

use std::collections::BTreeSet;
use std::{collections::BTreeMap, sync::Arc};

use fxhash::FxHashMap;
use parserdef_ssc::{mod_parser_ssc, parser_ssc};
use petgraph::Graph;

use yaboc_base::error::SResult;
use yaboc_base::error::{Silencable, SilencedError};
use yaboc_base::interner::{DefId, DefinitionPath, FieldName, Identifier};
use yaboc_base::source::{FileId, SpanIndex};
use yaboc_expr::{ExprHead, Expression, TakeRef};
use yaboc_hir::walk::ChildIter;
use yaboc_hir::{self as hir, ExprId, HirIdWrapper};

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
    fn resolve_expr_error(&self, expr_id: hir::ExprId) -> Result<ResolvedExpr, ResolveError>;
    fn resolve_expr(&self, expr_id: hir::ExprId) -> SResult<ResolvedExpr>;
    fn captures(&self, id: hir::BlockId) -> Arc<BTreeSet<DefId>>;
    fn parserdef_ref(&self, loc: DefId, name: Vec<Identifier>)
        -> SResult<Option<hir::ParserDefId>>;
    fn cyclic_import(&self) -> Option<Arc<Vec<ResolveError>>>;
}

fn cyclic_import(db: &dyn Resolves) -> Option<Arc<Vec<ResolveError>>> {
    let mut graph = Graph::new();
    let mut index_map = FxHashMap::default();
    for module in db.all_modules() {
        let DefinitionPath::Module(file) = db.lookup_intern_hir_path(module.0) else {
            panic!("Module {:?} is not a file", module);
        };
        let index = graph.add_node(file);
        index_map.insert(file, index);
    }
    for module in db.all_modules() {
        let DefinitionPath::Module(file) = db.lookup_intern_hir_path(module.0) else {
            panic!("Module {:?} is not a file", module);
        };
        let from = index_map[&file];
        let Ok(imports) = db.imports(file) else {
            continue;
        };
        for import_name in imports.iter() {
            let Ok(to) = db.import_id(file, import_name.inner) else {
                continue;
            };
            let to = index_map[&to];
            graph.add_edge(from, to, ());
        }
    }
    let sscs = petgraph::algo::kosaraju_scc(&graph);
    let mut errors = Vec::new();
    for ssc in sscs {
        let contains_cycle = ssc.len() > 1 || graph.contains_edge(ssc[0], ssc[0]);
        if !contains_cycle {
            continue;
        }
        let files: Vec<_> = ssc.iter().map(|index| graph[*index]).collect();
        errors.push(ResolveError::CyclicImport(Arc::new(files)));
    }
    if errors.is_empty() {
        None
    } else {
        Some(Arc::new(errors))
    }
}

fn resolve_expr(db: &dyn Resolves, expr_id: hir::ExprId) -> SResult<ResolvedExpr> {
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
            ret.extend(
                resolved_expr
                    .expr
                    .take_ref()
                    .iter_parts()
                    .filter_map(|subexpr| {
                        if let ExprHead::Niladic(expr::ResolvedAtom::Captured(capture, _)) = subexpr
                        {
                            Some(capture)
                        } else {
                            None
                        }
                    }),
            );
        }
    }
    Arc::new(ret)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolveError {
    Unresolved(ExprId, SpanIndex, FieldName),
    CyclicImport(Arc<Vec<FileId>>),
    ModuleInExpression(ExprId, SpanIndex),
    Silenced(SilencedError),
}

impl From<SilencedError> for ResolveError {
    fn from(e: SilencedError) -> Self {
        ResolveError::Silenced(e)
    }
}

impl Silencable for ResolveError {
    type Out = SilencedError;

    fn silence(self) -> Self::Out {
        match self {
            ResolveError::Silenced(e) => e,
            _ => SilencedError::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hir::Parser;
    use yaboc_ast::{import::Import, AstDatabase};
    use yaboc_base::{
        config::ConfigDatabase, interner::InternerDatabase, source::FileDatabase, Context,
    };
    use yaboc_hir::HirDatabase;
    #[salsa::database(
        InternerDatabase,
        ConfigDatabase,
        AstDatabase,
        FileDatabase,
        HirDatabase,
        ResolveDatabase
    )]
    #[derive(Default)]
    pub struct ResolveTestDatabase {
        storage: salsa::Storage<ResolveTestDatabase>,
    }

    impl salsa::Database for ResolveTestDatabase {}

    #[test]
    fn recursion_ssc() {
        let ctx = Context::<ResolveTestDatabase>::mock(
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
