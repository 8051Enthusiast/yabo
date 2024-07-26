pub mod error;
pub mod expr;
pub mod parserdef_ssc;
mod refs;

use std::collections::BTreeSet;
use std::sync::Arc;

use fxhash::FxHashMap;
use hir::ModuleId;
use parserdef_ssc::{mod_parser_ssc, parser_ssc, ModuleOrder};
use petgraph::Graph;

use yaboc_base::error::SResult;
use yaboc_base::error::{Silencable, SilencedError};
use yaboc_base::interner::{DefId, DefinitionPath, FieldName, Identifier, Regex, RegexKind};
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
    fn mod_parser_ssc(&self, module: hir::ModuleId) -> Result<ModuleOrder, ResolveErrors>;
    fn parser_ssc(&self, parser: hir::ParserDefId) -> SResult<FunctionSscId>;
    fn resolve_expr_error(&self, expr_id: hir::ExprId) -> Result<ResolvedExpr, ResolveError>;
    fn resolve_expr(&self, expr_id: hir::ExprId) -> SResult<ResolvedExpr>;
    fn resolve_regex(&self, regex: Regex) -> Result<regex_syntax::hir::Hir, RegexError>;
    fn captures(&self, id: hir::BlockId) -> Arc<BTreeSet<DefId>>;
    fn lambda_captures(&self, id: hir::LambdaId) -> Arc<BTreeSet<DefId>>;
    fn parserdef_ref(&self, loc: DefId, name: Vec<Identifier>)
        -> SResult<Option<hir::ParserDefId>>;
    fn module_sequence(&self) -> Result<Arc<Vec<ModuleId>>, ResolveErrors>;
    fn global_sequence(&self) -> SResult<Arc<[hir::ParserDefId]>>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RegexError {
    Syntax(Box<regex_syntax::Error>),
    Opaque(String),
}

fn resolve_regex(db: &dyn Resolves, regex: Regex) -> Result<regex_syntax::hir::Hir, RegexError> {
    let regex = db.lookup_intern_regex(regex);
    let regex_str = match regex.kind {
        RegexKind::Regular => regex.regex,
        RegexKind::Hexagex => {
            let r =
                hexagex::hexagex(&regex.regex).map_err(|e| RegexError::Opaque(e.to_string()))?;
            r.to_string()
        }
    };
    regex_syntax::ParserBuilder::new()
        .utf8(false)
        .unicode(false)
        .dot_matches_new_line(true)
        .case_insensitive(false)
        .build()
        .parse(&regex_str)
        .map_err(|e| RegexError::Syntax(Box::new(e)))
}

fn module_sequence(db: &dyn Resolves) -> Result<Arc<Vec<ModuleId>>, ResolveErrors> {
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
    if !errors.is_empty() {
        return Err(ResolveErrors(errors.into()));
    }
    let ordered = petgraph::algo::toposort(&graph, None).unwrap();
    let ordered: Vec<_> = ordered
        .iter()
        .map(|index| ModuleId(db.intern_hir_path(DefinitionPath::Module(graph[*index]))))
        .collect();
    Ok(Arc::new(ordered))
}

fn global_sequence(db: &dyn Resolves) -> SResult<Arc<[hir::ParserDefId]>> {
    let mut ret = Vec::new();
    for &module in db.module_sequence().silence()?.iter() {
        ret.extend(db.mod_parser_ssc(module).silence()?.statics.iter().copied());
    }
    Ok(ret.into())
}

fn resolve_expr(db: &dyn Resolves, expr_id: hir::ExprId) -> SResult<ResolvedExpr> {
    db.resolve_expr_error(expr_id).silence()
}

fn expr_captures(
    db: &dyn Resolves,
    expr: ExprId,
    ret: &mut BTreeSet<DefId>,
    id: DefId,
) -> SResult<()> {
    let resolved_expr = db.resolve_expr(expr)?;
    for subexpr in resolved_expr.expr.take_ref().iter_parts() {
        if let ExprHead::Niladic(expr::ResolvedAtom::Captured(capture)) = subexpr {
            ret.insert(capture);
        } else if let ExprHead::Niladic(expr::ResolvedAtom::Block(bid, _)) = subexpr {
            ret.extend(
                db.captures(bid)
                    .iter()
                    .filter(|x| !id.is_ancestor_of(db, **x)),
            );
        } else if let ExprHead::Niladic(expr::ResolvedAtom::Lambda(lid)) = subexpr {
            ret.extend(
                db.lambda_captures(lid)
                    .iter()
                    .filter(|x| !id.is_ancestor_of(db, **x)),
            );
        }
    }
    Ok(())
}

pub fn captures(db: &dyn Resolves, id: hir::BlockId) -> Arc<BTreeSet<DefId>> {
    let mut ret = BTreeSet::new();
    let root_context = match id.lookup(db) {
        Ok(x) => x,
        Err(_) => return Default::default(),
    }
    .root_context;
    for i in ChildIter::new(root_context.0, db)
        .without_kinds(hir::HirNodeKind::Block | hir::HirNodeKind::Lambda)
    {
        if let hir::HirNode::Expr(expr) = i {
            if expr_captures(db, expr.id, &mut ret, id.0).is_err() {
                continue;
            }
        }
    }
    Arc::new(ret)
}

pub fn lambda_captures(db: &dyn Resolves, id: hir::LambdaId) -> Arc<BTreeSet<DefId>> {
    let lambda = match id.lookup(db) {
        Ok(x) => x,
        Err(_) => return Default::default(),
    };
    let mut ret = BTreeSet::new();
    let Ok(()) = expr_captures(db, lambda.expr, &mut ret, id.0) else {
        return Default::default();
    };
    // the arguments are treated as captures, so we need to remove them
    // to get the real captures
    for arg in lambda.args.iter() {
        ret.remove(&arg.0);
    }
    Arc::new(ret)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolveError {
    Unresolved(ExprId, SpanIndex, FieldName),
    CyclicImport(Arc<Vec<FileId>>),
    ModuleInExpression(ExprId, SpanIndex),
    CyclicGlobal(hir::ParserDefId),
    UnorderedSpan(ExprId, SpanIndex),
    NonParserRefInSpan(ExprId, SpanIndex),
    RegexParseError(String, ExprId, SpanIndex, [u32; 2]),
    OtherRegexError(String, ExprId, SpanIndex),
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolveErrors(pub Arc<[ResolveError]>);

impl From<SilencedError> for ResolveErrors {
    fn from(e: SilencedError) -> Self {
        ResolveErrors(Arc::new([e.into()]))
    }
}

impl Silencable for ResolveErrors {
    type Out = SilencedError;

    fn silence(self) -> Self::Out {
        self.0.silence()
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
