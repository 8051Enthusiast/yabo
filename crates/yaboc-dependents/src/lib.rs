pub mod error;
mod represent;

use std::sync::Arc;

use enumflags2::{bitflags, BitFlags};
use yaboc_ast::expr::{ExprIter, ExpressionHead, OpWithData};
use yaboc_base::{
    dbpanic,
    error::{SResult, SilencedError},
    error_type,
    interner::DefId,
};
use yaboc_hir::{self as hir, HirIdWrapper, ParserAtom, ParserPredecessor};
use yaboc_hir_types::TyHirs;
use yaboc_resolve::expr::ResolvedAtom;

use petgraph::{graph::NodeIndex, visit::EdgeRef, Graph};

use fxhash::{FxHashMap, FxHashSet};

#[salsa::query_group(DependentsDatabase)]
pub trait Dependents: TyHirs {
    fn block_serialization(
        &self,
        id: hir::BlockId,
    ) -> Result<BlockSerialization, BlockSerializationError>;
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum SubValueKind {
    Val,
    Front,
    Back,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SubValue {
    pub kind: SubValueKind,
    pub id: DefId,
}

impl SubValue {
    pub fn new(kind: SubValueKind, id: DefId) -> Self {
        Self { kind, id }
    }

    pub fn new_val(id: DefId) -> Self {
        Self {
            kind: SubValueKind::Val,
            id,
        }
    }
    pub fn new_front(id: DefId) -> Self {
        Self {
            kind: SubValueKind::Front,
            id,
        }
    }
    pub fn new_back(id: DefId) -> Self {
        Self {
            kind: SubValueKind::Back,
            id,
        }
    }
}

fn val_refs(
    db: &dyn Dependents,
    node: &hir::HirNode,
    parent_block: hir::BlockId,
) -> SResult<FxHashSet<(SubValue, bool)>> {
    match node {
        hir::HirNode::Let(l) => {
            let mut ret = FxHashSet::default();
            ret.insert((SubValue::new_val(l.expr.0), true));
            Ok(ret)
        }
        hir::HirNode::Expr(expr) => {
            let mut ret = FxHashSet::default();
            for e in ExprIter::new(&expr.expr) {
                match e.0 {
                    ExpressionHead::Niladic(OpWithData {
                        inner: ParserAtom::Block(b),
                        ..
                    }) => ret.extend(
                        db.captures(b)
                            .iter()
                            .copied()
                            .filter(|target| parent_block.0.is_ancestor_of(db, *target))
                            .map(|target| (SubValue::new_val(target), true)),
                    ),
                    _ => continue,
                };
            }
            let rexpr = db.resolve_expr(expr.id)?;
            ret.extend(ExprIter::new(&rexpr).filter_map(|x| match x.0 {
                ExpressionHead::Niladic(OpWithData {
                    inner: ResolvedAtom::Val(v),
                    ..
                }) => Some((SubValue::new_val(v), true)),
                _ => None,
            }));
            Ok(ret)
        }
        hir::HirNode::Parse(p) => {
            let mut ret = FxHashSet::default();
            ret.insert((SubValue::new_val(p.expr.0), true));
            ret.insert((SubValue::new_front(p.id.id()), true));
            ret.insert((SubValue::new_back(p.id.id()), false));
            Ok(ret)
        }
        hir::HirNode::Block(block) => Ok(block
            .root_context
            .lookup(db)?
            .vars
            .iter()
            .map(|(_, id)| (SubValue::new_val(*id.inner()), true))
            .collect()),
        hir::HirNode::Choice(choice) => {
            let mut ret = FxHashSet::default();
            for ctx in choice.subcontexts.iter() {
                let context = ctx.lookup(db)?;
                ret.extend(
                    context
                        .children
                        .into_iter()
                        .map(|c| (SubValue::new_val(c), false)),
                );
                if let Some((_, end)) = context.endpoints {
                    ret.insert((SubValue::new_back(end), false));
                }
            }
            Ok(ret)
        }
        hir::HirNode::ChoiceIndirection(ind) => Ok(ind
            .choices
            .iter()
            .map(|(_, id)| (SubValue::new_val(*id), true))
            .chain(std::iter::once((
                SubValue::new_val(ind.target_choice.id()),
                true,
            )))
            .collect()),
        hir::HirNode::TExpr(_) => Ok(FxHashSet::default()),
        hir::HirNode::Array(_) => todo!(),
        hir::HirNode::ArgDef(_)
        | hir::HirNode::Module(_)
        | hir::HirNode::ParserDef(_)
        | hir::HirNode::Import(_)
        | hir::HirNode::Context(_) => {
            panic!(
                "attempting to get value references of invalid node {:?}",
                node
            )
        }
    }
}

fn pred(node: &hir::HirNode) -> Option<[ParserPredecessor; 2]> {
    match node {
        hir::HirNode::Choice(c) => c.endpoints,
        hir::HirNode::Parse(c) => Some([c.front, c.back]),
        _ => None,
    }
}

fn between_parser_refs(
    db: &dyn Dependents,
    node: &hir::HirNode,
    parent_block: hir::BlockId,
) -> SResult<Option<SubValue>> {
    let [pred, _] = match pred(node) {
        Some(p) => p,
        None => return Ok(None),
    };
    Ok(Some(match pred {
        hir::ParserPredecessor::ChildOf(ctx) => SubValue::new_front(
            ctx.lookup(db)?
                .parent_choice
                .map_or(parent_block.id(), |c| c.id()),
        ),
        hir::ParserPredecessor::After(pred) => SubValue::new_back(pred),
    }))
}

fn inner_parser_refs(db: &dyn Dependents, node: &hir::HirNode) -> SResult<FxHashSet<SubValue>> {
    match node {
        hir::HirNode::Parse(p) => {
            let mut ret = FxHashSet::default();
            ret.insert(SubValue::new_val(p.expr.0));
            ret.insert(SubValue::new_front(p.id.id()));
            Ok(ret)
        }
        hir::HirNode::Choice(c) if c.endpoints.is_none() => Ok(FxHashSet::default()),
        hir::HirNode::Choice(c) => {
            let mut ret = FxHashSet::default();
            for ctx in c.subcontexts.iter() {
                if let Some((_, back)) = ctx.lookup(db)?.endpoints {
                    ret.insert(SubValue::new_back(back));
                }
            }
            ret.insert(SubValue::new_val(c.id.0));
            Ok(ret)
        }
        hir::HirNode::Block(b) => {
            let mut ret = FxHashSet::default();
            ret.insert(match b.root_context.lookup(db)?.endpoints {
                Some((_, back)) => SubValue::new_back(back),
                None => SubValue::new_front(b.id.0),
            });
            Ok(ret)
        }
        _ => Ok(FxHashSet::default()),
    }
}

fn node_subvalue_kinds(node: &hir::HirNode) -> &'static [SubValueKind] {
    match node {
        hir::HirNode::Choice(c) if c.endpoints.is_none() => &[SubValueKind::Val][..],
        hir::HirNode::Parse(_) | hir::HirNode::Block(_) | hir::HirNode::Choice(_) => {
            &[SubValueKind::Front, SubValueKind::Back, SubValueKind::Val][..]
        }
        hir::HirNode::Let(_)
        | hir::HirNode::Expr(_)
        | hir::HirNode::ChoiceIndirection(_)
        | hir::HirNode::ArgDef(_) => &[SubValueKind::Val][..],
        _ => &[],
    }
}

pub struct DependencyGraph {
    val_map: FxHashMap<SubValue, NodeIndex>,
    graph: Graph<SubValue, bool>,
}

impl DependencyGraph {
    fn init_nodes(&mut self, db: &dyn Dependents, block: hir::BlockId) -> SResult<()> {
        for hir_node in hir::walk::ChildIter::new(block.lookup(db)?.root_context.0, db)
            .without_kinds(hir::HirNodeKind::Block)
            .chain(std::iter::once(db.hir_node(block.id())?))
        {
            match hir_node {
                hir::HirNode::Block(b) if b.id != block => continue,
                _ => {}
            }
            let kinds = node_subvalue_kinds(&hir_node);
            for kind in kinds {
                let sub_value = SubValue::new(*kind, hir_node.id());
                self.val_map
                    .insert(sub_value, self.graph.add_node(sub_value));
            }
        }
        Ok(())
    }

    fn add_edges(
        &mut self,
        db: &dyn Dependents,
        from: SubValue,
        to: impl IntoIterator<Item = (SubValue, bool)>,
    ) {
        let from_idx = self.val_map[&from];
        for (sub_value, is_data) in to {
            let to_idx = *self.val_map.get(&sub_value).unwrap_or_else(|| {
                dbpanic!(db, "subvalue {} not found", &sub_value);
            });
            self.graph.add_edge(from_idx, to_idx, is_data);
        }
    }

    fn init_edges(&mut self, db: &dyn Dependents, block: hir::BlockId) -> SResult<()> {
        for &sub_value in self.val_map.clone().keys() {
            let hir_node = db.hir_node(sub_value.id)?;
            match sub_value.kind {
                SubValueKind::Val => {
                    self.add_edges(db, sub_value, val_refs(db, &hir_node, block)?);
                }
                SubValueKind::Front => self.add_edges(
                    db,
                    sub_value,
                    between_parser_refs(db, &hir_node, block)?.map(|x| (x, true)),
                ),
                SubValueKind::Back => {
                    self.add_edges(
                        db,
                        sub_value,
                        inner_parser_refs(db, &hir_node)?
                            .into_iter()
                            .map(|x| (x, true)),
                    );
                }
            };
        }
        Ok(())
    }

    pub fn find_descendants(&self, sub_values: &[SubValue]) -> FxHashSet<SubValue> {
        let mut visited = FxHashSet::default();
        let mut stack = Vec::new();
        for &sub_value in sub_values {
            stack.push(sub_value);
        }
        while let Some(sub_value) = stack.pop() {
            if !visited.insert(sub_value) {
                continue;
            }
            for edge in self.graph.edges(self.val_map[&sub_value]) {
                if *edge.weight() {
                    stack.push(self.graph[edge.target()]);
                }
            }
        }
        visited
    }

    pub fn new(db: &dyn Dependents, block: hir::BlockId) -> SResult<Self> {
        let mut ret = Self {
            val_map: FxHashMap::default(),
            graph: Graph::new(),
        };
        ret.init_nodes(db, block)?;
        ret.init_edges(db, block)?;
        Ok(ret)
    }
}

#[bitflags]
#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum NeededBy {
    Len = 1 << 0,
    Val = 1 << 1,
    Backtrack = 1 << 2,
}

pub type RequirementSet = BitFlags<NeededBy>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct BlockSlot(u32);

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct SubValueInfo {
    pub val: SubValue,
    pub requirements: RequirementSet,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BlockSerialization {
    pub eval_order: Arc<Vec<SubValueInfo>>,
}

error_type!(BlockSerializationError(Arc<Vec<Vec<SubValue>>>) in self);

impl From<SilencedError> for BlockSerializationError {
    fn from(_: SilencedError) -> Self {
        BlockSerializationError {
            inner: Default::default(),
        }
    }
}

pub fn block_serialization(
    db: &dyn Dependents,
    block: hir::BlockId,
) -> Result<BlockSerialization, BlockSerializationError> {
    let graph = match DependencyGraph::new(db, block) {
        Ok(g) => g,
        Err(e) => return Err(e.into()),
    };

    let reachable_block_val = graph.find_descendants(&[SubValue::new_val(block.id())]);
    let reachable_block_back = graph.find_descendants(&[SubValue::new_back(block.id())]);

    let condensed_graph = petgraph::algo::condensation(graph.graph, false);
    let mut errors = Vec::new();
    for node in condensed_graph.node_indices() {
        if condensed_graph.contains_edge(node, node) {
            errors.push(condensed_graph.node_weight(node).unwrap().clone());
        }
    }
    if !errors.is_empty() {
        return Err(BlockSerializationError {
            inner: Arc::new(errors),
        });
    }
    let mut eval_order = Vec::new();
    for node in petgraph::algo::toposort(&condensed_graph, None)
        .unwrap()
        .into_iter()
    {
        let nodes = condensed_graph.node_weight(node).unwrap();
        eval_order.extend(nodes.iter().copied().map(|val| {
            let mut requirements = RequirementSet::empty();
            if reachable_block_val.contains(&val) {
                requirements |= NeededBy::Val;
            }
            if reachable_block_back.contains(&val) {
                requirements |= NeededBy::Len;
            }
            // TODO(8051): we don't know what nodes are needed right now, so we just assume everything
            // is needed.
            requirements |= NeededBy::Backtrack;
            SubValueInfo { val, requirements }
        }));
    }
    eval_order.reverse();
    Ok(BlockSerialization {
        eval_order: Arc::new(eval_order),
    })
}

#[cfg(test)]
mod tests {
    use hir::{HirDatabase, Parser};
    use yaboc_ast::{import::Import, AstDatabase};
    use yaboc_base::{
        config::ConfigDatabase, interner::InternerDatabase, source::FileDatabase, Context,
    };
    use yaboc_hir_types::{HirTypesDatabase, NominalId};
    use yaboc_resolve::ResolveDatabase;
    use yaboc_types::{TypeInterner, TypeInternerDatabase};

    use super::*;
    #[salsa::database(
        InternerDatabase,
        ConfigDatabase,
        AstDatabase,
        FileDatabase,
        HirDatabase,
        ResolveDatabase,
        TypeInternerDatabase,
        HirTypesDatabase,
        DependentsDatabase
    )]
    #[derive(Default)]
    pub struct DependentsTestDatabase {
        storage: salsa::Storage<DependentsTestDatabase>,
    }

    impl salsa::Database for DependentsTestDatabase {}
    #[test]
    fn simple_cycle() {
        let ctx = Context::<DependentsTestDatabase>::mock(
            r#"
def for[u8] *> cycle = {
    let x: int = y
    let y: int = x + 1
}
        "#,
        );
        assert!(!error::errors(&ctx.db).is_empty());
    }
    #[test]
    fn parser_cycle() {
        let ctx = Context::<DependentsTestDatabase>::mock(
            r#"
def for[u8] *> cycle = {
    | let x: int = y
    | x: ~
    y: ~
}
        "#,
        );
        assert!(!error::errors(&ctx.db).is_empty());
    }
    #[test]
    fn choice_indirection() {
        let ctx = Context::<DependentsTestDatabase>::mock(
            r#"
def for[int] *> main = {
    | y: ~
    | let y: int = 0
}
        "#,
        );
        let main = ctx.parser("main");
        let block = match ctx
            .db
            .lookup_intern_type(ctx.db.parser_returns(main).unwrap().deref)
        {
            yaboc_types::Type::Nominal(n) => match NominalId::from_nominal_head(&n) {
                NominalId::Block(b) => b,
                NominalId::Def(_) => panic!(),
            },
            _ => panic!(),
        };
        let _ = ctx.db.block_serialization(block).unwrap();
        // TODO(8051): write actual test
    }
    #[test]
    fn inner_choice() {
        let ctx = Context::<DependentsTestDatabase>::mock(
            r"
def for['a] *> first = ~
def for['b] *> second = ~
def for[int] *> main = {
    a: ~
    b: ~
    c: {
        | c: first
        | c: second
    }
}
            ",
        );
        let main = ctx.parser("main");
        let block = match ctx
            .db
            .lookup_intern_type(ctx.db.parser_returns(main).unwrap().deref)
        {
            yaboc_types::Type::Nominal(n) => match NominalId::from_nominal_head(&n) {
                NominalId::Block(b) => b,
                NominalId::Def(_) => panic!(),
            },
            _ => panic!(),
        };
        let _ = ctx.db.block_serialization(block).unwrap();
    }
}
