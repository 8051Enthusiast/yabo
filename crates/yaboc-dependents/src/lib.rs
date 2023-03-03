mod backtrack;
pub mod error;
mod represent;

use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use enumflags2::{bitflags, BitFlags};
use yaboc_base::{
    dbpanic,
    error::{SResult, SilencedError},
    error_type,
    interner::{DefId, FieldName},
};
use yaboc_expr::{ExprHead, Expression, FetchExpr, TakeRef};
use yaboc_hir::{self as hir, HirIdWrapper, ParserAtom, ParserPredecessor};
use yaboc_hir_types::TyHirs;
use yaboc_resolve::expr::{Resolved, ResolvedAtom};

use petgraph::{graph::NodeIndex, visit::EdgeRef, Direction, Graph};

use backtrack::can_backtrack;
use fxhash::{FxHashMap, FxHashSet};

pub use represent::dependency_dot;

#[salsa::query_group(DependentsDatabase)]
pub trait Dependents: TyHirs {
    fn can_backtrack(&self, def: DefId) -> SResult<bool>;
    fn block_serialization(
        &self,
        id: hir::BlockId,
    ) -> Result<BlockSerialization, BlockSerializationError>;
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum SubValueKind {
    Front,
    Bt,
    Back,
    Val,
}

impl SubValueKind {
    fn all() -> [Self; 4] {
        [
            SubValueKind::Front,
            SubValueKind::Bt,
            SubValueKind::Back,
            SubValueKind::Val,
        ]
    }
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
    pub fn new_bt(id: DefId) -> Self {
        Self {
            kind: SubValueKind::Bt,
            id,
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct SubValuePrio {
    pub tail: bool,
    pub backtrack: bool,
    pub subvalue: SubValue,
}

impl Ord for SubValuePrio {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.tail
            .cmp(&other.tail)
            .then(self.backtrack.cmp(&other.backtrack).reverse())
            .then(self.subvalue.cmp(&other.subvalue))
    }
}

impl PartialOrd for SubValuePrio {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
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
            for (e, _) in expr.expr.asref().iter_parts() {
                match e {
                    ExprHead::Niladic(ParserAtom::Block(b)) => ret.extend(
                        db.captures(b)
                            .iter()
                            .copied()
                            .filter(|target| parent_block.0.is_ancestor_of(db, *target))
                            .map(|target| (SubValue::new_val(target), true)),
                    ),
                    _ => continue,
                };
            }
            let rexpr = Resolved::fetch_expr(db, expr.id)?;
            ret.extend(rexpr.take_ref().iter_parts().filter_map(|x| match x {
                ExprHead::Niladic(ResolvedAtom::Val(v, _)) => Some((SubValue::new_val(v), true)),
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
            for (i, ctx) in choice.subcontexts.iter().enumerate() {
                let is_last = i == choice.subcontexts.len() - 1;
                let context = ctx.lookup(db)?;
                add_ctx_bt(&context, db, |v| {
                    ret.insert((v, !is_last));
                })?;
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
            panic!("attempting to get value references of invalid node {node:?}")
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

fn add_ctx_bt(
    root_ctx: &hir::StructCtx,
    db: &dyn Dependents,
    mut add: impl FnMut(SubValue),
) -> Result<(), SilencedError> {
    for child in root_ctx.children.iter() {
        if db.can_backtrack(*child)? {
            add(SubValue::new_bt(*child));
        }
    }
    Ok(())
}

fn bt_refs(db: &dyn Dependents, node: &hir::HirNode) -> SResult<FxHashSet<SubValue>> {
    let mut ret = FxHashSet::default();
    match node {
        hir::HirNode::Let(l) => {
            ret.insert(SubValue::new_bt(l.expr.id()));
        }
        hir::HirNode::Expr(e) => {
            ret.insert(SubValue::new_val(e.id.id()));
        }
        hir::HirNode::Parse(p) => {
            ret.insert(SubValue::new_bt(p.expr.id()));
            ret.insert(SubValue::new_front(p.id.id()));
            ret.insert(SubValue::new_val(p.expr.id()));
        }
        hir::HirNode::Block(b) => {
            let root_ctx = b.root_context.lookup(db)?;
            add_ctx_bt(&root_ctx, db, |v| {
                ret.insert(v);
            })?;
        }
        hir::HirNode::Choice(c) => {
            let last_ctx = c.subcontexts.last().unwrap().lookup(db)?;
            ret.insert(SubValue::new_val(c.id.id()));
            add_ctx_bt(&last_ctx, db, |v| {
                ret.insert(v);
            })?;
        }
        hir::HirNode::Import(_)
        | hir::HirNode::ArgDef(_)
        | hir::HirNode::Module(_)
        | hir::HirNode::Context(_)
        | hir::HirNode::ParserDef(_)
        | hir::HirNode::ChoiceIndirection(_)
        | hir::HirNode::TExpr(_)
        | hir::HirNode::Array(_) => {}
    }
    Ok(ret)
}

fn node_subvalue_kinds(node: &hir::HirNode) -> &'static [SubValueKind] {
    match node {
        hir::HirNode::Choice(c) if c.endpoints.is_none() => {
            &[SubValueKind::Val, SubValueKind::Bt][..]
        }
        hir::HirNode::Parse(_) | hir::HirNode::Block(_) | hir::HirNode::Choice(_) => &[
            SubValueKind::Front,
            SubValueKind::Back,
            SubValueKind::Val,
            SubValueKind::Bt,
        ][..],
        hir::HirNode::Let(_) | hir::HirNode::Expr(_) => &[SubValueKind::Val, SubValueKind::Bt][..],
        hir::HirNode::ChoiceIndirection(_) | hir::HirNode::ArgDef(_) => &[SubValueKind::Val][..],
        _ => &[],
    }
}

pub struct DependencyGraph {
    block: hir::BlockId,
    val_map: FxHashMap<SubValue, NodeIndex>,
    graph: Graph<SubValue, bool>,
}

impl DependencyGraph {
    pub fn new(db: &dyn Dependents, block: hir::BlockId) -> SResult<Self> {
        let mut ret = Self {
            block,
            val_map: FxHashMap::default(),
            graph: Graph::new(),
        };
        ret.init_nodes(db)?;
        ret.init_edges(db)?;
        Ok(ret)
    }

    fn init_nodes(&mut self, db: &dyn Dependents) -> SResult<()> {
        for hir_node in hir::walk::ChildIter::new(self.block.lookup(db)?.root_context.0, db)
            .without_kinds(hir::HirNodeKind::Block)
            .chain(std::iter::once(db.hir_node(self.block.id())?))
        {
            match hir_node {
                hir::HirNode::Block(b) if b.id != self.block => continue,
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

    fn init_edges(&mut self, db: &dyn Dependents) -> SResult<()> {
        for &sub_value in self.val_map.clone().keys() {
            let hir_node = db.hir_node(sub_value.id)?;
            match sub_value.kind {
                SubValueKind::Val => {
                    self.add_edges(db, sub_value, val_refs(db, &hir_node, self.block)?)
                }
                SubValueKind::Front => self.add_edges(
                    db,
                    sub_value,
                    between_parser_refs(db, &hir_node, self.block)?.map(|x| (x, true)),
                ),
                SubValueKind::Back => self.add_edges(
                    db,
                    sub_value,
                    inner_parser_refs(db, &hir_node)?
                        .into_iter()
                        .map(|x| (x, true)),
                ),
                SubValueKind::Bt => self.add_edges(
                    db,
                    sub_value,
                    bt_refs(db, &hir_node)?.into_iter().map(|x| (x, true)),
                ),
            };
        }
        Ok(())
    }

    pub fn find_reachable(&self, sub_values: &[SubValue]) -> FxHashSet<SubValue> {
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

    fn id_is_tail(&self, id: DefId, existing: &FxHashSet<DefId>) -> Result<bool, SilencedError> {
        // this is art, don't touch
        for kind in SubValueKind::all() {
            let subvalue = SubValue::new(kind, id);
            let Some(&idx) = self.val_map.get(&subvalue) else { continue };
            for nidx in self.graph.neighbors_directed(idx, Direction::Incoming) {
                let neighbor_val = self.graph[nidx];
                let edge_idx = self.graph.find_edge(nidx, idx).unwrap();
                let true = self.graph.edge_weight(edge_idx).unwrap() else { continue };
                let false = existing.contains(&neighbor_val.id) else { continue };
                let false = neighbor_val.id == self.block.0 else { continue };
                let false = neighbor_val.id == id else { continue };
                let SubValueKind::Back = subvalue.kind else { return Ok(false) };
                let SubValueKind::Back = neighbor_val.kind else { return Ok(false) };
            }
        }
        Ok(true)
    }

    pub fn tail_returns(&self, db: &(impl Dependents + ?Sized)) -> SResult<FxHashSet<DefId>> {
        let block = self.block.lookup(db)?;
        let mut ret = FxHashSet::default();
        if !block.returns {
            return Ok(ret);
        }
        let root_ctx = block.root_context.lookup(db)?;
        let mut return_id_stack = vec![*root_ctx.vars.set[&FieldName::Return].inner()];
        'outer: while let Some(id) = return_id_stack.pop() {
            let true = self.id_is_tail(id, &ret)? else { continue };

            ret.insert(id);

            let hir::HirNode::ChoiceIndirection(c) = db.hir_node(id)? else {
                continue
            };
            let choice = c.target_choice.lookup(db)?;
            if let Some([_, back]) = choice.endpoints {
                let ParserPredecessor::ChildOf(_) = back else { continue };
            }
            let ctx = choice.parent_context.lookup(db)?;
            for child in ctx.children.iter().map(|x| db.hir_node(*x)) {
                let child = child?;
                let hir::HirNode::ChoiceIndirection(c) = child else { continue };
                if c.target_choice != choice.id {
                    continue;
                }
                let id_val = SubValue::new_val(c.id.0);
                for neigh in self
                    .graph
                    .neighbors_directed(self.val_map[&id_val], Direction::Incoming)
                {
                    let subval = self.graph[neigh];
                    let node = db.hir_node(subval.id)?;
                    let edge = self.graph.find_edge(neigh, self.val_map[&id_val]).unwrap();
                    // control edges shouldn't be considered
                    if !self.graph[edge] {
                        continue;
                    }
                    //
                    if !node.is_kind(hir::HirNodeKind::ChoiceIndirection.into())
                        && subval.id != self.block.0
                    {
                        continue 'outer;
                    }
                }
            }

            return_id_stack.extend(c.choices.iter().map(|x| x.1));
        }
        Ok(ret)
    }

    pub fn reachables(&self) -> [FxHashSet<SubValue>; 3] {
        let reachable_val = self.find_reachable(&[SubValue::new_val(self.block.id())]);
        let reachable_back = self.find_reachable(&[SubValue::new_back(self.block.id())]);
        let reachable_bt = self.find_reachable(&[SubValue::new_bt(self.block.id())]);
        [reachable_val, reachable_back, reachable_bt]
    }

    pub fn eval_order(mut self, db: &dyn Dependents) -> SResult<BlockSerialization> {
        let mut eval_order = Vec::new();
        let mut parse_requirements: BTreeMap<_, RequirementMatrix> = BTreeMap::new();
        let tails = self.tail_returns(db)?;

        let [reachable_val, reachable_back, reachable_bt] = self.reachables();

        let mut edgeless = BTreeSet::default();

        let get_prio = |graph: &Graph<SubValue, bool>, idx: NodeIndex<u32>| {
            let subvalue = graph[idx];
            let tail = tails.contains(&subvalue.id);
            let backtrack = db.can_backtrack(subvalue.id).unwrap_or_default();
            SubValuePrio {
                tail,
                backtrack,
                subvalue,
            }
        };

        for idx in self.val_map.values() {
            let outdegree = self.graph.edges_directed(*idx, Direction::Outgoing).count();
            if outdegree == 0 {
                edgeless.insert(get_prio(&self.graph, *idx));
            }
        }

        while let Some(prio) = edgeless.first().copied() {
            edgeless.take(&prio);
            let val = prio.subvalue;
            let idx = self.val_map[&val];
            for neighbor in self
                .graph
                .neighbors_directed(idx, Direction::Incoming)
                .collect::<Vec<_>>()
            {
                let edge_idx = self.graph.find_edge(neighbor, idx).unwrap();
                self.graph.remove_edge(edge_idx);
                if self
                    .graph
                    .edges_directed(neighbor, Direction::Outgoing)
                    .count()
                    == 0
                {
                    edgeless.insert(get_prio(&self.graph, neighbor));
                }
            }
            let mut requirements = RequirementSet::empty();
            if reachable_val.contains(&val) {
                requirements |= NeededBy::Val;
            }
            if reachable_back.contains(&val) {
                requirements |= NeededBy::Len;
            }
            if reachable_bt.contains(&val) {
                requirements |= NeededBy::Backtrack;
            }

            let required_by = match val.kind {
                SubValueKind::Bt => NeededBy::Backtrack.into(),
                SubValueKind::Val => NeededBy::Val.into(),
                SubValueKind::Front => RequirementSet::empty(),
                SubValueKind::Back => NeededBy::Len.into(),
            };
            let matrix = RequirementMatrix::from_outer_product(required_by, requirements);
            parse_requirements
                .entry(val.id)
                .and_modify(|e| *e = *e | matrix)
                .or_insert(matrix);
            eval_order.push(SubValueInfo {
                val,
                requirements,
                tail: prio.tail,
            })
        }
        Ok(BlockSerialization {
            eval_order: Arc::new(eval_order),
            parse_requirements: Arc::new(parse_requirements),
            tails: Arc::new(tails.into_iter().collect()),
        })
    }

    pub fn find_cycles(&self) -> Vec<Vec<SubValue>> {
        let condensed_graph = petgraph::algo::condensation(self.graph.clone(), false);
        let mut cycles = Vec::new();
        for node in condensed_graph.node_indices() {
            if condensed_graph.contains_edge(node, node) {
                cycles.push(condensed_graph.node_weight(node).unwrap().clone());
            }
        }
        cycles
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

impl std::fmt::Display for NeededBy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NeededBy::Len => write!(f, "len"),
            NeededBy::Val => write!(f, "val"),
            NeededBy::Backtrack => write!(f, "backtrack"),
        }
    }
}

pub type RequirementSet = BitFlags<NeededBy>;

// stored in column-major order
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct RequirementMatrix([RequirementSet; 3]);

impl RequirementMatrix {
    pub fn new(len: RequirementSet, val: RequirementSet, backtrack: RequirementSet) -> Self {
        Self([len, val, backtrack])
    }

    pub fn id() -> Self {
        Self::new(
            NeededBy::Len.into(),
            NeededBy::Val.into(),
            NeededBy::Backtrack.into(),
        )
    }

    pub fn zero() -> Self {
        Self::default()
    }

    /// calculates the boolean matrix operation self * needed_by
    pub fn needs_when(self, needed_by: RequirementSet) -> RequirementSet {
        let mut ret = RequirementSet::empty();
        for (i, val) in Self::id().iter_cols().enumerate() {
            if needed_by.contains(val) {
                ret |= self.0[i];
            }
        }
        ret
    }

    pub fn from_outer_product(left: RequirementSet, right: RequirementSet) -> Self {
        let mut ret = Self::default();
        for (i, val) in Self::id().iter_cols().enumerate() {
            if right.contains(val) {
                ret.0[i] |= left;
            }
        }
        ret
    }

    pub fn iter_cols(self) -> impl Iterator<Item = RequirementSet> {
        self.0.into_iter()
    }
}

impl std::ops::Mul<RequirementSet> for RequirementMatrix {
    type Output = RequirementSet;
    fn mul(self, rhs: RequirementSet) -> Self::Output {
        self.needs_when(rhs)
    }
}

impl std::ops::BitOr for RequirementMatrix {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Self([
            self.0[0] | rhs.0[0],
            self.0[1] | rhs.0[1],
            self.0[2] | rhs.0[2],
        ])
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct SubValueInfo {
    pub val: SubValue,
    pub requirements: RequirementSet,
    pub tail: bool,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BlockSerialization {
    pub eval_order: Arc<Vec<SubValueInfo>>,
    pub parse_requirements: Arc<BTreeMap<DefId, RequirementMatrix>>,
    pub tails: Arc<BTreeSet<DefId>>,
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
    let graph = DependencyGraph::new(db, block)?;

    let errors = graph.find_cycles();
    if !errors.is_empty() {
        return Err(BlockSerializationError {
            inner: Arc::new(errors),
        });
    }

    Ok(graph.eval_order(db)?)
}

#[cfg(test)]
mod tests {
    use hir::{BlockId, HirDatabase};
    use yaboc_ast::{import::Import, AstDatabase};
    use yaboc_base::{
        config::ConfigDatabase,
        interner::InternerDatabase,
        source::{FileDatabase, FileId},
        Context,
    };
    use yaboc_hir_types::{id_cursor::IdCursor, HirTypesDatabase};
    use yaboc_resolve::ResolveDatabase;
    use yaboc_types::TypeInternerDatabase;

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
def *cycle = {
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
def *cycle = {
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
def *main = {
    | y: ~
    | let y: int = 0
}
        "#,
        );
        let block = IdCursor::at_file(&ctx.db, FileId::default())
            .pd("main")
            .expr()
            .expr_block(0);
        let _ = ctx.db.block_serialization(BlockId(block.id)).unwrap();
        // TODO(8051): write actual test
    }
    #[test]
    fn inner_choice() {
        let ctx = Context::<DependentsTestDatabase>::mock(
            r"
def for['a] *> first = +
def for['b] *> second = +
def for[int] *> main = {
    a: ~
    b: second?
    c: {
        let x = a
        | c: first?
        | c: second
    }
}
            ",
        );
        let block = IdCursor::at_file(&ctx.db, FileId::default())
            .pd("main")
            .expr()
            .expr_block(0);
        let s = ctx.db.block_serialization(BlockId(block.id)).unwrap();
        let a = s.parse_requirements[&block.field("a").id];
        let b = s.parse_requirements[&block.field("b").id];
        let c = s.parse_requirements[&block.field("c").id];
        assert_eq!(
            format!("{}", a),
            "111\n\
             110\n\
             000"
        );
        assert_eq!(
            format!("{}", c),
            "100\n\
             010\n\
             000"
        );
        assert_eq!(
            format!("{}", b),
            "110\n\
             010\n\
             001"
        );
        assert_eq!(format!("{}", b * NeededBy::Backtrack.into()), "Backtrack");
        assert_eq!(format!("{}", b * NeededBy::Val.into()), "Len | Val");
        assert_eq!(
            format!("{}", c * (NeededBy::Len | NeededBy::Val)),
            "Len | Val"
        );
    }

    #[test]
    fn tail_return() {
        let ctx = Context::<DependentsTestDatabase>::mock(
            r"
def for[int] *> main: int = {
  x: ~
  | let _ = x if 3
    return: main
  | let return = 4
}
        ",
        );
        let block = IdCursor::at_file(&ctx.db, FileId::default())
            .pd("main")
            .expr()
            .expr_block(0);
        let s = ctx.db.block_serialization(BlockId(block.id)).unwrap();
        let ret_choice = block.return_field();
        assert!(s.tails.contains(&ret_choice.id));
        let ret_parse = ret_choice.choice(0);
        assert!(s.tails.contains(&ret_parse.id));
        let ret_let = ret_choice.choice(1);
        assert!(s.tails.contains(&ret_let.id));
    }
    #[test]
    fn choice_ref_no_tail() {
        let ctx = Context::<DependentsTestDatabase>::mock(
            r"
def for[int] *> main: int = {
    x: ~
    let y = z if 3
    | let z = x if 3..4
      return: main
    | let return = 4
}
        ",
        );
        let block = IdCursor::at_file(&ctx.db, FileId::default())
            .pd("main")
            .expr()
            .expr_block(0);
        let s = ctx.db.block_serialization(BlockId(block.id)).unwrap();
        let ret_parse = block.return_field().choice(0);
        assert!(!s.tails.contains(&ret_parse.id));
    }
    #[test]
    fn bt_no_tail() {
        let ctx = Context::<DependentsTestDatabase>::mock(
            r"
def for[int] *> main: int = {
    x: ~ if 1..4
    | return: main?
    | let return = x if 1..2
}
        ",
        );
        let block = IdCursor::at_file(&ctx.db, FileId::default())
            .pd("main")
            .expr()
            .expr_block(0);
        let s = ctx.db.block_serialization(BlockId(block.id)).unwrap();
        let ret_parse = block.return_field().choice(0);
        assert!(!s.tails.contains(&ret_parse.id));
    }
}
