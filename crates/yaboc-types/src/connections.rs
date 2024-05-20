use std::collections::hash_map::Entry;

use fxhash::{FxHashMap, FxHashSet};
use petgraph::unionfind::UnionFind;
use yaboc_base::interner::DefId;

use crate::{TypeConvError, TypeVarRef};

#[derive(Default)]
pub struct Connections {
    edges: FxHashSet<[TypeVarRef; 2]>,
}

impl Connections {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    /// adds an unordered edge to the graph
    pub fn add_edge(&mut self, a: TypeVarRef, b: TypeVarRef) {
        let start = a.min(b);
        let end = a.max(b);
        self.edges.insert([start, end]);
    }

    pub fn connections_map(&self) -> Result<ConnectionMap, TypeConvError> {
        let mut id_map = FxHashMap::default();
        let mut idx = 0;
        for &nodes in self.edges.iter() {
            for &node in &nodes {
                id_map.entry(node).or_insert_with(|| {
                    let id = idx;
                    idx += 1;
                    id
                });
            }
        }
        let mut uf = UnionFind::new(id_map.len());
        for [a, b] in &self.edges {
            uf.union(id_map[a], id_map[b]);
        }
        let mut connected_component = FxHashMap::default();
        let mut type_var = FxHashMap::default();
        for (node, &id) in &id_map {
            let root = uf.find(id);
            match type_var.entry((root, node.0)) {
                Entry::Vacant(ent) => {
                    ent.insert(*node);
                }
                Entry::Occupied(ent) => {
                    // if two type vars are in the same connected component in the same
                    // parserdef, then we must have a polymorphic recursion somewhere
                    return Err(TypeConvError::PolymorphicRecursion(*node, *ent.get()));
                }
            }
            connected_component.insert(*node, root);
        }
        Ok(ConnectionMap {
            connected_component,
            type_var,
        })
    }
}

pub struct ConnectionMap {
    /// a map from the typevar to its connected component
    connected_component: FxHashMap<TypeVarRef, usize>,
    /// a map to get the type var back from its connected component and DefID
    type_var: FxHashMap<(usize, DefId), TypeVarRef>,
}

impl ConnectionMap {
    /// for a typevar returns its corresponding typevar in the current
    /// parserdef
    pub fn def_map(&self, id: DefId, var: TypeVarRef) -> Option<TypeVarRef> {
        if id == var.0 {
            return Some(var);
        }
        // if the current var does not appear in any connected component,
        // that means it didn't appear in any add_edge call so we cannot
        // map it back to the current parserdef
        let component = self.connected_component.get(&var)?;
        self.type_var.get(&(*component, id)).copied()
    }
}
