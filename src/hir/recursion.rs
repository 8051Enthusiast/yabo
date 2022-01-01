use petgraph::Graph;
use std::{collections::HashMap, sync::Arc};

use super::*;

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct FunctionSscId(salsa::InternId);

impl salsa::InternKey for FunctionSscId {
    fn from_intern_id(v: salsa::InternId) -> Self {
        FunctionSscId(v)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

pub fn parser_ssc(db: &dyn Hirs, id: ParserDefId) -> Result<FunctionSscId, ()> {
    let module = db.hir_parent_module(id.0)?;
    let sscs = db.mod_parser_ssc(module)?;
    sscs.get(&id).copied().ok_or(())
}

pub fn mod_parser_ssc(
    db: &dyn Hirs,
    module: ModuleId,
) -> Result<Arc<BTreeMap<ParserDefId, FunctionSscId>>, ()> {
    let mut graph = Graph::new();
    let module = module.lookup(db)?;
    let mut index_map = HashMap::new();
    for parser in module.defs.values() {
        let index = graph.add_node(*parser);
        index_map.insert(*parser, index);
    }
    for &parser in module.defs.values() {
        let from = index_map[&parser];
        for refs in refs::find_parser_refs(db, parser.0)? {
            let to = index_map[&refs];
            graph.add_edge(from, to, ());
        }
    }
    let sscs = petgraph::algo::kosaraju_scc(&graph);
    let mut ret = BTreeMap::new();
    for ssc in sscs {
        let new_ssc: Vec<_> = ssc
            .iter()
            .map(|x| *graph.node_weight(*x).unwrap())
            .collect();
        let id = db.intern_recursion_scc(new_ssc.clone());
        for parser in new_ssc {
            ret.insert(parser, id);
        }
    }
    Ok(Arc::new(ret))
}
