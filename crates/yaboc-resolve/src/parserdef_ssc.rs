use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    sync::Arc,
};

use hir::DefKind;
use petgraph::{
    visit::{DfsPostOrder, Walker},
    Graph,
};

use yaboc_base::error::{SResult, Silencable, SilencedError};
use yaboc_hir::{self as hir, HirIdWrapper};

use crate::{ResolveError, ResolveErrors};

use super::{refs, Resolves};

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

pub fn parser_ssc(db: &dyn Resolves, id: hir::ParserDefId) -> SResult<FunctionSscId> {
    let module = db.hir_parent_module(id.0)?;
    let order = db.mod_parser_ssc(module).silence()?;
    order.sscs.get(&id).copied().ok_or_else(SilencedError::new)
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ModuleOrder {
    pub sscs: Arc<BTreeMap<hir::ParserDefId, FunctionSscId>>,
    pub statics: Arc<[hir::ParserDefId]>,
}

pub fn mod_parser_ssc(
    db: &dyn Resolves,
    module: hir::ModuleId,
) -> Result<ModuleOrder, ResolveErrors> {
    db.module_sequence().silence()?;
    let mut graph = Graph::new();
    let module = module.lookup(db)?;
    let mut index_map = HashMap::new();
    for parser in module.defs.values() {
        let index = graph.add_node(*parser);
        index_map.insert(*parser, index);
    }
    for &parser in module.defs.values() {
        let from = index_map[&parser];
        for refs in refs::find_parser_refs_within_mod(db, parser.0)? {
            let to = index_map[&refs];
            graph.add_edge(from, to, ());
        }
    }
    let sscs = petgraph::algo::kosaraju_scc(&graph);
    let mut ret_sscs = BTreeMap::new();
    let mut statics = BTreeSet::default();
    let mut errs = Vec::new();
    for ssc in sscs {
        let is_rec = ssc.len() > 1 || graph.contains_edge(ssc[0], ssc[0]);
        let new_ssc: Vec<_> = ssc
            .iter()
            .map(|x| *graph.node_weight(*x).unwrap())
            .collect();
        let id = db.intern_recursion_scc(new_ssc.clone());
        for pd in new_ssc {
            ret_sscs.insert(pd, id);
            if pd.lookup(db)?.kind == DefKind::Static {
                if is_rec {
                    errs.push(ResolveError::CyclicGlobal(pd));
                }
                statics.insert(pd);
            }
        }
    }
    if !errs.is_empty() {
        return Err(ResolveErrors(errs.into()));
    }
    let mut ret_statics = Vec::new();
    while let Some(s) = statics.pop_first() {
        let dfs = DfsPostOrder::new(&graph, index_map[&s]);
        for node in dfs.iter(&graph) {
            if statics.remove(&graph[node]) {
                ret_statics.push(graph[node]);
            }
        }
        ret_statics.push(s);
    }
    let ret = ModuleOrder {
        sscs: Arc::new(ret_sscs),
        statics: ret_statics.into(),
    };
    Ok(ret)
}
