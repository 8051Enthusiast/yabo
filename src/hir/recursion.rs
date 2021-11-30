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

pub fn mod_parser_ssc(db: &dyn Hirs, module: ModuleId) -> Result<Arc<BTreeMap<ParserDefId, FunctionSscId>>, ()> {
    let mut graph = Graph::new();
    let module = module.lookup(db)?;
    let mut index_map = HashMap::new();
    for parser in module.defs.values() {
        let index = graph.add_node(*parser);
        index_map.insert(*parser, index);
    }
    for &parser in module.defs.values() {
        let from = index_map[&parser];
        for refs in parser_refs(db, parser.0)? {
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

fn parser_refs(db: &dyn Hirs, id: HirId) -> Result<Vec<ParserDefId>, ()> {
    let mut ids = vec![id];
    let mut ret = Vec::new();
    while let Some(id) = ids.pop() {
        match db.hir_node(id)? {
            // note: we leave out the type annotation here because it is not relevant
            HirNode::Let(l) => ids.push(l.expr.0),
            HirNode::Expr(e) => ret.append(&mut vexpr_prefs(db, &e.expr, id)?),
            HirNode::PExpr(e) => ret.append(&mut pexpr_prefs(db, &e.expr, id)?),
            HirNode::Module(_) => continue,
            otherwise => ids.append(&mut otherwise.children()),
        }
    }
    Ok(ret)
}

fn pexpr_prefs(
    db: &dyn Hirs,
    expr: &Expression<HirParse>,
    id: HirId,
) -> Result<Vec<ParserDefId>, ()> {
    match &expr {
        Expression::BinaryOp(l) => match l.as_ref() {
            expr::ParseBinOp::Pipe(a, b, _) => {
                let mut a = pexpr_prefs(db, a, id).unwrap_or_default();
                a.append(&mut pexpr_prefs(db, b, id).unwrap_or_default());
                Ok(a)
            }
            expr::ParseBinOp::Wiggle(a, _, _) => pexpr_prefs(db, a, id),
            expr::ParseBinOp::Dot(_, _, _) => todo!(),
        },
        Expression::UnaryOp(l) => match l.as_ref() {
            expr::ParseUnOp::If(a, _) | expr::ParseUnOp::Try(a, _) => pexpr_prefs(db, a, id),
        },
        Expression::Atom(a) => match &a.atom {
            ParserAtom::Atom(Atom::Id(ident)) => {
                parser_reference(db, id, *ident).map(|x| x.into_iter().collect())
            }
            ParserAtom::Atom(_) => Ok(vec![]),
            ParserAtom::Array(array) => parser_refs(db, array.0),
            ParserAtom::Block(block) => parser_refs(db, block.0),
        },
    }
}

fn vexpr_prefs(
    db: &dyn Hirs,
    expr: &Expression<HirVal>,
    id: HirId,
) -> Result<Vec<ParserDefId>, ()> {
    match &expr {
        Expression::BinaryOp(a) => match a.as_ref() {
            expr::ValBinOp::Basic(a, _, b, _) | expr::ValBinOp::Else(a, b, _) => {
                let mut a = vexpr_prefs(db, a, id).unwrap_or_default();
                a.append(&mut vexpr_prefs(db, b, id).unwrap_or_default());
                Ok(a)
            }
            expr::ValBinOp::Pipe(a, b, _) => {
                let mut a = vexpr_prefs(db, a, id).unwrap_or_default();
                a.append(&mut pexpr_prefs(db, b, id).unwrap_or_default());
                Ok(a)
            }
            expr::ValBinOp::Dot(_, _, _) => todo!(),
        },
        Expression::UnaryOp(a) => match a.as_ref() {
            expr::ValUnOp::Not(a, _) | expr::ValUnOp::Neg(a, _) | expr::ValUnOp::Pos(a, _) => {
                vexpr_prefs(db, a, id)
            }
        },
        Expression::Atom(a) => match &a.atom {
            Atom::Id(ident) => parser_reference(db, id, *ident).map(|x| x.into_iter().collect()),
            _ => Ok(vec![]),
        },
    }
}

fn parser_reference(
    db: &dyn Hirs,
    id: HirId,
    ident: Identifier,
) -> Result<Option<ParserDefId>, ()> {
    let mut block = db.hir_parent_block(id)?;
    while let Some(b) = block {
        let block_context = b.lookup(db)?.root_context.lookup(db)?;
        if block_context.vars.set.contains_key(&ident) {
            return Ok(None);
        }
        block = db.hir_parent_block(b.0)?;
    }
    let module = db.hir_parent_module(id)?.lookup(db)?;
    Ok(module.defs.get(&ident).cloned())
}
