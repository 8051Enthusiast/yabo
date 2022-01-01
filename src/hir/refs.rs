use std::collections::HashSet;

use crate::hir::represent::HirToString;

use super::*;

pub enum VarType {
    ParserDef,
    Value,
}

pub fn resolve_var_ref(
    db: &dyn Hirs,
    context: HirId,
    ident: Identifier,
) -> Result<Option<(HirId, VarType)>, ()> {
    let mut current_id = context;
    loop {
        current_id = match &db.hir_node(current_id)? {
            HirNode::Block(b) => match b.super_context {
                Some(id) => id.id(),
                None => db.hir_parent_module(current_id)?.id(),
            },
            HirNode::Module(m) => {
                return Ok(m.defs.get(&ident).map(|s| (s.id(), VarType::ParserDef)))
            }
            HirNode::Context(ctx) => match ctx.vars.get(ident) {
                Some(id) => return Ok(Some((*id.inner(), VarType::Value))),
                None => ctx
                    .parent_context
                    .map(|x| x.id())
                    .unwrap_or(ctx.block_id.id()),
            },
            n => unreachable!(
                "Internal Compiler Error: invalid node {}",
                n.hir_to_string(db)
            ),
        }
    }
}

// identifiers inside of expression
fn expr_idents(expr: &ValExpression) -> Vec<Identifier> {
    let mut ret = Vec::new();
    for node in crate::expr::ExprIter::new(&expr.expr) {
        let ident = match node {
            Expression::Atom(a) => match a.atom {
                ParserAtom::Atom(Atom::Id(ident)) => ident,
                _ => continue,
            },
            _ => continue,
        };
        ret.push(ident);
    }
    ret
}

fn expr_parser_refs(db: &dyn Hirs, expr: &ValExpression, context: HirId) -> Vec<ParserDefId> {
    expr_idents(&expr)
        .iter()
        .flat_map(|ident| resolve_var_ref(db, context, *ident))
        .flatten()
        .flat_map(|(id, ty)| matches!(ty, VarType::ParserDef).then(|| ParserDefId(id)))
        .collect::<Vec<_>>()
}

pub fn find_parser_refs(db: &dyn Hirs, id: HirId) -> Result<Vec<ParserDefId>, ()> {
    let mut refs = HashSet::new();
    for node in walk::ChildIter::new(id, db) {
        let (expr, context) = match node {
            HirNode::Let(l) => (l.expr.lookup(db)?, l.context.id()),
            HirNode::Parse(p) => (p.expr.lookup(db)?, p.parent_context.id()),
            HirNode::Array(a) => {
                // use context if it exists, else just use the module (since we are in a parser def)
                let context = a
                    .context
                    .map(|x| Ok(x.id()))
                    .unwrap_or_else(|| db.hir_parent_module(a.id.id()).map(|x| x.id()))?;
                (a.expr.lookup(db)?, context)
            }
            HirNode::ParserDef(p) => {
                let context = db.hir_parent_module(p.id.id())?.id();
                (p.to.lookup(db)?, context)
            }
            _ => continue,
        };
        refs.extend(expr_parser_refs(db, &expr, context));
    }
    Ok(refs.into_iter().collect())
}
