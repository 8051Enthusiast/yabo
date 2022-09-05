use std::collections::HashSet;

use crate::{
    expr::{Atom, ExpressionHead},
    hir::{walk, HirIdWrapper, HirNode},
    interner::{DefId, FieldName, Identifier},
};

use super::*;

pub enum VarType {
    ParserDef,
    Value,
}

pub fn parserdef_ref(
    db: &(impl Resolves + ?Sized),
    loc: DefId,
    name: Identifier,
) -> SResult<Option<hir::ParserDefId>> {
    let parent_module = db.hir_parent_module(loc)?;
    Ok(parent_module.lookup(db)?.defs.get(&name).copied())
}

pub fn resolve_var_ref(
    db: &(impl Resolves + ?Sized),
    loc: DefId,
    ident: FieldName,
) -> SResult<Option<(DefId, VarType)>> {
    let mut current_id = loc;
    loop {
        current_id = match &db.hir_node(current_id)? {
            HirNode::Block(b) => match b.super_context {
                Some(id) => id.id(),
                None => db.hir_parent_module(current_id)?.id(),
            },
            HirNode::Module(m) => {
                let ident = match ident {
                    FieldName::Ident(n) => n,
                    FieldName::Return => return Ok(None),
                };
                return Ok(m.defs.get(&ident).map(|s| (s.id(), VarType::ParserDef)));
            }
            HirNode::Context(ctx) => match ctx.vars.get(ident) {
                Some(id) => return Ok(Some((*id.inner(), VarType::Value))),
                None => ctx
                    .parent_context
                    .map(|x| x.id())
                    .unwrap_or_else(|| ctx.block_id.id()),
            },
            _ => current_id.parent(db),
        }
    }
}

// identifiers inside of expression
pub fn expr_idents(expr: &hir::ValExpression) -> Vec<FieldName> {
    let mut ret = Vec::new();
    for node in crate::expr::ExprIter::new(&expr.expr) {
        let ident = match &node.0 {
            ExpressionHead::Niladic(a) => match a.inner {
                hir::ParserAtom::Atom(Atom::Field(ident)) => ident,
                _ => continue,
            },
            _ => continue,
        };
        ret.push(ident);
    }
    ret
}

pub fn expr_parser_refs<'a>(
    db: &'a (impl Resolves + ?Sized),
    expr: &hir::ValExpression,
    context: DefId,
) -> impl Iterator<Item = hir::ParserDefId> + 'a {
    expr_idents(expr)
        .into_iter()
        .flat_map(move |ident| resolve_var_ref(db, context, ident).ok()?)
        .flat_map(|(id, ty)| matches!(ty, VarType::ParserDef).then(|| hir::ParserDefId(id)))
}

pub fn expr_value_refs<'a>(
    db: &'a (impl Resolves + ?Sized),
    expr: &hir::ValExpression,
    context: DefId,
) -> impl Iterator<Item = DefId> + 'a {
    expr_idents(expr)
        .into_iter()
        .flat_map(move |ident| resolve_var_ref(db, context, ident).ok()?)
        .flat_map(|(id, ty)| matches!(ty, VarType::Value).then(|| id))
}

pub fn find_parser_refs(
    db: &(impl Resolves + ?Sized),
    id: DefId,
) -> SResult<Vec<hir::ParserDefId>> {
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
