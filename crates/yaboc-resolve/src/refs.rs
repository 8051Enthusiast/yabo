use std::collections::HashSet;

use yaboc_ast::expr::{Atom, ExpressionHead};
use yaboc_base::interner::{DefId, FieldName, Identifier};
use yaboc_hir::{walk, HirIdWrapper, HirNode};

use super::*;

pub enum VarType {
    ParserDef,
    Value,
}

pub fn parserdef_ref(
    db: &(impl Resolves + ?Sized),
    loc: DefId,
    name: Vec<Identifier>,
) -> SResult<Option<hir::ParserDefId>> {
    let mut current_module = db.hir_parent_module(loc)?;
    for modname in name[..name.len() - 1].iter() {
        let Some(import) = current_module.lookup(db)?.imports.get(modname).copied() else {
            return Ok(None);
        };
        current_module = import.lookup(db)?.mod_ref;
    }
    let final_name = name.last().unwrap();
    Ok(current_module.lookup(db)?.defs.get(final_name).copied())
}

pub enum Resolved {
    Value(DefId, VarType),
    Module(hir::ModuleId),
    Unresolved,
}

pub fn resolve_var_ref(
    db: &(impl Resolves + ?Sized),
    loc: DefId,
    ident: FieldName,
) -> SResult<Resolved> {
    let mut current_id = loc;
    loop {
        current_id = match &db.hir_node(current_id)? {
            HirNode::Block(b) => match b.super_context {
                Some(id) => id.id(),
                None => db.hir_parent_parserdef(current_id)?.id(),
            },
            HirNode::Module(m) => {
                let ident = match ident {
                    FieldName::Ident(n) => n,
                    FieldName::Return => return Ok(Resolved::Unresolved),
                };
                if let Some(s) = m.defs.get(&ident) {
                    return Ok(Resolved::Value(s.id(), VarType::ParserDef));
                }
                if let Some(import) = m.imports.get(&ident) {
                    return Ok(Resolved::Module(import.lookup(db)?.mod_ref));
                } else {
                    return Ok(Resolved::Unresolved);
                }
            }
            HirNode::Context(ctx) => match ctx.vars.get(ident) {
                Some(id) => return Ok(Resolved::Value(*id.inner(), VarType::Value)),
                None => ctx
                    .parent_context
                    .map(|x| x.id())
                    .unwrap_or_else(|| ctx.block_id.id()),
            },
            HirNode::ParserDef(pd) => {
                let id = match ident {
                    FieldName::Ident(n) => n,
                    // the only parent here can be a module, which does not have return identifiers either
                    FieldName::Return => return Ok(Resolved::Unresolved),
                };
                match db.parserdef_arg(pd.id, id)? {
                    Some(child_id) => return Ok(Resolved::Value(child_id.0, VarType::Value)),
                    None => db.hir_parent_module(current_id)?.id(),
                }
            }
            _ => current_id.parent(db),
        }
    }
}

// identifiers inside of expression
pub fn expr_idents(expr: &hir::ValExpression) -> Vec<FieldName> {
    let mut ret = Vec::new();
    for node in yaboc_ast::expr::ExprIter::new(&expr.expr) {
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
        .flat_map(move |ident| resolve_var_ref(db, context, ident).ok())
        .flat_map(|resolved| match resolved {
            Resolved::Value(id, VarType::ParserDef) => Some(hir::ParserDefId(id)),
            _ => None,
        })
}

pub fn find_parser_refs_within_mod(
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
