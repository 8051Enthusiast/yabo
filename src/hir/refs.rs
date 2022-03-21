use std::collections::HashSet;

use crate::hir::represent::HirToString;

use super::*;

pub enum VarType {
    ParserDef,
    Value,
}

pub fn parserdef_ref(db: &dyn Hirs, loc: HirId, name: FieldName) -> Result<ParserDefId, ()> {
    let (id, kind) = resolve_var_ref(db, loc, name)?.ok_or(())?;
    if let VarType::Value = kind {
        return Err(());
    }
    if !is_const_ref(db, id) {
        return Err(());
    }
    // TODO(8051): change this when adding other types of toplevel items
    Ok(ParserDefId(id))
}

pub fn is_const_ref(db: &dyn Hirs, id: HirId) -> bool {
    match db.lookup_intern_hir_path(id).path() {
        [PathComponent::File(_), PathComponent::Named(_)] => true,
        _ => false,
    }
}

pub fn resolve_var_ref(
    db: &dyn Hirs,
    loc: HirId,
    ident: FieldName,
) -> Result<Option<(HirId, VarType)>, ()> {
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
                    FieldName::Return | FieldName::Next | FieldName::Prev => return Ok(None),
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
            n => unreachable!(
                "Internal Compiler Error: invalid node {}",
                n.hir_to_string(db)
            ),
        }
    }
}

// identifiers inside of expression
fn expr_idents(expr: &ValExpression) -> Vec<FieldName> {
    let mut ret = Vec::new();
    for node in crate::expr::ExprIter::new(&expr.expr) {
        let ident = match node {
            Expression::Atom(a) => match a.atom {
                ParserAtom::Atom(Atom::Field(ident)) => ident,
                _ => continue,
            },
            _ => continue,
        };
        ret.push(ident);
    }
    ret
}

fn expr_parser_refs(db: &dyn Hirs, expr: &ValExpression, context: HirId) -> Vec<ParserDefId> {
    expr_idents(expr)
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
