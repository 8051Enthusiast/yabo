use std::collections::HashSet;

use hir::DefKind;
use yaboc_ast::expr::Atom;
use yaboc_hir::{walk, HirNode};

use super::*;

pub enum VarType {
    ParserDef,
    Static,
    Value,
}

fn core_mod(db: &(impl Resolves + ?Sized)) -> SResult<hir::Module> {
    let core = db.intern_hir_path(DefinitionPath::Module(db.core()?));
    let HirNode::Module(module) = db.hir_node(core)? else {
        panic!("Core is not a module");
    };
    Ok(module)
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
    if let Some(parserdef) = current_module.lookup(db)?.defs.get(final_name).copied() {
        return Ok(Some(parserdef));
    }
    Ok(core_mod(db)?.defs.get(final_name).copied())
}

pub enum Resolved {
    Value(DefId, VarType),
    Module(hir::ModuleId),
    Unresolved,
}

fn resolve_pd(db: &(impl Resolves + ?Sized), pd: hir::ParserDefId) -> SResult<Resolved> {
    let parserdef = pd.lookup(db)?;
    if parserdef.kind == DefKind::Static {
        Ok(Resolved::Value(pd.id(), VarType::Static))
    } else {
        Ok(Resolved::Value(pd.id(), VarType::ParserDef))
    }
}

pub fn resolve_var_ref(
    db: &(impl Resolves + ?Sized),
    loc: DefId,
    ident: FieldName,
    use_core: bool,
    outside_block: bool,
) -> SResult<Resolved> {
    let mut current_id = loc;
    loop {
        current_id = match &db.hir_node(current_id)? {
            HirNode::Block(b) => {
                if !outside_block {
                    return Ok(Resolved::Unresolved);
                }
                match b.super_context {
                    Some(id) => id.id(),
                    None => db.hir_parent_parserdef(current_id)?.id(),
                }
            }
            HirNode::Module(m) => {
                let ident = match ident {
                    FieldName::Ident(n) => n,
                    FieldName::Return => return Ok(Resolved::Unresolved),
                };
                if let Some(s) = m.defs.get(&ident) {
                    return resolve_pd(db, *s);
                }
                if let Some(import) = m.imports.get(&ident) {
                    return Ok(Resolved::Module(import.lookup(db)?.mod_ref));
                } else if use_core {
                    let core = core_mod(db)?;
                    if let Some(s) = core.defs.get(&ident) {
                        return resolve_pd(db, *s);
                    }
                    // we intentionally do not check for imports here, this way the core
                    // module can have imports for implementation without polluting the
                    // global namespace
                }
                return Ok(Resolved::Unresolved);
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
            HirNode::Lambda(lambda_id) => match ident {
                FieldName::Ident(n) => match db.lambda_arg(lambda_id.id, n)? {
                    Some(child_id) => return Ok(Resolved::Value(child_id.0, VarType::Value)),
                    None => current_id.parent(db).unwrap(),
                },
                FieldName::Return => current_id.parent(db).unwrap(),
            },
            _ => current_id.parent(db).unwrap(),
        }
    }
}

// identifiers inside of expression
pub fn expr_idents(expr: &hir::ValExpression) -> Vec<FieldName> {
    let mut ret = Vec::new();
    for part in expr.expr.expr.take_ref().iter_parts() {
        let ExprHead::Niladic(hir::ParserAtom::Atom(Atom::Field(ident))) = part else {
            continue;
        };
        ret.push(ident);
    }
    ret
}

fn expr_parser_refs<'a>(
    db: &'a (impl Resolves + ?Sized),
    expr: &hir::ValExpression,
    context: DefId,
) -> impl Iterator<Item = hir::ParserDefId> + 'a {
    expr_idents(expr)
        .into_iter()
        .flat_map(move |ident| resolve_var_ref(db, context, ident, false, true).ok())
        .flat_map(|resolved| match resolved {
            Resolved::Value(id, VarType::ParserDef | VarType::Static) => Some(hir::ParserDefId(id)),
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
