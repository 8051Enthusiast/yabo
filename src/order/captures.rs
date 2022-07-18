use std::{collections::BTreeSet, sync::Arc};

use crate::{
    dbeprintln,
    hir::{self, walk::ChildIter, HirIdWrapper},
    interner::DefId,
};

use super::Orders;

pub fn captures(db: &dyn Orders, id: hir::BlockId) -> Arc<BTreeSet<DefId>> {
    let mut ret = BTreeSet::new();
    let root_context = match id.lookup(db) {
        Ok(x) => x,
        Err(_) => return Default::default(),
    }
    .root_context;
    for i in ChildIter::new(root_context.0, db).without_kinds(hir::HirNodeKind::Block) {
        if let hir::HirNode::Expr(expr) = i {
            ret.extend(
                hir::refs::expr_value_refs(db, &expr, expr.id.0)
                    .filter(|target| !id.0.is_ancestor_of(db, *target)),
            );
        }
    }
    dbeprintln!(db, "{}:", &id.0);
    for i in ret.iter() {
        dbeprintln!(db, "\t{}", i);
    }
    Arc::new(ret)
}
