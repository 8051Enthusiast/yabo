use std::{collections::BTreeSet, sync::Arc};

use crate::{
    hir::{self, walk::ChildIter},
    interner::DefId,
};

use super::Orders;

pub fn captures(db: &dyn Orders, id: hir::BlockId) -> Arc<BTreeSet<DefId>> {
    let mut ret = BTreeSet::new();
    for i in ChildIter::new(id.0, db).without_kinds(hir::HirNodeKind::Block) {
        match i {
            hir::HirNode::Expr(expr) => {
                ret.extend(
                    hir::refs::expr_value_refs(db, &expr, expr.id.0)
                        .filter(|target| !id.0.is_ancestor_of(db, *target)),
                );
            }
            hir::HirNode::Block(block) => {
                if block.id != id {
                    ret.extend(db.captures(block.id).iter());
                }
            }
            _ => continue,
        }
    }
    Arc::new(ret)
}
