use std::sync::Arc;

use crate::expr::{Atom, Expression, KindWithData, OpWithData, ValBinOp, ValUnOp, ValVarOp};
use crate::hir::HirIdWrapper;
use crate::resolve::refs;
use crate::source::SpanIndex;
use crate::{expr::ExpressionKind, hir, interner::DefId};

use super::{ResolveError, Resolves};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedAtom {
    Val(DefId),
    Captured(DefId),
    ParserDef(hir::ParserDefId),
    Number(i64),
    Char(u32),
    Single,
    Nil,
    Block(hir::BlockId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedKind;

impl ExpressionKind for ResolvedKind {
    type NiladicOp = ResolvedAtom;
    type MonadicOp = ValUnOp<Arc<Expression<hir::HirConstraintSpanned>>>;
    type DyadicOp = ValBinOp;
    type VariadicOp = ValVarOp;
}

pub type ResolvedExpr = Expression<KindWithData<ResolvedKind, SpanIndex>>;

pub fn resolve_expr_error(
    db: &dyn Resolves,
    expr_id: hir::ExprId,
) -> Result<Arc<ResolvedExpr>, ResolveError> {
    let expr = expr_id.lookup(db)?.expr;
    let parent_block = db.hir_parent_block(expr_id.0)?;
    expr.convert_niladic(&mut |x| {
        let inner = match &x.inner {
            hir::ParserAtom::Atom(Atom::Number(s)) => ResolvedAtom::Number(*s),
            hir::ParserAtom::Atom(Atom::Char(s)) => ResolvedAtom::Char(*s),
            hir::ParserAtom::Single => ResolvedAtom::Single,
            hir::ParserAtom::Nil => ResolvedAtom::Nil,
            hir::ParserAtom::Block(b) => ResolvedAtom::Block(*b),
            hir::ParserAtom::Atom(Atom::Field(f)) => {
                let (id, kind) = refs::resolve_var_ref(db, expr_id.0, *f)?
                    .ok_or(ResolveError::Unresolved(expr_id, x.data, *f))?;
                match kind {
                    refs::VarType::ParserDef => ResolvedAtom::ParserDef(hir::ParserDefId(id)),
                    refs::VarType::Value => {
                        let is_captured = parent_block
                            .map(|x| !x.0.is_ancestor_of(db, id))
                            .unwrap_or(false);
                        if is_captured {
                            ResolvedAtom::Captured(id)
                        } else {
                            ResolvedAtom::Val(id)
                        }
                    }
                }
            }
        };
        Ok(OpWithData {
            inner,
            data: x.data,
        })
    })
    .map(Arc::new)
}
