use crate::error::{SResult, SilencedError};
use crate::expr::{Atom, Expression, KindWithData, OpWithData, ValBinOp, ValUnOp};
use crate::source::SpanIndex;
use crate::types::TypeId;
use crate::{expr::ExpressionKind, hir, interner::DefId};

use super::Orders;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedAtom {
    Val(DefId),
    Captured(DefId),
    ParserDef(hir::ParserDefId),
    Number(i64),
    Char(u32),
    Single,
    Block(hir::BlockId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedExpr;

impl ExpressionKind for ResolvedExpr {
    type NiladicOp = ResolvedAtom;
    type MonadicOp = ValUnOp<std::sync::Arc<Expression<hir::HirConstraintSpanned>>>;
    type DyadicOp = ValBinOp;
}

pub type TypedResolvedExpr = Expression<KindWithData<ResolvedExpr, (TypeId, SpanIndex)>>;

pub fn resolve_expr(db: &dyn Orders, expr_id: hir::ExprId) -> SResult<TypedResolvedExpr> {
    let expr = db.parser_expr_at(expr_id)?;
    let parent_block = db.hir_parent_block(expr_id.0)?;
    expr.convert_niladic(&mut |x| {
        let inner = match &x.inner {
            hir::ParserAtom::Atom(Atom::Number(s)) => ResolvedAtom::Number(*s),
            hir::ParserAtom::Atom(Atom::Char(s)) => ResolvedAtom::Char(*s),
            hir::ParserAtom::Single => ResolvedAtom::Single,
            hir::ParserAtom::Block(b) => ResolvedAtom::Block(*b),
            hir::ParserAtom::Atom(Atom::Field(f)) => {
                let (id, kind) =
                    hir::refs::resolve_var_ref(db, expr_id.0, *f)?.ok_or(SilencedError)?;
                match kind {
                    hir::refs::VarType::ParserDef => ResolvedAtom::ParserDef(hir::ParserDefId(id)),
                    hir::refs::VarType::Value => {
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
}
