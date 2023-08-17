use yaboc_ast::expr::{WiggleKind, BtMarkKind};
use yaboc_base::{error::SResult, interner::DefId};
use yaboc_expr::{ExprHead, Expression, FetchExpr, TakeRef};
use yaboc_hir::{ExprId, HirNode};
use yaboc_hir_types::FullTypeId;
use yaboc_resolve::expr::{Resolved, ResolvedAtom};
use yaboc_resolve::expr::{ValBinOp, ValUnOp};
use yaboc_types::Type;

use crate::Dependents;

fn expr_backtrack_status(db: &dyn Dependents, expr: ExprId) -> SResult<(bool, bool)> {
    Resolved::expr_with_data::<FullTypeId>(db, expr)?
        .take_ref()
        .try_fold(|(head, ty)| {
            let ty = db.lookup_intern_type(db.least_deref_type(*ty)?);
            let is_parser = matches!(ty, Type::ParserArg { .. });
            Ok(match head {
                ExprHead::Niladic(
                    ResolvedAtom::Val(_) | ResolvedAtom::Captured(_) | ResolvedAtom::ParserDef(_),
                ) => (false, false),
                ExprHead::Niladic(ResolvedAtom::Block(b)) => {
                    (false, db.can_backtrack(b.0).unwrap_or(false))
                }
                ExprHead::Niladic(_) => (false, false),
                ExprHead::Monadic(ValUnOp::Dot(_, acc), (will, can)) => {
                    (will || acc.can_backtrack(), can)
                }
                ExprHead::Monadic(ValUnOp::Wiggle(_, kind), (will, can)) if !is_parser => {
                    (will || kind == WiggleKind::If, can)
                }
                ExprHead::Monadic(ValUnOp::Wiggle(_, kind), (will, can)) if is_parser => {
                    (will, can || kind == WiggleKind::If)
                }
                ExprHead::Monadic(ValUnOp::BtMark(BtMarkKind::KeepBt), (will, _)) => (will, true),
                ExprHead::Monadic(ValUnOp::EvalFun, (will, can)) => (will, can),
                ExprHead::Monadic(_, (will, _)) => (will, false),
                ExprHead::Dyadic(ValBinOp::ParserApply, [(will_left, _), (will_right, can)]) => {
                    (will_left || will_right || can, true)
                }
                ExprHead::Dyadic(ValBinOp::Else, [(_, can_left), (will, can_right)]) => {
                    (will, can_left || can_right)
                }
                ExprHead::Dyadic(_, [(will_left, _), (will_right, _)]) => {
                    (will_left || will_right, false)
                }
                ExprHead::Variadic(_, inner) => (inner.iter().any(|(will, _)| *will), inner[0].1),
            })
        })
}

pub fn can_backtrack(db: &dyn Dependents, def: DefId) -> SResult<bool> {
    let result = match db.hir_node(def)? {
        HirNode::Expr(e) => {
            let (will, _) = expr_backtrack_status(db, e.id)?;
            will
        }
        HirNode::Let(l) => {
            let (will, _) = expr_backtrack_status(db, l.expr)?;
            will
        }
        HirNode::Parse(p) => {
            let (will, can) = expr_backtrack_status(db, p.expr)?;
            will || can
        }
        HirNode::Choice(c) => db.can_backtrack(c.subcontexts.last().unwrap().0)?,
        HirNode::ChoiceIndirection(_) => false,
        HirNode::Context(c) => c
            .children
            .iter()
            .any(|c| db.can_backtrack(*c).unwrap_or(false)),
        HirNode::Block(b) => db.can_backtrack(b.root_context.0)?,
        _ => false,
    };
    Ok(result)
}
