use yaboc_ast::expr::{
    Dyadic, ExpressionHead, Monadic, OpWithData, ValBinOp, ValUnOp, Variadic, WiggleKind,
};
use yaboc_base::{error::SResult, interner::DefId};
use yaboc_hir::{ExprId, HirNode};
use yaboc_resolve::expr::ResolvedAtom;

use crate::Dependents;

fn expr_backtrack_status(db: &dyn Dependents, expr: ExprId) -> SResult<(bool, bool)> {
    Ok(
        db.parser_expr_at(expr)?.fold(&mut |h| match h {
            ExpressionHead::Niladic(OpWithData {
                inner:
                    ResolvedAtom::Val(_, q)
                    | ResolvedAtom::Captured(_, q)
                    | ResolvedAtom::ParserDef(_, q),
                ..
            }) => (false, *q),
            ExpressionHead::Niladic(OpWithData {
                inner: ResolvedAtom::Block(b),
                ..
            }) => (false, db.can_backtrack(b.0).unwrap_or(false)),
            ExpressionHead::Niladic(_) => (false, false),
            ExpressionHead::Monadic(Monadic {
                op:
                    OpWithData {
                        inner: ValUnOp::Dot(_, q),
                        ..
                    },
                inner: (will, _),
            }) => (will || *q, true),
            ExpressionHead::Monadic(Monadic {
                op:
                    OpWithData {
                        inner: ValUnOp::Wiggle(_, kind),
                        ..
                    },
                inner: (will, can),
            }) => (will || *kind == WiggleKind::If, can),
            ExpressionHead::Monadic(Monadic {
                inner: (will, _), ..
            }) => (will, false),
            ExpressionHead::Dyadic(Dyadic {
                op:
                    OpWithData {
                        inner: ValBinOp::ParserApply,
                        ..
                    },
                inner: [(will_left, _), (will_right, can)],
            }) => (will_left || will_right || can, true),
            ExpressionHead::Dyadic(Dyadic {
                op:
                    OpWithData {
                        inner: ValBinOp::Else,
                        ..
                    },
                inner: [(_, can_left), (will, can_right)],
            }) => (will, can_left || can_right),
            ExpressionHead::Dyadic(Dyadic {
                op:
                    OpWithData {
                        inner: ValBinOp::Compose,
                        ..
                    },
                inner: [(will_left, can_left), (will_right, can_right)],
            }) => (will_left || will_right, can_left || can_right),
            ExpressionHead::Dyadic(Dyadic {
                inner: [(will_left, _), (will_right, _)],
                ..
            }) => (will_left || will_right, false),
            ExpressionHead::Variadic(Variadic { inner, .. }) => {
                (inner.iter().any(|(will, _)| *will), inner[0].1)
            }
        }),
    )
}

pub fn can_backtrack(db: &dyn Dependents, def: DefId) -> SResult<bool> {
    match db.hir_node(def)? {
        HirNode::Let(l) => {
            let (will, _) = expr_backtrack_status(db, l.expr)?;
            Ok(will)
        }
        HirNode::Parse(p) => {
            let (will, can) = expr_backtrack_status(db, p.expr)?;
            Ok(will || can)
        }
        HirNode::Choice(c) => db.can_backtrack(c.subcontexts.last().unwrap().0),
        HirNode::ChoiceIndirection(_) => Ok(false),
        HirNode::Context(c) => Ok(c
            .children
            .iter()
            .any(|c| db.can_backtrack(*c).unwrap_or(false))),
        HirNode::Block(b) => db.can_backtrack(b.root_context.0),
        _ => panic!("can backtrack can only be called on children of a context"),
    }
}
