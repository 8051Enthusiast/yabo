use yaboc_ast::expr::{
    Dyadic, ExpressionHead, Monadic, OpWithData, ValBinOp, ValUnOp, Variadic, WiggleKind,
};
use yaboc_base::{error::SResult, interner::DefId};
use yaboc_hir::{ExprId, HirNode};
use yaboc_resolve::expr::ResolvedAtom;
use yaboc_types::Type;

use crate::Dependents;

fn expr_backtrack_status(db: &dyn Dependents, expr: ExprId) -> SResult<(bool, bool)> {
    db.parser_expr_at(expr)?.try_fold(&mut |h| {
        let ty = h.root_data().0;
        let ty = db.lookup_intern_type(db.least_deref_type(ty)?);
        let is_parser = matches!(ty, Type::ParserArg { .. });
        Ok(match h {
            ExpressionHead::Niladic(OpWithData {
                inner:
                    ResolvedAtom::Val(_, q)
                    | ResolvedAtom::Captured(_, q)
                    | ResolvedAtom::ParserDef(_, q),
                ..
            }) => (false, q),
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
            }) => (will || q, true),
            ExpressionHead::Monadic(Monadic {
                op:
                    OpWithData {
                        inner: ValUnOp::Wiggle(_, kind),
                        ..
                    },
                inner: (will, can),
            }) if !is_parser => (will || kind == WiggleKind::If, can),
            ExpressionHead::Monadic(Monadic {
                op:
                    OpWithData {
                        inner: ValUnOp::Wiggle(_, kind),
                        ..
                    },
                inner: (will, can),
            }) if is_parser => (will, can || kind == WiggleKind::If),
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
                ..
            }) => unreachable!(),
            ExpressionHead::Dyadic(Dyadic {
                inner: [(will_left, _), (will_right, _)],
                ..
            }) => (will_left || will_right, false),
            ExpressionHead::Variadic(Variadic { inner, .. }) => {
                (inner.iter().any(|(will, _)| *will), inner[0].1)
            }
        })
    })
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
