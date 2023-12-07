use yaboc_ast::expr::{BtMarkKind, WiggleKind};
use yaboc_base::{error::SResult, interner::DefId};
use yaboc_expr::{ExprHead, Expression, FetchExpr, TakeRef};
use yaboc_hir::{ExprId, HirNode};
use yaboc_hir_types::FullTypeId;
use yaboc_resolve::expr::{Origin, Resolved, ResolvedAtom, ValVarOp};
use yaboc_resolve::expr::{ValBinOp, ValUnOp};
use yaboc_types::Type;

use crate::Dependents;

#[derive(Clone, Copy, Debug)]
pub struct BacktrackStatus(u8);

impl BacktrackStatus {
    fn eval(self) -> Self {
        Self(self.0 >> 1 | (self.0 & 1))
    }

    fn has_backtracked(self) -> bool {
        self.0 & 1 != 0
    }

    fn backtrack_if(self, s: bool) -> Self {
        Self((s as u8) | self.0)
    }

    fn can_backtrack(self) -> bool {
        self.0 & 2 != 0
    }

    fn can_backtrack_if(self, s: bool) -> Self {
        Self(self.0 | (s as u8) << 1)
    }

    fn combine_then(self, then: Self) -> Self {
        Self(then.0 | (self.0 & 1))
    }

    fn combine_else(self, else_: Self) -> Self {
        Self(else_.0 | (self.0 & !1))
    }

    fn combine_compose(self, others: &[Self]) -> Self {
        // for a desugared compose application, we do not require the backtrack
        // annotation (?) on the operator itself, but allow the arguments to have
        // it.
        // however, their can_backtrack status only becomes effective if both
        // the function and the parser is applied, hence the shift left by 2
        let arg_can_backtrack = others.iter().any(|s| s.can_backtrack());
        let has_backtracked = others.iter().any(|s| s.has_backtracked());
        self.backtrack_if(has_backtracked) | Self((arg_can_backtrack as u8) << 2)
    }

    fn empty() -> Self {
        Self(0)
    }
}

impl std::ops::BitOr for BacktrackStatus {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

fn expr_backtrack_status(db: &dyn Dependents, expr: ExprId) -> SResult<BacktrackStatus> {
    Resolved::expr_with_data::<FullTypeId>(db, expr)?
        .take_ref()
        .try_fold(|(head, ty)| -> Result<BacktrackStatus, _> {
            let ty = db.lookup_intern_type(db.least_deref_type(*ty)?);
            let is_parser = matches!(ty, Type::ParserArg { .. });
            Ok(match head {
                ExprHead::Niladic(
                    ResolvedAtom::Val(_) | ResolvedAtom::Captured(_) | ResolvedAtom::ParserDef(_),
                ) => BacktrackStatus::empty(),
                ExprHead::Niladic(ResolvedAtom::Block(b, _)) => BacktrackStatus::empty()
                    .can_backtrack_if(db.can_backtrack(b.0).unwrap_or(false)),
                ExprHead::Niladic(ResolvedAtom::Regex(_)) => {
                    BacktrackStatus::empty().can_backtrack_if(true)
                }
                ExprHead::Niladic(_) => BacktrackStatus::empty(),
                ExprHead::Monadic(ValUnOp::Dot(_, acc), status) => {
                    status.backtrack_if(acc.can_backtrack())
                }
                ExprHead::Monadic(ValUnOp::Wiggle(_, kind), status) if !is_parser => {
                    status.backtrack_if(kind == WiggleKind::If)
                }
                ExprHead::Monadic(ValUnOp::Wiggle(_, kind), status) if is_parser => {
                    status.can_backtrack_if(kind == WiggleKind::If)
                }
                ExprHead::Monadic(ValUnOp::BtMark(mark), status) => {
                    status.can_backtrack_if(mark == BtMarkKind::KeepBt)
                }
                ExprHead::Monadic(ValUnOp::EvalFun, status) => status.eval(),
                ExprHead::Monadic(_, status) => status,
                ExprHead::Dyadic(ValBinOp::ParserApply, [lhs, rhs]) => lhs.eval() | rhs,
                ExprHead::Dyadic(ValBinOp::Else, [lhs, rhs]) => lhs.combine_else(rhs),
                ExprHead::Dyadic(ValBinOp::Then, [lhs, rhs]) => lhs.combine_then(rhs),
                ExprHead::Dyadic(_, [lhs, rhs]) => lhs | rhs,
                ExprHead::Variadic(ValVarOp::PartialApply(Origin::Compose), inner) => {
                    inner[0].combine_compose(&inner[1..])
                }
                ExprHead::Variadic(_, inner) => {
                    inner[0].backtrack_if(inner[1..].iter().any(|status| status.has_backtracked()))
                }
            })
        })
}

pub fn can_backtrack(db: &dyn Dependents, def: DefId) -> SResult<bool> {
    let result = match db.hir_node(def)? {
        HirNode::Expr(e) => expr_backtrack_status(db, e.id)?.has_backtracked(),
        HirNode::Let(l) => expr_backtrack_status(db, l.expr)?.has_backtracked(),
        HirNode::Parse(p) => expr_backtrack_status(db, p.expr)?.eval().has_backtracked(),
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
