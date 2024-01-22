use hir::ExprId;
use yaboc_ast::expr::{FieldAccessMode, WiggleKind};
use yaboc_expr::{FetchKindData, ShapedData};
use yaboc_resolve::expr::{ValBinOp, ValUnOp};

use super::*;
use yaboc_expr::Expression;

pub fn expr_reqs(
    db: &dyn Dependents,
    expr_id: ExprId,
) -> SResult<Arc<ShapedData<Vec<ExprDepData>, Resolved>>> {
    let expr = Resolved::expr_with_data::<BacktrackStatus>(db, expr_id)?;
    let expr_ref = expr.take_ref();
    let mut ret: ShapedData<Vec<ExprDepData>, Resolved> = expr
        .take_ref()
        .data
        .map(|x| ExprDepData {
            reqs: RequirementMatrix::id(),
            bt: *x,
        })
        .collect();
    let val_to_val = RequirementMatrix::outer(NeededBy::Val.into(), NeededBy::Val.into());
    let bt_to_bt = RequirementMatrix::outer(NeededBy::Backtrack.into(), NeededBy::Backtrack.into());
    let bt_to_val = RequirementMatrix::outer(NeededBy::Val.into(), NeededBy::Backtrack.into());
    let val_to_bt = RequirementMatrix::outer(NeededBy::Backtrack.into(), NeededBy::Val.into());
    for (idx, (part, bt)) in expr_ref.iter_parts_with_idx().rev() {
        let current = ret[idx].reqs;
        let mut mat = val_to_val;
        if bt.has_backtracked() {
            mat |= bt_to_bt;
        }
        ret[idx].reqs = mat * current;
        match part {
            ExprHead::Niladic(_) => {}
            ExprHead::Monadic(op, inner) => {
                use ValUnOp::*;
                let mut inner_mat = RequirementMatrix::id();
                match op {
                    EvalFun if expr.data[inner].can_backtrack() => {
                        // if the inner function can backtrack,
                        // we need to know its value to know whether to backtrack
                        inner_mat |= bt_to_val
                    }
                    Wiggle(_, WiggleKind::If) | Dot(_, FieldAccessMode::Backtrack) => {
                        // this checks the inner value, which means we need to know
                        // the value to know whether to backtrack
                        inner_mat |= bt_to_val
                    }
                    BtMark(_) | Wiggle(..) | Dot(..) | Size | GetAddr | Not | Neg | EvalFun => (),
                }
                ret[inner].reqs = inner_mat * current;
            }
            ExprHead::Dyadic(op, [lhs, rhs]) => {
                use ValBinOp::*;
                let mut lhs_mat @ mut rhs_mat = RequirementMatrix::id();
                match op {
                    ParserApply if expr.data[rhs].can_backtrack() => {
                        // if the rhs is backtrackable, we need to know both values
                        // to evaluate
                        lhs_mat |= bt_to_val;
                        rhs_mat |= bt_to_val;
                    }
                    Else => {
                        // to know the value, we need to know whether the lhs backtracks
                        lhs_mat |= val_to_bt;
                    }
                    Then => {
                        // the value of the lhs is ignored, so we only need backtracking
                        lhs_mat = bt_to_bt;
                    }
                    And | Xor | Or | LesserEq | Lesser | GreaterEq | Greater | Uneq | Equals
                    | ShiftR | ShiftL | Minus | Plus | Div | Modulo | Mul | ParserApply => {}
                }
                ret[lhs].reqs = lhs_mat * current;
                ret[rhs].reqs = rhs_mat * current;
            }
            ExprHead::Variadic(_, args) => {
                for &arg in args.iter() {
                    ret[arg].reqs = current;
                }
            }
        }
    }
    Ok(Arc::new(ret))
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct ExprDepData {
    pub reqs: RequirementMatrix,
    pub bt: BacktrackStatus,
}

impl<DB: Dependents + ?Sized> FetchKindData<ExprDepData, ExprId, DB> for Resolved {
    type Err = SilencedError;

    type Data = Arc<ShapedData<Vec<ExprDepData>, Resolved>>;

    fn fetch_kind_data(db: &DB, id: ExprId) -> Result<Self::Data, Self::Err> {
        db.expr_reqs(id)
    }
}
