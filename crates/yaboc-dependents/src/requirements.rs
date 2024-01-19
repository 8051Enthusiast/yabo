use enumflags2::BitFlags;
use hir::ExprId;
use yaboc_ast::expr::{FieldAccessMode, WiggleKind};
use yaboc_expr::{FetchKindData, ShapedData};
use yaboc_resolve::expr::{ValBinOp, ValUnOp};

use super::*;
use yaboc_expr::Expression;

#[bitflags]
#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum NeededBy {
    Len = 1 << 0,
    Val = 1 << 1,
    Backtrack = 1 << 2,
}

impl std::fmt::Display for NeededBy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NeededBy::Len => write!(f, "len"),
            NeededBy::Val => write!(f, "val"),
            NeededBy::Backtrack => write!(f, "backtrack"),
        }
    }
}

pub type RequirementSet = BitFlags<NeededBy>;

// stored in column-major order
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct RequirementMatrix([RequirementSet; 3]);

impl RequirementMatrix {
    pub fn new(len: RequirementSet, val: RequirementSet, backtrack: RequirementSet) -> Self {
        Self([len, val, backtrack])
    }

    pub fn id() -> Self {
        Self::new(
            NeededBy::Len.into(),
            NeededBy::Val.into(),
            NeededBy::Backtrack.into(),
        )
    }

    pub fn zero() -> Self {
        Self::default()
    }

    /// calculates the boolean matrix operation self * needed_by
    pub fn needs_when(self, needed_by: RequirementSet) -> RequirementSet {
        let mut ret = RequirementSet::empty();
        for (i, val) in Self::id().iter_cols().enumerate() {
            if needed_by.contains(val) {
                ret |= self.0[i];
            }
        }
        ret
    }

    pub fn outer(left: RequirementSet, right: RequirementSet) -> Self {
        let mut ret = Self::default();
        for (i, val) in Self::id().iter_cols().enumerate() {
            if right.contains(val) {
                ret.0[i] |= left;
            }
        }
        ret
    }

    pub fn iter_cols(self) -> impl Iterator<Item = RequirementSet> {
        self.0.into_iter()
    }
}

impl std::ops::Mul<RequirementSet> for RequirementMatrix {
    type Output = RequirementSet;
    fn mul(self, rhs: RequirementSet) -> Self::Output {
        self.needs_when(rhs)
    }
}

impl std::ops::Mul<RequirementMatrix> for RequirementMatrix {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        let mut ret = Self::default();
        for (i, col) in rhs.iter_cols().enumerate() {
            ret.0[i] = self * col;
        }
        ret
    }
}

impl std::ops::BitOr for RequirementMatrix {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Self([
            self.0[0] | rhs.0[0],
            self.0[1] | rhs.0[1],
            self.0[2] | rhs.0[2],
        ])
    }
}

impl std::ops::BitOrAssign for RequirementMatrix {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0[0] |= rhs.0[0];
        self.0[1] |= rhs.0[1];
        self.0[2] |= rhs.0[2];
    }
}

pub fn expr_reqs(
    db: &dyn Dependents,
    expr_id: ExprId,
) -> SResult<Arc<ShapedData<Vec<RequirementMatrix>, Resolved>>> {
    let expr = Resolved::expr_with_data::<BacktrackStatus>(db, expr_id)?;
    let expr_ref = expr.take_ref();
    let mut ret = ShapedData::from_raw_data(vec![RequirementMatrix::id(); expr_ref.len()]);
    let val_to_val = RequirementMatrix::outer(NeededBy::Val.into(), NeededBy::Val.into());
    let bt_to_bt = RequirementMatrix::outer(NeededBy::Backtrack.into(), NeededBy::Backtrack.into());
    let bt_to_val = RequirementMatrix::outer(NeededBy::Val.into(), NeededBy::Backtrack.into());
    let val_to_bt = RequirementMatrix::outer(NeededBy::Backtrack.into(), NeededBy::Val.into());
    for (idx, (part, bt)) in expr_ref.iter_parts_with_idx().rev() {
        let current = ret[idx];
        let mut mat = val_to_val;
        if bt.has_backtracked() {
            mat |= bt_to_bt;
        }
        ret[idx] = mat * current;
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
                ret[inner] = inner_mat * current;
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
                ret[lhs] = lhs_mat * current;
                ret[rhs] = rhs_mat * current;
            }
            ExprHead::Variadic(_, args) => {
                for &arg in args.iter() {
                    ret[arg] = current;
                }
            }
        }
    }
    Ok(Arc::new(ret))
}

impl<DB: Dependents + ?Sized> FetchKindData<RequirementMatrix, ExprId, DB> for Resolved {
    type Err = SilencedError;

    type Data = Arc<ShapedData<Vec<RequirementMatrix>, Resolved>>;

    fn fetch_kind_data(db: &DB, id: ExprId) -> Result<Self::Data, Self::Err> {
        db.expr_reqs(id)
    }
}
