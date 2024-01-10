use enumflags2::BitFlags;

use super::*;
use std;

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

    pub fn from_outer_product(left: RequirementSet, right: RequirementSet) -> Self {
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
