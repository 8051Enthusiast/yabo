use enumflags2::{bitflags, BitFlags};
#[bitflags]
#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum NeededBy {
    Len = 1 << 0,
    Val = 1 << 1,
    Backtrack = 1 << 2,
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

    pub fn diag(diag: RequirementSet) -> Self {
        let mut ret = Self::default();
        for (i, val) in Self::id().iter_cols().enumerate() {
            if diag.contains(val) {
                ret.0[i] = val;
            }
        }
        ret
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

    /// calculate the equivalent of target * self for any target consisting of
    /// three columns corresponding to [NeededBy::Len, NeededBy::Val, NeededBy::Backtrack]
    pub fn pullback<'a, T>(self, target: &'a [T; 3]) -> [T; 3]
    where
        T: Default + std::ops::BitOrAssign<&'a T>,
    {
        let mut ret = <[T; 3]>::default();
        for (i, col) in self.iter_cols().enumerate() {
            for (j, val) in Self::id().iter_cols().enumerate() {
                if col.contains(val) {
                    ret[i] |= &target[j];
                }
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

impl std::fmt::Display for NeededBy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NeededBy::Len => write!(f, "len"),
            NeededBy::Val => write!(f, "val"),
            NeededBy::Backtrack => write!(f, "backtrack"),
        }
    }
}

impl std::fmt::Display for RequirementMatrix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, bit) in Self::id().iter_cols().enumerate() {
            if i != 0 {
                writeln!(f)?;
            }
            for column in self.iter_cols() {
                if column.contains(bit) {
                    write!(f, "1")?;
                } else {
                    write!(f, "0")?;
                }
            }
        }
        Ok(())
    }
}
