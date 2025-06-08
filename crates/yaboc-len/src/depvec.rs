use std::ops::Index;

use smallvec::SmallVec;
use yaboc_req::{NeededBy, RequirementSet};

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct SmallBitVec(SmallVec<[u64; 2]>);

impl Index<usize> for SmallBitVec {
    type Output = bool;

    fn index(&self, index: usize) -> &Self::Output {
        let (word, bit) = (index / 64, index % 64);
        let Some(w) = self.0.get(word) else {
            return &false;
        };
        if (w >> bit) & 1 != 0 {
            &true
        } else {
            &false
        }
    }
}

impl SmallBitVec {
    pub fn ones(bits: usize) -> Self {
        let len = bits.div_ceil(64);
        let mut words = SmallVec::from_elem(u64::MAX, len);
        if bits % 64 != 0 {
            let mask = (1 << (bits % 64)) - 1;
            *words.last_mut().unwrap() &= mask;
        }
        SmallBitVec(words)
    }

    pub fn iter_ones(&self) -> impl Iterator<Item = usize> + '_ {
        (0..64 * self.0.len()).filter(move |i| self[*i])
    }

    pub fn zeroes(_: usize) -> Self {
        SmallBitVec::default()
    }

    pub fn or(&self, other: &Self) -> Self {
        let (va, vb) = if self.0.len() > other.0.len() {
            (self, other)
        } else {
            (other, self)
        };
        let mut words = SmallVec::with_capacity(va.0.len());
        words.extend(self.0.iter().zip(other.0.iter()).map(|(a, b)| a | b));
        words.extend_from_slice(&va.0[vb.0.len()..]);
        SmallBitVec(words)
    }

    pub fn is_zero_below(&self, idx: usize) -> bool {
        (0..idx).all(|i| !self[i])
    }

    pub fn set(&mut self, idx: usize) {
        let (word, bit) = (idx / 64, idx % 64);
        if self.0.len() < word + 1 {
            self.0
                .extend(std::iter::repeat_n(0, word + 1 - self.0.len()));
        }
        self.0[word] |= 1 << bit;
    }

    pub fn truncate(&self, n: usize) -> Self {
        let len = n.div_ceil(64);
        let mut words = SmallVec::from_slice(&self.0[..len.min(self.0.len())]);
        if n % 64 != 0 {
            let mask = (1 << (n % 64)) - 1;
            if let Some(last) = words.last_mut() {
                *last &= mask;
            }
        }
        while words.last() == Some(&0) {
            words.pop();
        }
        words.shrink_to_fit();
        SmallBitVec(words)
    }

    pub fn bit_capacity(&self) -> usize {
        self.0.len() * 64
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct DepVec<const REVERSED: bool>(SmallBitVec);

impl<const REVERSED: bool> DepVec<REVERSED> {
    pub fn set_val(&mut self, idx: usize) {
        self.0.set(idx << 1);
    }

    pub fn set_len(&mut self, idx: usize) {
        self.0.set(idx << 1 | 1);
    }

    pub fn has_val(&self, idx: usize) -> bool {
        self.0[idx << 1]
    }

    pub fn has_len(&self, idx: usize) -> bool {
        self.0[idx << 1 | 1]
    }

    pub fn req(&self, idx: usize) -> RequirementSet {
        let mut ret = RequirementSet::empty();
        if self.has_val(idx) {
            ret |= NeededBy::Val;
        }
        if self.has_len(idx) {
            ret |= NeededBy::Len;
        }
        ret
    }

    pub fn or(&self, other: &Self) -> Self {
        DepVec(self.0.or(&other.0))
    }

    pub fn is_zero_below(&self, idx: usize) -> bool {
        self.0.is_zero_below(idx << 1)
    }

    pub fn array_deps() -> Self {
        let mut ret = DepVec::default();
        ret.set_val(0);
        ret.set_len(1);
        ret
    }

    pub fn truncate(&mut self, size: usize) {
        self.0 = self.0.truncate(size << 1);
    }

    pub fn iter_ones(&self) -> impl Iterator<Item = (usize, NeededBy)> + '_ {
        self.0.iter_ones().map(|i| {
            (
                i >> 1,
                if i & 1 == 0 {
                    NeededBy::Val
                } else {
                    NeededBy::Len
                },
            )
        })
    }
}

impl LevelDepVec {
    pub fn split_at(&self, total: usize, arg_count: usize) -> (LevelDepVec, IndexDepVec) {
        let idx = total - arg_count;
        let mut first_ret = self.clone();
        let mut second_ret = IndexDepVec::default();
        for i in idx..total {
            if first_ret.has_val(i) {
                second_ret.set_val(total - 1 - i);
            }
            if first_ret.has_len(i) {
                second_ret.set_len(total - 1 - i);
            }
        }
        first_ret.truncate(idx);
        (first_ret, second_ret)
    }
}

impl<const REVERSED: bool> std::ops::BitOrAssign<&DepVec<REVERSED>> for DepVec<REVERSED> {
    fn bitor_assign(&mut self, rhs: &Self) {
        self.0 = self.0.or(&rhs.0);
    }
}

pub type LevelDepVec = DepVec<false>;
pub type IndexDepVec = DepVec<true>;

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ArgDeps(pub [LevelDepVec; 3]);

impl ArgDeps {
    pub fn len(&mut self) -> &mut LevelDepVec {
        &mut self.0[0]
    }
    pub fn val(&mut self) -> &mut LevelDepVec {
        &mut self.0[1]
    }
    pub fn backtrack(&mut self) -> &mut LevelDepVec {
        &mut self.0[2]
    }
}

impl std::ops::BitOrAssign<&ArgDeps> for ArgDeps {
    fn bitor_assign(&mut self, rhs: &Self) {
        for (a, b) in self.0.iter_mut().zip(rhs.0.iter()) {
            *a |= b;
        }
    }
}

impl std::ops::BitOrAssign<&[LevelDepVec; 3]> for ArgDeps {
    fn bitor_assign(&mut self, rhs: &[LevelDepVec; 3]) {
        for (a, b) in self.0.iter_mut().zip(rhs.iter()) {
            *a |= b;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn bitvec() {
        let a = SmallBitVec::ones(100);
        let b = SmallBitVec::ones(64);
        assert!(b[63]);
        assert!(!b[64]);
        let c = a.or(&b);
        let d = b.or(&a);
        assert!(c[99]);
        assert!(!c[100]);
        assert_eq!(c, a);
        assert_eq!(c, d);
        let e = d.truncate(65);
        assert!(e[64]);
        assert!(!e[65]);
        let mut f = e.truncate(64);
        assert_eq!(f, b);
        f.set(128);
        assert_ne!(f, b);
        assert!(f[128]);
        let f = f.truncate(128);
        assert_eq!(f, b);
    }
}
