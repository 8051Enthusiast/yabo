use bumpalo::Bump;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::ptr;

// Taken literally from https://lcnr.de/blog/low-effort-interner/, thank you lcnr!

// as the `T` field is private, this struct
// can only be constructed in this module.
#[derive(Copy, Clone, Debug)]
#[repr(transparent)]
pub struct Uniq<T>(pub T, pub private::PrivateZst);
impl<T> Uniq<T> {
    pub fn value(self) -> T {
        self.0
    }
}
impl<T> Deref for Uniq<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.0
    }
}
impl<T> PartialEq for Uniq<T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self, other) // such fast
    }
}
impl<T> Eq for Uniq<T> {}
impl<T> PartialOrd for Uniq<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (self as *const Uniq<T>).partial_cmp(&(other as *const _))
    }
}
impl<T> Ord for Uniq<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self as *const Uniq<T>).cmp(&(other as *const _))
    }
}
impl<T> Hash for Uniq<T> {
    fn hash<H: Hasher>(&self, s: &mut H) {
        (self as *const Uniq<T>).hash(s) // much wow
    }
}

pub struct Interner<'a, T> {
    set: HashSet<&'a T>,
    arena: &'a Bump,
}

impl<'a, T> Interner<'a, T> {
    pub fn new(arena: &'a Bump) -> Interner<'a, T> {
        Interner {
            set: HashSet::new(),
            arena,
        }
    }
}

impl<'a, T: Eq + Hash> Interner<'a, T> {
    pub fn intern(&mut self, value: T) -> &'a Uniq<T> {
        if let Some(value) = self.set.get(&value) {
            // 👻 `Uniq` is `#[repr(transparent)]` so this is sound 👻
            unsafe { std::mem::transmute::<&'a T, &'a Uniq<T>>(value) }
        } else {
            let value = self.arena.alloc(Uniq(value, private::PrivateZst));
            assert!(self.set.insert(value));
            value
        }
    }
}

mod private {
    #[derive(Clone, Copy, Debug)]
    pub struct PrivateZst;
}
