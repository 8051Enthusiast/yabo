use bumpalo::Bump;
use fxhash::FxHashSet;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::ptr;

use crate::databased_display::DatabasedDisplay;

// Taken literally from https://lcnr.de/blog/low-effort-interner/, thank you lcnr!

// as the ZST is inside a private module, Uniq
// can only be constructed in this module.
#[derive(Copy, Clone, Debug)]
#[repr(transparent)]
pub struct Uniq<T: ?Sized>(pub private::PrivateZst, pub T);
impl<T: ?Sized> Deref for Uniq<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.1
    }
}
impl<T: ?Sized> PartialEq for Uniq<T> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self, other) // such fast
    }
}
impl<T: ?Sized> Eq for Uniq<T> {}
impl<T: ?Sized> PartialOrd for Uniq<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (self as *const Uniq<T>).partial_cmp(&(other as *const _))
    }
}
impl<T: ?Sized> Ord for Uniq<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self as *const Uniq<T>).cmp(&(other as *const _))
    }
}
impl<T: ?Sized> Hash for Uniq<T> {
    fn hash<H: Hasher>(&self, s: &mut H) {
        (self as *const Uniq<T>).hash(s) // much wow
    }
}

pub struct Interner<'a, T: ?Sized> {
    set: FxHashSet<&'a T>,
    arena: &'a Bump,
}

impl<'a, T: ?Sized> Interner<'a, T> {
    pub fn new(arena: &'a Bump) -> Interner<'a, T> {
        Interner {
            set: Default::default(),
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
            let value = self.arena.alloc(Uniq(private::PrivateZst, value));
            assert!(self.set.insert(value));
            value
        }
    }
}

// this is an extension from the blog post to intern slices
impl<'a, T: Eq + Hash + Copy> Interner<'a, [T]> {
    pub fn intern_slice(&mut self, slice: &[T]) -> &'a Uniq<[T]> {
        if let Some(value) = self.set.get(slice) {
            // same as above
            unsafe { std::mem::transmute::<&'a [T], &'a Uniq<[T]>>(value) }
        } else {
            let value = self.arena.alloc_slice_copy(slice);
            // we can not do the same as above since we can not
            // construct a Uniq<[T]> directly
            let uniq_value = unsafe { std::mem::transmute::<&'a [T], &'a Uniq<[T]>>(value) };
            assert!(self.set.insert(uniq_value));
            uniq_value
        }
    }
}

impl<DB, T: DatabasedDisplay<DB>> DatabasedDisplay<DB> for Uniq<T> {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        self.1.db_fmt(f, db)
    }
}

mod private {
    #[derive(Clone, Copy, Debug)]
    pub struct PrivateZst;
}
