use std::{collections::HashSet, error::Error};

use crate::source::Span;

pub type SResult<T> = Result<T, SilencedError>;
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct SilencedError;

impl std::fmt::Display for SilencedError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "SilencedError")
    }
}

impl Error for SilencedError {}

pub trait Silencable {
    type Out;
    fn silence(self) -> Self::Out;
}

impl<T, E: Silencable> Silencable for Result<T, E> {
    type Out = Result<T, SilencedError>;
    fn silence(self) -> Self::Out {
        self.map_err(|_| SilencedError)
    }
}

impl<E: Into<SilencedError>> Silencable for E {
    type Out = SilencedError;
    fn silence(self) -> Self::Out {
        self.into()
    }
}

impl<E: Into<SilencedError>> Silencable for Vec<E> {
    type Out = SilencedError;
    fn silence(self) -> Self::Out {
        SilencedError
    }
}

pub trait ErrorCollector<E> {
    fn with_error<T, F: FnOnce() -> Result<T, E>>(&mut self, f: F) -> Option<T>;
}

impl<E> ErrorCollector<E> for Vec<E> {
    fn with_error<T, F: FnOnce() -> Result<T, E>>(&mut self, f: F) -> Option<T> {
        match f() {
            Ok(t) => Some(t),
            Err(e) => {
                self.push(e);
                None
            }
        }
    }
}

impl<E: Eq + std::hash::Hash> ErrorCollector<E> for HashSet<E> {
    fn with_error<T, F: FnOnce() -> Result<T, E>>(&mut self, f: F) -> Option<T> {
        match f() {
            Ok(t) => Some(t),
            Err(e) => {
                self.insert(e);
                None
            }
        }
    }
}

impl<E: Ord> ErrorCollector<E> for std::collections::BTreeSet<E> {
    fn with_error<T, F: FnOnce() -> Result<T, E>>(&mut self, f: F) -> Option<T> {
        match f() {
            Ok(t) => Some(t),
            Err(e) => {
                self.insert(e);
                None
            }
        }
    }
}

#[macro_export]
macro_rules! error_type {
    {$name:ident($typ:ty) in $p:path} => {
        #[derive(Clone, Debug, PartialEq, Eq, Default, Hash)]
        pub struct $name {
            inner: $typ,
        }

        impl $name {
            #[warn(dead_code)]
            pub(in $p) fn use_error(self) -> $typ {
                self.inner
            }
        }

        impl $crate::error::Silencable for $name {
            type Out = $crate::error::SilencedError;
            fn silence(self) -> Self::Out {
                $crate::error::SilencedError
            }
        }
    }
}

pub type Report = ariadne::Report<Span>;