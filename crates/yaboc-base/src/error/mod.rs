pub mod diagnostic;

use std::{collections::HashSet, convert::Infallible, error::Error, sync::Arc};

pub type SResult<T> = Result<T, SilencedError>;

// in debug mode we want to capture backtraces on silent errors
// since they might indicate bugs in the compiler, but in release
// capture backtraces is expensive so we don't do it
#[cfg(debug_assertions)]
#[derive(Clone, Debug)]
pub struct SilencedError(Arc<std::backtrace::Backtrace>);

#[cfg(not(debug_assertions))]
#[derive(Clone, Debug)]
pub struct SilencedError(());

impl SilencedError {
    pub fn new() -> Self {
        Self::default()
    }
}

impl From<Infallible> for SilencedError {
    fn from(never: Infallible) -> Self {
        match never {}
    }
}

#[cfg(debug_assertions)]
impl Default for SilencedError {
    fn default() -> Self {
        Self(Arc::new(std::backtrace::Backtrace::capture()))
    }
}

#[cfg(not(debug_assertions))]
impl Default for SilencedError {
    fn default() -> Self {
        Self(())
    }
}

impl std::fmt::Display for SilencedError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:#?}", self.0)
    }
}

impl Error for SilencedError {}

impl std::cmp::PartialEq for SilencedError {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl std::cmp::Eq for SilencedError {}

impl std::hash::Hash for SilencedError {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        0.hash(state);
    }
}

impl Ord for SilencedError {
    fn cmp(&self, _: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
    }
}

impl PartialOrd for SilencedError {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub trait Silencable {
    type Out;
    fn silence(self) -> Self::Out;
}

impl<T, E: Silencable<Out = SilencedError>> Silencable for Result<T, E>
where
    E: std::fmt::Debug,
{
    type Out = Result<T, SilencedError>;
    fn silence(self) -> Self::Out {
        self.map_err(|e| e.silence())
    }
}

impl<E: Into<SilencedError>> Silencable for E {
    type Out = SilencedError;
    fn silence(self) -> Self::Out {
        self.into()
    }
}

impl<E: Silencable<Out = SilencedError>> Silencable for Vec<E> {
    type Out = SilencedError;
    fn silence(mut self) -> Self::Out {
        match self.pop() {
            Some(e) => e.silence(),
            None => SilencedError::default(),
        }
    }
}

impl<E: Silencable<Out = SilencedError> + Clone> Silencable for Arc<[E]> {
    type Out = SilencedError;
    fn silence(self) -> Self::Out {
        match self.first().cloned() {
            Some(e) => e.silence(),
            None => SilencedError::default(),
        }
    }
}

pub trait IsSilenced: From<SilencedError> {
    fn is_silenced(&self) -> bool;
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

            pub fn has_errors(&self) -> bool {
                !self.inner.is_empty()
            }
        }

        impl $crate::error::Silencable for $name {
            type Out = $crate::error::SilencedError;
            fn silence(self) -> Self::Out {
                $crate::error::SilencedError::new()
            }
        }
    }
}

pub type Report = diagnostic::Diagnostic;
