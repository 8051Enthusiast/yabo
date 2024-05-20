use std::fmt::{self, Display};

#[macro_export]
macro_rules! dbwrite {
    ($f:expr, $db:expr, $st:literal, $($arg:expr),*$(,)?) => {
        write!($f, $st, $($crate::databased_display::DatabasedDisplay::db_wrap($arg, $db)),*)
    };
    ($f:expr, $db:expr, $st:literal) => {
        write!($f, $st)
    };
}

#[macro_export]
macro_rules! dbformat {
    ($db:expr, $st:literal, $($arg:expr),*$(,)?) => {
        format!($st, $($crate::databased_display::DatabasedDisplay::db_wrap($arg, $db)),*)
    };
    ($db:expr, $st:literal) => {
        format!($st)
    };
}

#[macro_export]
macro_rules! dbpanic {
    ($db:expr, $st:literal, $($arg:expr),*$(,)?) => {
        panic!($st, $($crate::databased_display::DatabasedDisplay::db_wrap($arg, $db)),*)
    };
    ($db:expr, $st:literal) => {
        panic!($st)
    };
}

#[macro_export]
macro_rules! dbprintln {
    ($db:expr, $st:literal, $($arg:expr),*$(,)?) => {
        println!($st, $($crate::databased_display::DatabasedDisplay::db_wrap($arg, $db)),*)
    };
    ($db:expr, $st:literal) => {
        println!($st)
    };
}

#[macro_export]
macro_rules! dbeprintln {
    ($db:expr, $st:literal, $($arg:expr),*$(,)?) => {
        eprintln!($st, $($crate::databased_display::DatabasedDisplay::db_wrap($arg, $db)),*)
    };
    ($db:expr, $st:literal) => {
        eprintln!($st)
    };
}

pub trait DatabasedDisplay<DB: ?Sized> {
    fn to_db_string(&self, db: &DB) -> String {
        dbformat!(db, "{}", self)
    }
    fn db_fmt(&self, f: &mut fmt::Formatter<'_>, db: &DB) -> fmt::Result;
    fn db_wrap<'a>(&'a self, db: &'a DB) -> DatabasedDisplayWrap<'a, DB, Self> {
        DatabasedDisplayWrap { inner: self, db }
    }
}

impl<DB: ?Sized, T: DatabasedDisplay<DB>> DatabasedDisplay<DB> for [T] {
    fn db_fmt(&self, f: &mut fmt::Formatter<'_>, db: &DB) -> fmt::Result {
        write!(f, "[")?;
        let mut iter = self.iter();
        if let Some(first) = iter.next() {
            write!(f, "{}", first.db_wrap(db))?;
            for elem in iter {
                write!(f, ", {}", elem.db_wrap(db))?;
            }
        }
        write!(f, "]")
    }
}

impl<DB: ?Sized, T: Display> DatabasedDisplay<DB> for T {
    fn db_fmt(&self, f: &mut fmt::Formatter<'_>, _: &DB) -> fmt::Result {
        write!(f, "{self}")
    }
}
pub struct DatabasedDisplayWrap<'a, DB: ?Sized, T: ?Sized + DatabasedDisplay<DB>> {
    pub(crate) inner: &'a T,
    pub(crate) db: &'a DB,
}

impl<'a, DB: ?Sized, T: ?Sized + DatabasedDisplay<DB>> fmt::Display
    for DatabasedDisplayWrap<'a, DB, T>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.db_fmt(f, self.db)
    }
}
