use crate::{
    databased_display::DatabasedDisplay,
    interner::{Interner, Regex},
    source::IndexSpanned,
};

impl<T, DB: Interner + ?Sized> DatabasedDisplay<DB> for IndexSpanned<T>
where
    T: DatabasedDisplay<DB>,
{
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        self.atom.db_fmt(f, db)
    }
}

impl<DB: Interner + ?Sized> DatabasedDisplay<DB> for Regex {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        let regex = db.lookup_intern_regex(*self);
        match regex.kind {
            crate::interner::RegexKind::Regular => write!(f, "/{}/", regex.regex),
            crate::interner::RegexKind::Hexagex => write!(f, "h/{}/", regex.regex),
        }
    }
}
