use crate::{databased_display::DatabasedDisplay, interner::Interner, source::IndexSpanned};

impl<T, DB: Interner + ?Sized> DatabasedDisplay<DB> for IndexSpanned<T>
where
    T: DatabasedDisplay<DB>,
{
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        self.atom.db_fmt(f, db)
    }
}
