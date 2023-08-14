use yaboc_base::{
    databased_display::DatabasedDisplay, dbwrite, hash::StableHash, interner::FieldName,
};

use crate::{expr::Atom, ArrayKind, Asts, ConstraintAtom};

impl<DB: Asts + ?Sized> StableHash<DB> for ArrayKind {
    fn update_hash(&self, state: &mut sha2::Sha256, db: &DB) {
        match self {
            ArrayKind::Each => 1,
        }
        .update_hash(state, db)
    }
}

impl<DB: Asts + ?Sized> DatabasedDisplay<DB> for Atom {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            Atom::Field(FieldName::Ident(id)) => dbwrite!(f, db, "{}", id),
            Atom::Field(FieldName::Return) => write!(f, "return"),
            Atom::Number(a) => write!(f, "{a}"),
            Atom::Char(a) => write!(f, "'{a}'"),
            Atom::Bool(a) => write!(f, "{a}"),
        }
    }
}

impl<DB: Asts + ?Sized> DatabasedDisplay<DB> for ConstraintAtom {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            ConstraintAtom::Atom(a) => a.db_fmt(f, db),
            ConstraintAtom::Range(start, end) => {
                dbwrite!(f, db, "{}..{}", start, end)
            }
            ConstraintAtom::NotEof => write!(f, "!eof"),
        }
    }
}
