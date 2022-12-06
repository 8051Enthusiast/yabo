use yaboc_base::databased_display::DatabasedDisplay;

use crate::{Arg, AbsInt};

impl<DB: AbsInt + ?Sized> DatabasedDisplay<DB> for Arg {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            Arg::Named(n) => n.db_fmt(f, db),
            Arg::From => write!(f, "<from>"),
        }
    }
}
