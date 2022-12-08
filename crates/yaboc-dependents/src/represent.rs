use yaboc_base::{databased_display::DatabasedDisplay, dbwrite};

use super::{Dependents, SubValue};

impl<DB: Dependents + ?Sized> DatabasedDisplay<DB> for SubValue {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self.kind {
            super::SubValueKind::Val => dbwrite!(f, db, "val({})", &self.id),
            super::SubValueKind::Front => dbwrite!(f, db, "front({})", &self.id),
            super::SubValueKind::Back => dbwrite!(f, db, "back({})", &self.id),
        }
    }
}
