use std::fmt::Display;

use yaboc_base::{databased_display::DatabasedDisplay, dbwrite};

use crate::RequirementMatrix;

use super::{Dependents, SubValue};

impl<DB: Dependents + ?Sized> DatabasedDisplay<DB> for SubValue {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self.kind {
            super::SubValueKind::Bt => dbwrite!(f, db, "bt({})", &self.id),
            super::SubValueKind::Val => dbwrite!(f, db, "val({})", &self.id),
            super::SubValueKind::Front => dbwrite!(f, db, "front({})", &self.id),
            super::SubValueKind::Back => dbwrite!(f, db, "back({})", &self.id),
        }
    }
}

impl Display for RequirementMatrix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, bit) in Self::id().iter_cols().enumerate() {
            if i != 0 {
                writeln!(f)?;
            }
            for column in self.iter_cols() {
                if column.contains(bit) {
                    write!(f, "1")?;
                } else {
                    write!(f, "0")?;
                }
            }
        }
        Ok(())
    }
}
