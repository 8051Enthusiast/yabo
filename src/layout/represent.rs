use crate::{databased_display::DatabasedDisplay, dbwrite};

use super::{ILayout, Layout, MonoLayout};
use crate::absint::{AbsInt, Arg};

impl<'a, DB: AbsInt> DatabasedDisplay<DB> for ILayout<'a> {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match &self.layout {
            Layout::None => write!(f, "<null>"),
            Layout::Mono(d, ty) => match d {
                MonoLayout::Primitive(p) => p.db_fmt(f, db),
                MonoLayout::Pointer => write!(f, "ptr"),
                MonoLayout::Single => write!(f, "simple"),
                MonoLayout::Nominal(_, args) => {
                    dbwrite!(f, db, "nominal[{}](", ty)?;
                    for (i, (arg, layout)) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        dbwrite!(f, db, "{}: {}", arg, layout)?;
                    }
                    write!(f, ")")
                }
                MonoLayout::NominalParser(_) => {
                    dbwrite!(f, db, "nominal-parser[{}]", ty)
                }
                MonoLayout::Block(_, vars) => {
                    dbwrite!(f, db, "block[{}]{{", ty)?;
                    for (i, (var, layout)) in vars.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        dbwrite!(f, db, "{}: {}", var, layout)?;
                    }
                    write!(f, "}}")
                }
                MonoLayout::BlockParser(_, captures) => {
                    dbwrite!(f, db, "block-parser[{}]{{", ty)?;
                    for (i, (capture, layout)) in captures.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        dbwrite!(f, db, "{}: {}", capture, layout)?;
                    }
                    write!(f, "}}")
                }
                MonoLayout::ComposedParser(left, right) => {
                    dbwrite!(f, db, "composed[{}]({} |> {})", ty, left, right)
                }
            },
            Layout::Multi(subs) => {
                for (i, layout) in subs.layouts.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    dbwrite!(f, db, "{}", layout)?;
                }
                Ok(())
            }
        }
    }
}

impl<DB: AbsInt> DatabasedDisplay<DB> for Arg {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            Arg::Named(n) => n.db_fmt(f, db),
            Arg::From => write!(f, "<from>"),
        }
    }
}
