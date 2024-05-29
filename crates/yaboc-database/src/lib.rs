use yaboc_absint::AbsIntDatabase;
use yaboc_ast::AstDatabase;
use yaboc_base::{
    config::ConfigDatabase, error::Report, interner::InternerDatabase, source::FileDatabase,
};
use yaboc_constraint::ConstraintDatabase;
use yaboc_dependents::DependentsDatabase;
use yaboc_hir::HirDatabase;
use yaboc_hir_types::HirTypesDatabase;
use yaboc_layout::LayoutDatabase;
use yaboc_mir::MirDatabase;
use yaboc_resolve::ResolveDatabase;
use yaboc_types::TypeInternerDatabase;

pub const ERROR_FNS: &[fn(&YabocDatabase) -> Vec<Report>] = &[
    yaboc_ast::error::errors,
    yaboc_hir::error::errors,
    yaboc_resolve::error::errors,
    yaboc_dependents::error::errors,
    yaboc_hir_types::error::errors,
    yaboc_constraint::error::errors,
];

#[salsa::database(
    InternerDatabase,
    ConfigDatabase,
    AstDatabase,
    FileDatabase,
    HirDatabase,
    ResolveDatabase,
    TypeInternerDatabase,
    HirTypesDatabase,
    DependentsDatabase,
    ConstraintDatabase,
    MirDatabase,
    AbsIntDatabase,
    LayoutDatabase
)]
#[derive(Default)]
pub struct YabocDatabase {
    storage: salsa::Storage<YabocDatabase>,
}

impl salsa::Database for YabocDatabase {}
