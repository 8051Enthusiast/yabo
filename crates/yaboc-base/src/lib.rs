pub mod config;
pub mod databased_display;
pub mod error;
pub mod hash;
pub mod interner;
pub mod low_effort_interner;
mod represent;
pub mod source;

#[derive(Default)]
pub struct Context<DB: Default> {
    pub fc: source::FileCollection,
    pub db: DB,
    pub collection_reports: Vec<error::Report>,
}
