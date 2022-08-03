use std::sync::Arc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Config {
    pub target_triple: String,
    pub target_cpu: String,
    pub target_features: String,
}

#[salsa::query_group(ConfigDatabase)]
pub trait Configs: salsa::Database {
    #[salsa::input]
    fn config(&self) -> Arc<Config>;
}