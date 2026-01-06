use std::{path::PathBuf, sync::Arc};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Config {
    pub target_triple: String,
    pub target_cpu: Option<String>,
    pub target_features: Option<String>,
    pub sysroot: Option<PathBuf>,
    pub cc: Option<String>,
    pub dynamic_linker: Option<String>,
    pub output_json: bool,
    pub asan: bool,
    pub msan: bool,
}

#[salsa::query_group(ConfigDatabase)]
pub trait Configs: salsa::Database {
    #[salsa::input]
    fn config(&self) -> Arc<Config>;
}
