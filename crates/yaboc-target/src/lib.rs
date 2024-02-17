use std::{path::PathBuf, sync::Arc};

use layout::TargetLayoutData;
pub use paste;

pub mod layout;
pub mod link;

#[derive(Clone)]
pub struct Target {
    pub data: TargetLayoutData,
    pub linker: Arc<dyn link::Linker>,
    pub default_features: &'static str,
    pub default_cpu: &'static str,
    pub use_tailcc: bool,
}

pub fn target(triple: &str, rt_path: PathBuf) -> Option<Target> {
    match triple {
        "x86_64-pc-linux-gnu" | "x86_64-unknown-linux-gnu" => Some(Target {
            data: layout::POINTER64,
            linker: Arc::new(link::UnixClangLinker::new(triple.to_string())),
            default_features: "",
            default_cpu: "x86-64-v2",
            use_tailcc: true,
        }),
        "wasm32-unknown-unknown" => Some(Target {
            data: layout::POINTER32,
            linker: Arc::new(link::WasmLinker::new(rt_path)),
            default_features: "",
            default_cpu: "generic",
            use_tailcc: false,
        }),
        _ => None,
    }
}
