use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use layout::TargetLayoutData;
pub use paste;
use yaboc_base::config::Config;

pub mod layout;
pub mod link;

#[derive(Clone)]
pub struct Target {
    pub data: TargetLayoutData,
    pub linker: Arc<dyn link::Linker>,
    pub default_features: &'static str,
    pub default_cpu: &'static str,
    pub use_tailcc: bool,
    pub use_musttail: bool,
}

pub fn target(config: &Config, rt_path: &Path) -> Option<Target> {
    match config.target_triple.as_str() {
        "x86_64-pc-linux-gnu" | "x86_64-unknown-linux-gnu" => Some(Target {
            data: layout::POINTER64,
            linker: Arc::new(link::UnixClangLinker::new(
                config.target_triple.clone(),
                config.cc.as_deref().unwrap_or("clang").to_string(),
                config.sysroot.as_ref().map(PathBuf::from),
            )),
            default_features: "",
            default_cpu: "x86-64-v2",
            use_tailcc: true,
            use_musttail: true,
        }),
        "wasm32-unknown-unknown" => Some(Target {
            data: layout::POINTER32,
            linker: Arc::new(link::WasmLinker::new(
                config.cc.as_deref().unwrap_or("clang").to_string(),
                rt_path.to_path_buf(),
            )),
            default_features: "",
            default_cpu: "generic",
            use_tailcc: false,
            use_musttail: false,
        }),
        "wasm32-unknown-emscripten" | "wasm32-emscripten" => Some(Target {
            data: layout::POINTER32,
            linker: Arc::new(link::EmscriptenLinker::new(
                config.cc.as_deref().unwrap_or("emcc").to_string(),
            )),
            default_features: "",
            default_cpu: "generic",
            use_tailcc: false,
            use_musttail: false,
        }),
        "wasm32-unknown-wasi" | "wasm32-wasi" => Some(Target {
            data: layout::POINTER32,
            linker: Arc::new(link::UnixClangLinker::new(
                config.target_triple.clone(),
                config.cc.as_deref().unwrap_or("clang").to_string(),
                config.sysroot.as_ref().map(PathBuf::from),
            )),
            default_features: "",
            default_cpu: "generic",
            use_tailcc: false,
            use_musttail: false,
        }),
        _ => None,
    }
}
