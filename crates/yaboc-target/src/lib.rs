use std::{
    borrow::Cow,
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
    pub features: Cow<'static, str>,
    pub cpu: Cow<'static, str>,
    pub use_tailcc: bool,
    pub use_musttail: bool,
}

pub fn target(config: &Config, rt_path: &Path) -> Option<Target> {
    let mut conf = match config.target_triple.as_str() {
        "x86_64-pc-linux-gnu" | "x86_64-unknown-linux-gnu" => Target {
            data: layout::POINTER64,
            linker: Arc::new(link::UnixClangLinker::new(
                config.target_triple.clone(),
                config.cc.as_deref().unwrap_or("clang").to_string(),
                config.sysroot.as_ref().map(PathBuf::from),
            )),
            features: Cow::Borrowed(""),
            cpu: Cow::Borrowed("x86-64-v2"),
            use_tailcc: true,
            use_musttail: true,
        },
        "wasm32-unknown-unknown" => Target {
            data: layout::POINTER32,
            linker: Arc::new(link::WasmLinker::new(
                config.cc.as_deref().unwrap_or("clang").to_string(),
                rt_path.to_path_buf(),
            )),
            features: Cow::Borrowed(""),
            cpu: Cow::Borrowed("generic"),
            use_tailcc: false,
            use_musttail: false,
        },
        "wasm32-unknown-emscripten" | "wasm32-emscripten" => Target {
            data: layout::POINTER32,
            linker: Arc::new(link::EmscriptenLinker::new(
                config.cc.as_deref().unwrap_or("emcc").to_string(),
            )),
            features: Cow::Borrowed(""),
            cpu: Cow::Borrowed("generic"),
            use_tailcc: false,
            use_musttail: false,
        },
        "wasm32-unknown-wasi" | "wasm32-wasi" => Target {
            data: layout::POINTER32,
            linker: Arc::new(link::UnixClangLinker::new(
                config.target_triple.clone(),
                config.cc.as_deref().unwrap_or("clang").to_string(),
                config.sysroot.as_ref().map(PathBuf::from),
            )),
            features: Cow::Borrowed(""),
            cpu: Cow::Borrowed("generic"),
            use_tailcc: false,
            use_musttail: false,
        },
        _ => return None,
    };
    if config.target_triple.starts_with("wasm32") {
        if let Some(features) = config.target_features.as_ref() {
            if features
                .split(',')
                .any(|f| matches!(f, "+tail-call" | "tail-call"))
            {
                conf.use_musttail = true;
            }
        }
    }
    if let Some(features) = config.target_features.as_ref() {
        conf.features = Cow::Owned(features.to_string());
    }
    if let Some(cpu) = config.target_cpu.as_ref() {
        conf.cpu = Cow::Owned(cpu.to_string());
    }
    Some(conf)
}
