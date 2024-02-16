use layout::TargetLayoutData;
pub use paste;

pub mod layout;
pub mod link;

pub struct Target {
    pub data: TargetLayoutData,
    pub linker: Box<dyn link::Linker>,
    pub default_features: &'static str,
    pub default_cpu: &'static str,
}

pub fn target(triple: &str) -> Option<Target> {
    match triple {
        "x86_64-pc-linux-gnu" => Some(Target {
            data: layout::POINTER64,
            linker: Box::new(link::UnixClangLinker::new(triple.to_string())),
            default_features: "",
            default_cpu: "x86-64-v2",
        }),
        _ => None,
    }
}
