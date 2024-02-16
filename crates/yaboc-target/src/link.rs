use std::{path::Path, process::Command};

pub trait Linker {
    fn link_shared(&self, path: &Path, output_path: &Path) -> std::io::Result<()>;
}

pub struct UnixClangLinker {
    cc: String,
    triple: String,
}

impl UnixClangLinker {
    pub fn new(triple: String) -> Self {
        Self {
            cc: "clang".to_string(),
            triple,
        }
    }
}

impl Linker for UnixClangLinker {
    fn link_shared(&self, path: &Path, output_path: &Path) -> std::io::Result<()> {
        let output = Command::new(&self.cc)
            .arg("-shared")
            .arg("-fPIC")
            .arg("-target")
            .arg(&self.triple)
            .arg("-o")
            .arg(output_path)
            .arg(path)
            .output()?;
        if !output.status.success() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!(
                    "Linker failed with status {}:\n{}",
                    output.status,
                    String::from_utf8_lossy(&output.stderr)
                ),
            ));
        }
        Ok(())
    }
}
