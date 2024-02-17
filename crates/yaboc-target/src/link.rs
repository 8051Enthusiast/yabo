use std::{
    path::{Path, PathBuf},
    process::Command,
};
use tempfile::NamedTempFile;

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

pub struct WasmLinker {
    cc: String,
    rt_path: PathBuf,
}

impl WasmLinker {
    pub fn new(rt_path: PathBuf) -> Self {
        Self {
            cc: "clang".to_string(),
            rt_path,
        }
    }

    fn build_rt(&self) -> std::io::Result<NamedTempFile> {
        let tmp_rt_obj = NamedTempFile::new()?;
        let output = Command::new(&self.cc)
            .arg("--target=wasm32-unknown-unknown")
            .arg("-ffreestanding")
            .arg("-nostdlib")
            .arg("-O3")
            .arg("-c")
            .arg("-o")
            .arg(tmp_rt_obj.path())
            .arg(&self.rt_path)
            .output()?;
        if !output.status.success() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!(
                    "Runtime build failed with status {}:\n{}",
                    output.status,
                    String::from_utf8_lossy(&output.stderr)
                ),
            ));
        }
        Ok(tmp_rt_obj)
    }

    const STACK_SIZE: u32 = 1048576;
}

impl Linker for WasmLinker {
    fn link_shared(&self, path: &Path, output_path: &Path) -> std::io::Result<()> {
        let tmp_rt_obj = self.build_rt()?;
        let output = Command::new(&self.cc)
            .arg("--target=wasm32-unknown-unknown")
            .arg("-nostdlib")
            .arg("-Wl,--no-entry")
            .arg("-Wl,--export-dynamic")
            .arg(&format!("-Wl,-z,stack-size={}", Self::STACK_SIZE))
            .arg("-Wl,--gc-sections")
            .arg("-o")
            .arg(output_path)
            .arg(tmp_rt_obj.path())
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
