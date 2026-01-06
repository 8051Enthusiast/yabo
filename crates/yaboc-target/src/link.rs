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
    rt_path: PathBuf,
    sysroot: Option<PathBuf>,
}

impl UnixClangLinker {
    pub fn new(triple: String, cc: String, sysroot: Option<PathBuf>, rt_path: PathBuf) -> Self {
        Self {
            cc,
            triple,
            rt_path,
            sysroot,
        }
    }
}

impl Linker for UnixClangLinker {
    fn link_shared(&self, path: &Path, output_path: &Path) -> std::io::Result<()> {
        let mut cmd = Command::new(&self.cc);
        cmd.arg("-shared")
            .arg("-fuse-ld=lld")
            .arg("-fPIC")
            .arg("-target")
            .arg(&self.triple)
            .arg("-ffunction-sections")
            .arg("-ffreestanding")
            .arg("-nostdlib")
            .arg("-Wl,--gc-sections")
            .arg("-O3");
        if let Some(sysroot) = &self.sysroot {
            cmd.arg("--sysroot").arg(sysroot);
        }
        let output = cmd
            .arg(path)
            .arg(&self.rt_path)
            .arg("-lgcc")
            .arg("-o")
            .arg(output_path)
            .output()?;
        if !output.status.success() {
            return Err(std::io::Error::other(format!(
                "Linker failed with status {}:\n{}",
                output.status,
                String::from_utf8_lossy(&output.stderr)
            )));
        }
        Ok(())
    }
}

pub struct WasmLinker {
    cc: String,
    rt_path: PathBuf,
}

impl WasmLinker {
    pub fn new(cc: String, rt_path: PathBuf) -> Self {
        Self { cc, rt_path }
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
            return Err(std::io::Error::other(format!(
                "Runtime build failed with status {}:\n{}",
                output.status,
                String::from_utf8_lossy(&output.stderr)
            )));
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
            .arg("-Wl,--export-table")
            .arg(format!("-Wl,-z,stack-size={}", Self::STACK_SIZE))
            .arg("-Wl,--gc-sections")
            .arg("-o")
            .arg(output_path)
            .arg(tmp_rt_obj.path())
            .arg(path)
            .output()?;
        if !output.status.success() {
            return Err(std::io::Error::other(format!(
                "Linker failed with status {}:\n{}",
                output.status,
                String::from_utf8_lossy(&output.stderr)
            )));
        }
        Ok(())
    }
}

pub struct EmscriptenLinker {
    cc: String,
}

impl EmscriptenLinker {
    pub fn new(cc: String) -> Self {
        Self { cc }
    }
}

impl Linker for EmscriptenLinker {
    fn link_shared(&self, path: &Path, output_path: &Path) -> std::io::Result<()> {
        let output = Command::new(&self.cc)
            .arg("-shared")
            .arg("-sSIDE_MODULE=1")
            .arg("-o")
            .arg(output_path)
            .arg(path)
            .output()?;
        if !output.status.success() {
            return Err(std::io::Error::other(format!(
                "Linker failed with status {}:\n{}",
                output.status,
                String::from_utf8_lossy(&output.stderr)
            )));
        }
        Ok(())
    }
}
