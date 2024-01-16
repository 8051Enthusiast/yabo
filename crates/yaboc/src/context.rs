use std::{ffi::OsStr, io::Write, process::Command, sync::Arc};

use bumpalo::Bump;
use inkwell::support::LLVMString;
use tempfile::NamedTempFile;
use yaboc_base::{
    config::{Config, Configs},
    error::Report,
    low_effort_interner,
    source::{AriadneCache, FileId},
    Context,
};
use yaboc_cg_llvm::CodeGenCtx;
use yaboc_database::YabocDatabase;
use yaboc_hir::represent::HirGraph;
use yaboc_layout::{InternedLayout, LayoutContext};
use yaboc_mir::{print_all_mir, print_all_mir_graphs};
const ERROR_FNS: &[fn(&YabocDatabase) -> Vec<Report>] = &[
    yaboc_ast::error::errors,
    yaboc_hir::error::errors,
    yaboc_resolve::error::errors,
    yaboc_dependents::error::errors,
    yaboc_hir_types::error::errors,
    yaboc_constraint::error::errors,
];

#[derive(Default)]
pub struct Driver(Context<YabocDatabase>);

impl std::ops::Deref for Driver {
    type Target = Context<YabocDatabase>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for Driver {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Driver {
    pub fn set_config(&mut self, config: Config) {
        let config = Arc::new(config);
        self.db.set_config(config);
    }
    fn diagnostics(&self) -> Vec<Report> {
        let mut ret = self.collection_reports.clone();
        ERROR_FNS.iter().for_each(|f| {
            ret.extend(f(&self.db));
        });
        ret
    }
    pub fn print_diagnostics(&self) -> bool {
        let diagnostics = self.diagnostics();
        if diagnostics.is_empty() {
            return false;
        }
        let output_json = self.db.config().output_json;
        for e in diagnostics {
            if output_json {
                let json = serde_json::to_string(&e.into_json(&self.db)).unwrap();
                eprintln!("{json}");
            } else {
                e.into_ariadne()
                    .eprint(AriadneCache::new(&self.db))
                    .expect("Error while printing errors")
            }
        }
        true
    }

    pub fn write_mir(&self, outfile: &OsStr) -> Result<(), std::io::Error> {
        let mut out = std::fs::File::create(outfile)?;
        print_all_mir(&self.db, &mut out, true)?;
        Ok(())
    }

    pub fn write_mir_graphs(
        &self,
        file: FileId,
        path: Option<&str>,
        outfile: &OsStr,
    ) -> Result<(), std::io::Error> {
        let mut out = std::fs::File::create(outfile)?;
        print_all_mir_graphs(&self.db, &mut out, path, file)?;
        Ok(())
    }

    pub fn write_hir(&self, outfile: &OsStr) -> Result<(), std::io::Error> {
        let mut out = std::fs::File::create(outfile)?;
        let hir_graph = HirGraph(&self.db);
        dot::render(&hir_graph, &mut out)?;
        Ok(())
    }

    pub fn write_deps(&self, outfile: &OsStr) -> Result<(), std::io::Error> {
        let Ok(s) = yaboc_dependents::dependency_dot(&self.db) else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "Error while generating dependency graph",
            ));
        };
        let mut out = std::fs::File::create(outfile)?;
        out.write_all(s.as_bytes())
    }

    pub fn write_lens(&self, outfile: &OsStr) -> Result<(), std::io::Error> {
        let Ok(s) = yaboc_constraint::represent::len_dot(&self.db) else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "Error while generating length graph",
            ));
        };
        let mut out = std::fs::File::create(outfile)?;
        out.write_all(s.as_bytes())
    }

    fn codegen_and_then(
        &self,
        f: impl FnOnce(CodeGenCtx) -> Result<(), LLVMString>,
    ) -> Result<(), String> {
        let llvm = inkwell::context::Context::create();
        let bump = Bump::new();
        let intern = low_effort_interner::Interner::<InternedLayout>::new(&bump);
        let layout_ctx = LayoutContext::new(intern);
        let mut layouts = yaboc_layout::AbsLayoutCtx::new(&self.db, layout_ctx);
        let mut codegen = match yaboc_cg_llvm::CodeGenCtx::new(&llvm, self, &mut layouts) {
            Ok(x) => x,
            Err(e) => panic!("Error while creating codegen context: {e:#?}"),
        };
        codegen.create_all_vtables();
        codegen.create_all_funs();
        codegen.create_pd_exports();
        codegen.create_max_buf_size();
        f(codegen).map_err(|x| x.to_string())
    }

    pub fn write_llvm(&self, outfile: &OsStr) -> Result<(), String> {
        self.codegen_and_then(|codegen| codegen.llvm_code(outfile))
    }

    pub fn write_object(&self, outfile: &OsStr) -> Result<(), String> {
        self.codegen_and_then(|codegen| codegen.object_file(outfile))
    }

    pub fn write_shared_lib(&self, outfile: &OsStr) -> Result<(), String> {
        let temp_object_file = NamedTempFile::new().map_err(|e| e.to_string())?;
        let temp_path = temp_object_file.into_temp_path();
        let temp_path = temp_path.as_os_str();
        self.codegen_and_then(|codegen| codegen.object_file(temp_path))?;
        Command::new("clang")
            .arg("-shared")
            .arg("-o")
            .arg(outfile)
            .arg(temp_path)
            .status()
            .map_err(|e| e.to_string())?;
        Ok(())
    }
}
