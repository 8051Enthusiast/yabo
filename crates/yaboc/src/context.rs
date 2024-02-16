use std::{ffi::OsStr, io::Write, sync::Arc};

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
use yaboc_target::Target;
const ERROR_FNS: &[fn(&YabocDatabase) -> Vec<Report>] = &[
    yaboc_ast::error::errors,
    yaboc_hir::error::errors,
    yaboc_resolve::error::errors,
    yaboc_dependents::error::errors,
    yaboc_hir_types::error::errors,
    yaboc_constraint::error::errors,
];

pub struct Driver {
    ctx: Context<YabocDatabase>,
    target: Target,
}

impl std::ops::Deref for Driver {
    type Target = Context<YabocDatabase>;

    fn deref(&self) -> &Self::Target {
        &self.ctx
    }
}

impl std::ops::DerefMut for Driver {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ctx
    }
}

impl Driver {
    pub fn new(config: Config) -> Option<Self> {
        let target = yaboc_target::target(&config.target_triple)?;
        let mut ctx = Context::<YabocDatabase>::default();
        let config = Arc::new(config);
        ctx.db.set_config(config);
        Some(Self { ctx, target })
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
        let layout_ctx = LayoutContext::new(intern, self.target.data);
        let mut layouts = yaboc_layout::AbsLayoutCtx::new(&self.db, layout_ctx);
        let mut codegen = match yaboc_cg_llvm::CodeGenCtx::new(&llvm, self, &mut layouts) {
            Ok(x) => x,
            Err(e) => panic!("Error while creating codegen context: {e:#?}"),
        };
        codegen.run_codegen();
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
        let temp_path = temp_path.to_path_buf();
        self.codegen_and_then(|codegen| codegen.object_file(temp_path.as_os_str()))?;
        self.target
            .linker
            .link_shared(&temp_path, outfile.as_ref())
            .map_err(|e| e.to_string())?;
        Ok(())
    }
}
