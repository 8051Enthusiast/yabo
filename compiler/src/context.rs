use std::ffi::OsStr;
use std::process::Command;
use std::sync::Arc;

use bumpalo::Bump;
use inkwell::support::LLVMString;
use tempfile::NamedTempFile;

use crate::absint::AbsIntDatabase;
use crate::ast::AstDatabase;
use crate::cg_llvm::{self, CodeGenCtx};
use crate::config::{Config, ConfigDatabase, Configs};
use crate::error::Report;
use crate::hir::represent::HirGraph;
use crate::hir::{HirDatabase, Hirs};
use crate::hir_types::{HirTypesDatabase, TyHirs};
use crate::interner::{Identifier, IdentifierName, Interner, InternerDatabase};
use crate::layout::{self, instantiate, InternerLayout, LayoutContext, LayoutDatabase};
use crate::low_effort_interner;
use crate::mir::{print_all_mir, MirDatabase};
use crate::order::OrdersDatabase;
use crate::resolve::ResolveDatabase;
use crate::source::{AriadneCache, FileCollection, FileDatabase};
use crate::types::TypeInternerDatabase;

#[salsa::database(
    InternerDatabase,
    ConfigDatabase,
    AstDatabase,
    FileDatabase,
    HirDatabase,
    ResolveDatabase,
    TypeInternerDatabase,
    HirTypesDatabase,
    OrdersDatabase,
    MirDatabase,
    AbsIntDatabase,
    LayoutDatabase
)]
#[derive(Default)]
pub struct LivingInTheDatabase {
    storage: salsa::Storage<LivingInTheDatabase>,
}

impl salsa::Database for LivingInTheDatabase {}
#[derive(Default)]
pub struct Context {
    pub fc: FileCollection,
    pub db: LivingInTheDatabase,
}

const ERROR_FNS: &[fn(&LivingInTheDatabase) -> Vec<Report>] = &[
    crate::ast::error::errors,
    crate::hir::error::errors,
    crate::resolve::error::errors,
    crate::order::error::errors,
    crate::hir_types::error::errors,
];

impl Context {
    pub fn update_db(&mut self) {
        self.fc.insert_into_db(&mut self.db);
    }
    pub fn id(&self, string: &str) -> Identifier {
        self.db.intern_identifier(IdentifierName {
            name: string.to_string(),
        })
    }
    #[cfg(test)]
    pub fn mock(s: &str) -> Self {
        let mut ctx = Self::default();
        ctx.fc.add_anon(s);
        ctx.update_db();
        ctx
    }
    pub fn parser(&self, s: &str) -> crate::hir::ParserDefId {
        use crate::{
            hir::ParserDefId,
            interner::{FieldName, HirPath},
            source::FileId,
        };

        let fd = FileId::default();
        ParserDefId(
            self.db
                .intern_hir_path(HirPath::new_fid(fd, FieldName::Ident(self.id(s)))),
        )
    }
    pub fn set_config(&mut self, config: Config) {
        let config = Arc::new(config);
        self.db.set_config(config);
    }
    pub fn diagnostics(&self) -> Vec<Report> {
        let mut ret = Vec::new();
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
        for e in diagnostics {
            e.eprint(AriadneCache::new(&self.db))
                .expect("Error while printing errors")
        }
        true
    }

    pub fn write_mir(&self, outfile: &OsStr) -> Result<(), std::io::Error> {
        let mut out = std::fs::File::create(outfile)?;
        print_all_mir(&self.db, &mut out)?;
        Ok(())
    }

    pub fn write_hir(&self, outfile: &OsStr) -> Result<(), std::io::Error> {
        let mut out = std::fs::File::create(outfile)?;
        let hir_graph = HirGraph(&self.db);
        dot::render(&hir_graph, &mut out)?;
        Ok(())
    }

    fn codegen_and_then(
        &self,
        f: impl FnOnce(CodeGenCtx) -> Result<(), LLVMString>,
    ) -> Result<(), String> {
        let llvm = inkwell::context::Context::create();
        let mut bump = Bump::new();
        let intern = low_effort_interner::Interner::<InternerLayout>::new(&mut bump);
        let layout_ctx = LayoutContext::new(intern);
        let exported_pds = self.db.all_exported_parserdefs();
        let exported_tys: Vec<_> = exported_pds
            .iter()
            .map(|x| self.db.parser_args(*x).unwrap().thunk)
            .collect();
        let mut layouts = layout::AbsLayoutCtx::new(&self.db, layout_ctx);
        instantiate(&mut layouts, &exported_tys).unwrap();
        let mut codegen = cg_llvm::CodeGenCtx::new(&llvm, self, &mut layouts).unwrap();
        codegen.create_all_vtables();
        codegen.create_all_funs();
        codegen.create_pd_exports();
        codegen.create_free_fun();
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
