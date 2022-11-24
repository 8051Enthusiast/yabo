use std::ffi::OsStr;
use std::process::Command;
use std::sync::Arc;

use bumpalo::Bump;
use fxhash::FxHashSet;
use inkwell::support::LLVMString;
use tempfile::NamedTempFile;

use crate::absint::AbsIntDatabase;
use crate::ast::{AstDatabase, Asts};
use crate::cg_llvm::{self, CodeGenCtx};
use crate::config::{Config, ConfigDatabase, Configs};
use crate::error::diagnostic::{DiagnosticKind, Label};
use crate::error::{Report, SilencedError};
use crate::hir::represent::HirGraph;
use crate::hir::{HirDatabase, Hirs};
use crate::hir_types::{HirTypesDatabase, TyHirs};
use crate::interner::{Identifier, IdentifierName, Interner, InternerDatabase};
use crate::layout::{self, instantiate, InternerLayout, LayoutContext, LayoutDatabase};
use crate::mir::{print_all_mir, MirDatabase};
use crate::order::OrdersDatabase;
use crate::resolve::ResolveDatabase;
use crate::source::{
    AriadneCache, FileCollection, FileDatabase, FileId, FileLoadError, FileResolver, Files,
};
use crate::types::TypeInternerDatabase;
use crate::{dbformat, low_effort_interner};

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
    collection_reports: Vec<Report>,
}

const ERROR_FNS: &[fn(&LivingInTheDatabase) -> Vec<Report>] = &[
    crate::ast::error::errors,
    crate::hir::error::errors,
    crate::resolve::error::errors,
    crate::order::error::errors,
    crate::hir_types::error::errors,
];

impl Context {
    pub fn update_db(&mut self, roots: &[FileId]) {
        for root in roots {
            self.db
                .set_input_file(*root, Arc::new(self.fc.file_data(*root).clone()));
        }
        self.collect_files(roots)
    }

    fn collect_files(&mut self, roots: &[FileId]) {
        let mut errors = Vec::new();
        let mut existing_files = FxHashSet::default();
        let mut new_files = Vec::from(roots);
        let mut resolver = FileResolver::new(&mut self.fc);
        while let Some(f) = new_files.pop() {
            if !existing_files.insert(f) {
                continue;
            }
            let Ok(imports) = self.db.imports(f) else {
                continue
            };
            for import in imports.iter() {
                let imported_file =
                    match resolver.resolve(&mut self.db, f, import.inner, import.span) {
                        Ok(imported_file) => imported_file,
                        Err(e) => {
                            match e {
                                FileLoadError::LoadError {
                                    source,
                                    name,
                                    error,
                                } => errors.push(
                                    Report::new(
                                        DiagnosticKind::Error,
                                        source.0,
                                        &dbformat!(
                                            &self.db,
                                            "Could not load {}: {}",
                                            &name,
                                            &error
                                        ),
                                    )
                                    .with_code(150)
                                    .with_label(Label::new(source.1).with_message("imported here")),
                                ),
                                FileLoadError::DoesNotExist { source, name } => errors.push(
                                    Report::new(
                                        DiagnosticKind::Error,
                                        source.0,
                                        &dbformat!(&self.db, "Could not find {}", &name),
                                    )
                                    .with_code(151)
                                    .with_label(Label::new(source.1).with_message("imported here")),
                                ),
                                FileLoadError::Silenced => (),
                            };
                            self.db
                                .set_import_id(f, import.inner, Err(SilencedError::new()));
                            continue;
                        }
                    };
                self.db.set_import_id(f, import.inner, Ok(imported_file));
                if !existing_files.contains(&imported_file) {
                    new_files.push(imported_file);
                }
            }
        }
        let mut existing_vec = existing_files.into_iter().collect::<Vec<_>>();
        existing_vec.sort_unstable();
        self.db.set_all_files(Arc::new(existing_vec));
        self.collection_reports = errors;
    }

    pub fn id(&self, string: &str) -> Identifier {
        self.db.intern_identifier(IdentifierName {
            name: string.to_string(),
        })
    }
    #[cfg(test)]
    pub fn mock(s: &str) -> Self {
        let mut ctx = Self::default();
        let anon = ctx.fc.add_anon(s);
        ctx.update_db(&[anon]);
        ctx
    }
    pub fn parser(&self, s: &str) -> crate::hir::ParserDefId {
        use crate::{
            hir::ParserDefId,
            interner::{FieldName, HirPath},
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
                eprintln!("{}", json);
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
