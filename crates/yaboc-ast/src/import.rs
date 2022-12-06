use std::{path::PathBuf, sync::Arc};

use fxhash::FxHashSet;
use yaboc_base::{
    error::SilencedError,
    interner::{Identifier, IdentifierName},
    source::{FileId, FileResolver},
    Context,
};

use crate::Asts;

// extension trait for a database that can import files
pub trait Import {
    fn update_db(&mut self, roots: &[FileId], absolute_mod_paths: &[(&str, &str)]);
    fn collect_files(&mut self, roots: &[FileId], absolute_mod_paths: &[(&str, &str)]);
    fn id(&self, string: &str) -> Identifier;
    fn mock(s: &str) -> Self;
}

impl<DB: Asts + Default> Import for Context<DB> {
    fn update_db(&mut self, roots: &[FileId], absolute_mod_paths: &[(&str, &str)]) {
        for root in roots {
            self.db
                .set_input_file(*root, Arc::new(self.fc.file_data(*root).clone()));
        }
        self.collect_files(roots, absolute_mod_paths)
    }

    fn collect_files(&mut self, roots: &[FileId], absolute_mod_paths: &[(&str, &str)]) {
        let mut errors = Vec::new();
        let mut existing_files = FxHashSet::default();
        let mut new_files = Vec::from(roots);
        let mut resolver = FileResolver::new(&mut self.fc);
        for (modname, path) in absolute_mod_paths {
            if let Err(e) =
                resolver.add_absolute_mod_path(&mut self.db, PathBuf::from(path), &modname)
            {
                errors.extend(e.into_report(&self.db))
            }
        }
        if let Err(e) = resolver.add_std(&mut self.db) {
            errors.extend(e.into_report(&self.db))
        }
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
                            errors.extend(e.into_report(&self.db));
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

    fn id(&self, string: &str) -> Identifier {
        self.db.intern_identifier(IdentifierName {
            name: string.to_string(),
        })
    }
    fn mock(s: &str) -> Self {
        let mut ctx = Self::default();
        let anon = ctx.fc.add_anon(s);
        ctx.update_db(&[anon], &[]);
        ctx
    }
}
