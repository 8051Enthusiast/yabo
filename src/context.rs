use crate::ast::AstDatabase;
use crate::hir::HirDatabase;
use crate::hir_types::HirTypesDatabase;
use crate::interner::{Identifier, IdentifierName, Interner, InternerDatabase};
use crate::source::{FileCollection, FileDatabase};
use crate::types::TypeInternerDatabase;

#[salsa::database(
    InternerDatabase,
    AstDatabase,
    FileDatabase,
    HirDatabase,
    TypeInternerDatabase,
    HirTypesDatabase
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
    #[cfg(test)]
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
}
