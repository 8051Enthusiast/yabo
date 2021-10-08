use crate::ast::AstDatabase;
use crate::interner::{Identifier, IdentifierName, InternerDatabase, Interner};
use crate::source::{FileCollection, FileDatabase};

#[salsa::database(InternerDatabase, AstDatabase, FileDatabase)]
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
                name: string.to_string()
        })
    }
    #[cfg(test)]
    pub fn mock(s: &str) -> Self {
        let mut ctx = Self::default();
        ctx.fc.add_anon(s);
        ctx.update_db();
        ctx
    }
}
