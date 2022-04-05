pub(crate) trait DatabasedDisplay<DB: ?Sized> {
    fn to_db_string(&self, db: &DB) -> String;
}