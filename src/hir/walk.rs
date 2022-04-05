use super::*;
pub struct ChildIter<'a, DB: Hirs + ?Sized> {
    child_list: Vec<HirId>,
    db: &'a DB,
}

impl<'a, DB: Hirs + ?Sized> ChildIter<'a, DB> {
    pub fn new(id: HirId, db: &'a DB) -> Self {
        Self {
            child_list: vec![id],
            db,
        }
    }
}

impl<'a, DB: Hirs + ?Sized> Iterator for ChildIter<'a, DB> {
    type Item = HirNode;

    fn next(&mut self) -> Option<Self::Item> {
        let next_node = loop {
            match self.db.hir_node(self.child_list.pop()?) {
                Ok(node) => break node,
                Err(_) => continue,
            }
        };
        self.child_list.append(&mut next_node.children());
        Some(next_node)
    }
}
