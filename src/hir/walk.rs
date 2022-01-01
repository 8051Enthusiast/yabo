use super::*;
pub struct ChildIter<'a> {
    child_list: Vec<HirId>,
    db: &'a dyn Hirs,
}

impl<'a> ChildIter<'a> {
    pub fn new(id: HirId, db: &'a dyn Hirs) -> Self {
        Self {
            child_list: vec![id],
            db,
        }
    }
}

impl<'a> Iterator for ChildIter<'a> {
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
