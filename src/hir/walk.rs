use super::*;
pub struct ChildIter<'a, DB: Hirs + ?Sized> {
    child_list: Vec<HirId>,
    db: &'a DB,
    kinds: BitFlags<HirNodeKind>,
}

impl<'a, DB: Hirs + ?Sized> ChildIter<'a, DB> {
    pub fn new(id: HirId, db: &'a DB) -> Self {
        Self {
            child_list: vec![id],
            db,
            kinds: BitFlags::all(),
        }
    }
    pub fn with_kinds(mut self, kinds: impl Into<BitFlags<HirNodeKind>>) -> Self {
        self.kinds = kinds.into();
        self
    }
    pub fn without_kinds(mut self, kinds: impl Into<BitFlags<HirNodeKind>>) -> Self {
        self.kinds &= !kinds.into();
        self
    }
}

impl<'a, DB: Hirs + ?Sized> Iterator for ChildIter<'a, DB> {
    type Item = HirNode;

    fn next(&mut self) -> Option<Self::Item> {
        let next_node = loop {
            match self.db.hir_node(self.child_list.pop()?) {
                Ok(node) => break node,
                _ => continue,
            }
        };
        if next_node.is_kind(self.kinds) {
            self.child_list.append(&mut next_node.children());
        }
        Some(next_node)
    }
}
