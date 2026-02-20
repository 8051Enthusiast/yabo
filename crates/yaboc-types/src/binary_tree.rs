use std::convert::Infallible;

#[derive(Debug)]
enum BinaryTreeNode<T> {
    Leaf(T),
    Branch(Box<BinaryTreeNode<T>>, Box<BinaryTreeNode<T>>),
}

impl<T> BinaryTreeNode<T> {
    fn try_iterate_impl<E>(self, f: &mut impl FnMut(T) -> Result<(), E>) -> Result<(), E> {
        match self {
            BinaryTreeNode::Leaf(t) => f(t)?,
            BinaryTreeNode::Branch(lhs, rhs) => {
                lhs.try_iterate_impl(f)?;
                rhs.try_iterate_impl(f)?;
            }
        };
        Ok(())
    }
}

#[derive(Debug)]
pub struct BinaryTree<T>(Option<BinaryTreeNode<T>>);

impl<T> BinaryTree<T> {
    pub fn new_empty() -> Self {
        BinaryTree(None)
    }

    pub fn empty(&self) -> bool {
        matches!(self, BinaryTree(None))
    }

    pub fn new_single(t: T) -> Self {
        BinaryTree(Some(BinaryTreeNode::Leaf(t)))
    }

    pub fn merge(self, other: Self) -> Self {
        match (self, other) {
            (BinaryTree(None), other) | (other, BinaryTree(None)) => other,
            (BinaryTree(Some(lhs)), BinaryTree(Some(rhs))) => {
                BinaryTree(Some(BinaryTreeNode::Branch(Box::new(lhs), Box::new(rhs))))
            }
        }
    }

    pub fn try_iterate<E>(self, mut f: impl FnMut(T) -> Result<(), E>) -> Result<(), E> {
        if let BinaryTree(Some(tree)) = self {
            tree.try_iterate_impl(&mut f)?;
        }
        Ok(())
    }

    pub fn iterate(self, mut f: impl FnMut(T)) {
        self.try_iterate::<Infallible>(|element| Ok(f(element)));
    }

    pub fn take(&mut self) -> Self {
        BinaryTree(self.0.take())
    }
}
