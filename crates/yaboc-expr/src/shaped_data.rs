use std::{
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use crate::{ExprIdx, TakeRef};

#[repr(transparent)]
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct ShapedData<D: ?Sized, K> {
    pub(crate) _marker: PhantomData<K>,
    pub(crate) data: D,
}

impl<K, D> ShapedData<D, K> {
    pub fn data(&self) -> &D {
        &self.data
    }
    pub fn into_data(self) -> D {
        self.data
    }
    pub fn from_raw_data(data: D) -> Self {
        ShapedData {
            data,
            _marker: PhantomData,
        }
    }
    pub fn ignore_shape<ToK>(self) -> ShapedData<D, ToK> {
        ShapedData {
            data: self.data,
            _marker: PhantomData,
        }
    }
}

impl<K, D> ShapedData<Vec<D>, K> {
    pub fn as_slice(&self) -> ShapedData<&[D], K> {
        ShapedData {
            data: &self.data,
            _marker: PhantomData,
        }
    }

    pub fn root(&self) -> ExprIdx<K> {
        let idx = self.data.len() - 1;
        ExprIdx::new_from_usize(idx)
    }

    pub fn root_data(&self) -> &D {
        &self.data[self.data.len() - 1]
    }
}

impl<K, D> TakeRef for ShapedData<Vec<D>, K> {
    type Ref<'a> = ShapedData<&'a [D], K>
    where
        Self: 'a;
    fn take_ref(&self) -> Self::Ref<'_> {
        ShapedData {
            data: self.data.as_slice(),
            _marker: PhantomData,
        }
    }
}

impl<K, D: IntoIterator> IntoIterator for ShapedData<D, K> {
    type Item = D::Item;
    type IntoIter = D::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.data.into_iter()
    }
}

impl<'a, K, D> IntoIterator for &'a ShapedData<D, K>
where
    &'a D: IntoIterator,
{
    type Item = <&'a D as IntoIterator>::Item;
    type IntoIter = <&'a D as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        (&self.data).into_iter()
    }
}

impl<K, D: IntoIterator> ShapedData<D, K> {
    pub fn map<D2>(self, f: impl FnMut(D::Item) -> D2) -> ShapedData<impl Iterator<Item = D2>, K> {
        ShapedData {
            data: self.data.into_iter().map(f),
            _marker: PhantomData,
        }
    }

    pub fn zip<D2>(
        self,
        other: ShapedData<D2, K>,
    ) -> ShapedData<impl Iterator<Item = (D::Item, D2::Item)>, K>
    where
        D2: IntoIterator,
    {
        ShapedData {
            data: self.data.into_iter().zip(other.data),
            _marker: PhantomData,
        }
    }

    pub fn collect<C>(self) -> ShapedData<C, K>
    where
        C: FromIterator<D::Item>,
    {
        ShapedData {
            data: self.data.into_iter().collect(),
            _marker: PhantomData,
        }
    }
}

impl<K, E, F, D: IntoIterator<Item = Result<F, E>>> ShapedData<D, K> {
    pub fn try_collect<C>(self) -> Result<ShapedData<C, K>, E>
    where
        C: FromIterator<F>,
    {
        Ok(ShapedData {
            data: self.data.into_iter().collect::<Result<C, E>>()?,
            _marker: PhantomData,
        })
    }
}

impl<'a, K, D: 'a, Data: IntoIterator<Item = &'a D>> ShapedData<Data, K> {
    pub fn cloned(self) -> ShapedData<impl Iterator<Item = D>, K>
    where
        D: Clone,
    {
        ShapedData {
            data: self.data.into_iter().cloned(),
            _marker: PhantomData,
        }
    }
}

impl<K, D: Index<usize>> ShapedData<D, K> {
    pub fn get(&self, idx: ExprIdx<K>) -> &D::Output {
        &self.data[idx.as_usize()]
    }
}

impl<K, D: Index<usize>> Index<ExprIdx<K>> for ShapedData<D, K> {
    type Output = D::Output;
    fn index(&self, idx: ExprIdx<K>) -> &Self::Output {
        &self.data[idx.as_usize()]
    }
}

impl<K, D: IndexMut<usize>> ShapedData<D, K> {
    pub fn get_mut(&mut self, idx: ExprIdx<K>) -> &mut D::Output {
        &mut self.data[idx.as_usize()]
    }
}

impl<K, D: IndexMut<usize>> IndexMut<ExprIdx<K>> for ShapedData<D, K> {
    fn index_mut(&mut self, idx: ExprIdx<K>) -> &mut Self::Output {
        &mut self.data[idx.as_usize()]
    }
}

pub trait IndexExpr<K> {
    type Output<'a>
    where
        Self: 'a;
    fn index_expr(&self, idx: ExprIdx<K>) -> Self::Output<'_>;
}

impl<K, D> IndexExpr<K> for ShapedData<Vec<D>, K> {
    type Output<'a> = &'a D where Self: 'a;
    fn index_expr(&self, idx: ExprIdx<K>) -> Self::Output<'_> {
        &self.data[idx.as_usize()]
    }
}

impl<K, D> IndexExpr<K> for ShapedData<&[D], K> {
    type Output<'b> = &'b D where Self: 'b;
    fn index_expr(&self, idx: ExprIdx<K>) -> Self::Output<'_> {
        &self.data[idx.as_usize()]
    }
}

impl<K, D: IndexExpr<K>> IndexExpr<K> for std::sync::Arc<D> {
    type Output<'a> = D::Output<'a> where Self: 'a;
    fn index_expr(&self, idx: ExprIdx<K>) -> Self::Output<'_> {
        self.as_ref().index_expr(idx)
    }
}

impl<K, D: IndexExpr<K>> IndexExpr<K> for std::rc::Rc<D> {
    type Output<'a> = D::Output<'a> where Self: 'a;
    fn index_expr(&self, idx: ExprIdx<K>) -> Self::Output<'_> {
        self.as_ref().index_expr(idx)
    }
}

impl<K, D, F: Fn(ExprIdx<K>) -> D> IndexExpr<K> for F {
    type Output<'a> = D where Self: 'a;
    fn index_expr(&self, idx: ExprIdx<K>) -> Self::Output<'_> {
        self(idx)
    }
}
