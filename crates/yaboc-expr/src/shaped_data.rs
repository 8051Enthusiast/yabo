use std::{marker::PhantomData, ops::Index};

use crate::ExprIdx;

#[repr(transparent)]
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct ShapedData<D, K> {
    pub(crate) data: D,
    pub(crate) _marker: PhantomData<K>,
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
    pub fn map<D2>(self, f: impl Fn(D::Item) -> D2) -> ShapedData<impl Iterator<Item = D2>, K> {
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
            data: self.data.into_iter().zip(other.data.into_iter()),
            _marker: PhantomData,
        }
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
