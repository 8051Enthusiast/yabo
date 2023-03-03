use std::{convert::Infallible, marker::PhantomData};

use crate::{shaped_data::IndexExpr, ExprIdx, ExprKind, TakeRef, ZipExpr};

pub trait FetchData<K: ExprKind, Id: Copy, DB: ?Sized> {
    type Data;
    type Err;
    fn fetch_data(db: &DB, id: Id) -> Result<Self::Data, Self::Err>;
}

pub trait FetchKindData<D, Id: Copy, DB: ?Sized>: ExprKind {
    type Err;
    type Data;
    fn fetch_kind_data(db: &DB, id: Id) -> Result<Self::Data, Self::Err>;
}

impl<DB: ?Sized, K: ExprKind, Id: Copy, D> FetchData<K, Id, DB> for D
where
    K: FetchKindData<D, Id, DB>,
{
    type Data = <K as FetchKindData<D, Id, DB>>::Data;
    type Err = K::Err;
    #[inline(always)]
    fn fetch_data(db: &DB, id: Id) -> Result<Self::Data, Self::Err> {
        <K as FetchKindData<D, Id, DB>>::fetch_kind_data(db, id)
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct Enumerated<K: ExprKind>(usize, PhantomData<K>);

impl<K: ExprKind> Iterator for Enumerated<K> {
    type Item = ExprIdx<K>;
    fn next(&mut self) -> Option<Self::Item> {
        let i = self.0;
        self.0 += 1;
        Some(ExprIdx::new_from_usize(i))
    }
}

impl<K: ExprKind> TakeRef for Enumerated<K> {
    type Ref<'a> = Enumerated<K>
    where
        Self: 'a;
    fn take_ref(&self) -> Self::Ref<'_> {
        Enumerated(self.0, PhantomData)
    }
}

impl<K: ExprKind> IndexExpr<K> for Enumerated<K> {
    type Output<'a> = ExprIdx<K>
    where
        Self: 'a;

    fn index_expr(&self, idx: ExprIdx<K>) -> Self::Output<'_> {
        idx
    }
}

impl<DB: ?Sized, K: ExprKind, Id: Copy> FetchKindData<ExprIdx<K>, Id, DB> for K {
    type Data = Enumerated<K>;
    type Err = Infallible;
    fn fetch_kind_data(_db: &DB, _id: Id) -> Result<Self::Data, Infallible> {
        Ok(Enumerated(0, PhantomData))
    }
}

pub trait FetchExpr<Id: Copy, DB: ?Sized>: ExprKind {
    type Expr;
    type Err;
    fn fetch_expr(db: &DB, id: Id) -> Result<Self::Expr, Self::Err>;

    fn expr_with_data<D: FetchData<Self, Id, DB>>(
        db: &DB,
        id: Id,
    ) -> Result<ZipExpr<Self::Expr, D::Data>, Self::Err>
    where
        Self::Err: From<D::Err>,
    {
        Ok(ZipExpr {
            expr: Self::fetch_expr(db, id)?,
            data: D::fetch_data(db, id)?,
        })
    }
}
