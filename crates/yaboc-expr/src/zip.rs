use crate::{
    fetch::FetchKindData, shaped_data::IndexExpr, ExprHead, ExprIdx, ExprKind, Expression,
    IdxExprRef, IdxExpression, ShapedData, TakeRef,
};

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct ZipExpr<Expr, Data> {
    pub expr: Expr,
    pub data: Data,
}

impl<Expr, Data: IntoIterator> ZipExpr<Expr, Data> {
    pub fn zip<D2: IntoIterator>(
        self,
        data: D2,
    ) -> ZipExpr<Expr, impl Iterator<Item = (Data::Item, D2::Item)>> {
        ZipExpr {
            expr: self.expr,
            data: self.data.into_iter().zip(data),
        }
    }

    pub fn map<D2>(
        self,
        f: impl FnMut(Data::Item) -> D2,
    ) -> ZipExpr<Expr, impl Iterator<Item = D2>> {
        ZipExpr {
            expr: self.expr,
            data: self.data.into_iter().map(f),
        }
    }

    pub fn collect(self) -> ZipExpr<Expr, Vec<Data::Item>> {
        ZipExpr {
            expr: self.expr,
            data: self.data.into_iter().collect(),
        }
    }
}

impl<'a, Expr, D: 'a, Data: IntoIterator<Item = &'a D>> ZipExpr<Expr, Data> {
    pub fn cloned(self) -> ZipExpr<Expr, std::iter::Cloned<Data::IntoIter>>
    where
        D: Clone,
    {
        ZipExpr {
            expr: self.expr,
            data: self.data.into_iter().cloned(),
        }
    }
}

impl<Expr: TakeRef, Data: TakeRef> TakeRef for ZipExpr<Expr, Data> {
    type Ref<'a> = ZipExpr<Expr::Ref<'a>, Data::Ref<'a>> where Self: 'a;

    fn take_ref(&self) -> Self::Ref<'_> {
        ZipExpr {
            expr: self.expr.take_ref(),
            data: self.data.take_ref(),
        }
    }
}

impl<K, Expr: IndexExpr<K>, Data: IndexExpr<K>> IndexExpr<K> for ZipExpr<Expr, Data> {
    type Output<'a> = (Expr::Output<'a>, Data::Output<'a>)
    where
        Self: 'a;

    fn index_expr(&self, idx: ExprIdx<K>) -> Self::Output<'_> {
        (self.expr.index_expr(idx), self.data.index_expr(idx))
    }
}

impl<K: ExprKind, Data: IntoIterator, Expr: Expression<K>> Expression<K> for ZipExpr<Expr, Data> {
    type Part = (Expr::Part, Data::Item);
    type Iter = std::iter::Zip<Expr::Iter, Data::IntoIter>;

    type MapOp<ToK: ExprKind> = ZipExpr<Expr::MapOp<ToK>, Data>;

    fn map_op_with_state<ToK: ExprKind, State>(
        self,
        state: &mut State,
        map_nil: impl FnMut(&mut State, <K as ExprKind>::NiladicOp) -> ToK::NiladicOp,
        map_mon: impl FnMut(&mut State, <K as ExprKind>::MonadicOp) -> ToK::MonadicOp,
        map_dya: impl FnMut(&mut State, <K as ExprKind>::DyadicOp) -> ToK::DyadicOp,
        map_var: impl FnMut(&mut State, <K as ExprKind>::VariadicOp) -> ToK::VariadicOp,
    ) -> Self::MapOp<ToK> {
        ZipExpr {
            expr: self
                .expr
                .map_op_with_state(state, map_nil, map_mon, map_dya, map_var),
            data: self.data,
        }
    }

    fn iter_parts(self) -> Self::Iter {
        self.expr.iter_parts().zip(self.data)
    }

    fn len(&self) -> usize {
        self.expr.len()
    }
}

pub struct IntoZip<A, B>(A, B);

impl<A: IntoIterator, B: IntoIterator> IntoIterator for IntoZip<A, B> {
    type Item = (A::Item, B::Item);
    type IntoIter = std::iter::Zip<A::IntoIter, B::IntoIter>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter().zip(self.1)
    }
}
impl<DB: ?Sized, K, Id, A, B> FetchKindData<(A, B), Id, DB> for K
where
    K: ExprKind + FetchKindData<A, Id, DB> + FetchKindData<B, Id, DB>,
    <K as FetchKindData<B, Id, DB>>::Err: From<<K as FetchKindData<A, Id, DB>>::Err>,
    Id: Copy,
{
    type Data =
        IntoZip<<K as FetchKindData<A, Id, DB>>::Data, <K as FetchKindData<B, Id, DB>>::Data>;
    type Err = <K as FetchKindData<B, Id, DB>>::Err;

    fn fetch_kind_data(db: &DB, id: Id) -> Result<Self::Data, Self::Err> {
        Ok(IntoZip(
            <K as FetchKindData<A, Id, DB>>::fetch_kind_data(db, id)?,
            <K as FetchKindData<B, Id, DB>>::fetch_kind_data(db, id)?,
        ))
    }
}

impl<A: TakeRef, B: TakeRef> TakeRef for IntoZip<A, B> {
    type Ref<'a> = IntoZip<A::Ref<'a>, B::Ref<'a>> where Self: 'a;

    fn take_ref(&self) -> Self::Ref<'_> {
        IntoZip(self.0.take_ref(), self.1.take_ref())
    }
}

impl<K, A: IndexExpr<K>, B: IndexExpr<K>> IndexExpr<K> for IntoZip<A, B> {
    type Output<'a> = (A::Output<'a>, B::Output<'a>)
    where
        Self: 'a;

    fn index_expr(&self, idx: ExprIdx<K>) -> Self::Output<'_> {
        (self.0.index_expr(idx), self.1.index_expr(idx))
    }
}

pub type DataExpr<K, D> = ZipExpr<IdxExpression<K>, ShapedData<Vec<D>, K>>;
pub type DataRefExpr<'a, K, D> = ZipExpr<IdxExprRef<'a, K>, ShapedData<&'a [D], K>>;

impl<K: ExprKind, D> DataExpr<K, D> {
    pub fn new_from_unfold<T>(init: T, mut unfold: impl FnMut(T) -> (ExprHead<K, T>, D)) -> Self {
        let mut heads = Vec::new();
        let mut data = Vec::new();
        Self::new_impl(&mut heads, &mut data, init, &mut unfold);
        let idx_expr = IdxExpression { heads };
        Self {
            expr: idx_expr,
            data: ShapedData::from_raw_data(data),
        }
    }

    fn new_impl<T>(
        heads: &mut Vec<ExprHead<K, ExprIdx<K>>>,
        data: &mut Vec<D>,
        element: T,
        unfold: &mut impl FnMut(T) -> (ExprHead<K, T>, D),
    ) -> ExprIdx<K> {
        let (unfolded, datum) = unfold(element);
        let unfolded = unfolded.map_inner(|inner| Self::new_impl(heads, data, inner, unfold));
        let ret = ExprIdx::new_from_usize(heads.len());
        heads.push(unfolded);
        data.push(datum);
        ret
    }
}
