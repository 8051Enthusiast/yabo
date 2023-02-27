use crate::{ExprHead, ExprIdx, ExprKind, Expression, IdxExprRef, IdxExpression, ShapedData};

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct ZipExpression<Expr, Data> {
    pub expr: Expr,
    pub data: Data,
}

impl<Expr, Data: IntoIterator> ZipExpression<Expr, Data> {
    pub fn zip<D2: IntoIterator>(
        self,
        data: D2,
    ) -> ZipExpression<Expr, impl Iterator<Item = (Data::Item, D2::Item)>> {
        ZipExpression {
            expr: self.expr,
            data: self.data.into_iter().zip(data.into_iter()),
        }
    }

    pub fn map<D2>(
        self,
        f: impl FnMut(Data::Item) -> D2,
    ) -> ZipExpression<Expr, impl Iterator<Item = D2>> {
        ZipExpression {
            expr: self.expr,
            data: self.data.into_iter().map(f),
        }
    }

    pub fn collect(self) -> ZipExpression<Expr, Vec<Data::Item>> {
        ZipExpression {
            expr: self.expr,
            data: self.data.into_iter().collect(),
        }
    }
}

impl<'a, Expr, D: 'a, Data: IntoIterator<Item = &'a D>> ZipExpression<Expr, Data> {
    pub fn cloned(self) -> ZipExpression<Expr, std::iter::Cloned<Data::IntoIter>>
    where
        D: Clone,
    {
        ZipExpression {
            expr: self.expr,
            data: self.data.into_iter().cloned(),
        }
    }
}

impl<K: ExprKind, Data> ZipExpression<IdxExpression<K>, Data> {
    pub fn as_ref(&self) -> ZipExpression<IdxExprRef<K>, &Data> {
        ZipExpression {
            expr: self.expr.as_ref(),
            data: &self.data,
        }
    }
}

impl<K: ExprKind, Data: IntoIterator, Expr: Expression<K>> Expression<K>
    for ZipExpression<Expr, Data>
{
    type Part = (Expr::Part, Data::Item);
    type Iter = std::iter::Zip<Expr::Iter, Data::IntoIter>;

    type MapOp<ToK: ExprKind> = ZipExpression<Expr::MapOp<ToK>, Data>;

    fn map_op_with_state<ToK: ExprKind, State>(
        self,
        state: &mut State,
        map_nil: impl FnMut(&mut State, <K as ExprKind>::NiladicOp) -> ToK::NiladicOp,
        map_mon: impl FnMut(&mut State, <K as ExprKind>::MonadicOp) -> ToK::MonadicOp,
        map_dya: impl FnMut(&mut State, <K as ExprKind>::DyadicOp) -> ToK::DyadicOp,
        map_var: impl FnMut(&mut State, <K as ExprKind>::VariadicOp) -> ToK::VariadicOp,
    ) -> Self::MapOp<ToK> {
        ZipExpression {
            expr: self
                .expr
                .map_op_with_state(state, map_nil, map_mon, map_dya, map_var),
            data: self.data,
        }
    }

    fn iter_parts(self) -> Self::Iter {
        self.expr.iter_parts().zip(self.data.into_iter())
    }

    fn len(&self) -> usize {
        self.expr.len()
    }
}

pub type DataExpr<K, D> = ZipExpression<IdxExpression<K>, ShapedData<Vec<D>, K>>;
pub type DataRefExpr<'a, K, D> = ZipExpression<IdxExprRef<'a, K>, &'a ShapedData<Vec<D>, K>>;

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
