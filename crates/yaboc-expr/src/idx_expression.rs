use std::ops::Index;

use crate::{IdxExprRef, ExprHead, ExprIdx, ExprKind, InvariantLifetime, ShapedData, ZipExpression, Expression, ExprRef};

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct IdxExpression<K: ExprKind> {
    pub(crate) heads: Vec<ExprHead<K, ExprIdx<K>>>,
}

impl<K: ExprKind> IdxExpression<K> {
    pub fn new_from_unfold<T>(init: T, mut unfold: impl FnMut(T) -> ExprHead<K, T>) -> Self {
        let mut heads = Vec::new();
        Self::new_impl(&mut heads, init, &mut unfold);
        Self { heads }
    }
    fn new_impl<T>(
        heads: &mut Vec<ExprHead<K, ExprIdx<K>>>,
        element: T,
        unfold: &mut impl FnMut(T) -> ExprHead<K, T>,
    ) -> ExprIdx<K> {
        let unfolded = unfold(element).map_inner(|inner| Self::new_impl(heads, inner, unfold));
        let ret = ExprIdx::new_from_usize(heads.len());
        heads.push(unfolded);
        ret
    }

    pub fn as_ref(&self) -> IdxExprRef<K> {
        IdxExprRef(self)
    }

    pub fn build_new<R>(f: impl for<'id> FnOnce(&mut ExprBuilder<'id, K>) -> R) -> Self {
        let mut builder = ExprBuilder {
            heads: vec![],
            _marker: InvariantLifetime::default(),
        };
        f(&mut builder);
        IdxExpression {
            heads: builder.heads,
        }
    }
    pub fn build_new_with_data<R, D>(
        f: impl for<'id> FnOnce(&mut ExprDataBuilder<'id, K, D>) -> R,
    ) -> ZipExpression<Self, ShapedData<Vec<D>, K>> {
        let mut builder = ExprDataBuilder {
            heads: vec![],
            data: vec![],
            _marker: InvariantLifetime::default(),
        };
        f(&mut builder);
        ZipExpression {
            expr: IdxExpression {
                heads: builder.heads,
            },
            data: ShapedData::from_raw_data(builder.data),
        }
    }
    pub fn get(&self, idx: ExprIdx<K>) -> &ExprHead<K, ExprIdx<K>> {
        &self.heads[idx.as_usize()]
    }
}

impl<K: ExprKind> Index<ExprIdx<K>> for IdxExpression<K> {
    type Output = ExprHead<K, ExprIdx<K>>;
    fn index(&self, idx: ExprIdx<K>) -> &Self::Output {
        &self.heads[idx.as_usize()]
    }
}

impl<K: ExprKind> Expression<K> for IdxExpression<K> {
    type Part = ExprHead<K, ExprIdx<K>>;
    type Iter = std::vec::IntoIter<Self::Part>;

    type MapOp<ToK: ExprKind> = IdxExpression<ToK>;

    fn map_op_with_state<ToK: ExprKind, State>(
        self,
        state: &mut State,
        mut map_nil: impl FnMut(&mut State, <K as ExprKind>::NiladicOp) -> ToK::NiladicOp,
        mut map_mon: impl FnMut(&mut State, <K as ExprKind>::MonadicOp) -> ToK::MonadicOp,
        mut map_dya: impl FnMut(&mut State, <K as ExprKind>::DyadicOp) -> ToK::DyadicOp,
        mut map_var: impl FnMut(&mut State, <K as ExprKind>::VariadicOp) -> ToK::VariadicOp,
    ) -> Self::MapOp<ToK> {
        let heads = self
            .heads
            .into_iter()
            .map(move |h| {
                h.map_op_with_state(
                    &mut *state,
                    &mut map_nil,
                    &mut map_mon,
                    &mut map_dya,
                    &mut map_var,
                )
                .map_inner(|x| x.ignore_shape())
            })
            .collect::<Vec<_>>();
        IdxExpression { heads }
    }

    fn iter_parts(self) -> Self::Iter {
        self.heads.into_iter()
    }

    fn len(&self) -> usize {
        self.heads.len()
    }
}

pub struct ExprBuilder<'id, K: ExprKind> {
    heads: Vec<ExprHead<K, ExprIdx<K>>>,
    _marker: InvariantLifetime<'id>,
}

impl<'id, K: ExprKind> ExprBuilder<'id, K> {
    pub fn add_expr(&mut self, head: ExprHead<K, ExprRef<'id, K>>) -> ExprRef<'id, K> {
        let idx = self.heads.len();
        self.heads.push(head.map_inner(|expr| expr.0));
        ExprRef(ExprIdx::new_from_usize(idx), InvariantLifetime::default())
    }
}

pub struct ExprDataBuilder<'id, K: ExprKind, Data> {
    pub heads: Vec<ExprHead<K, ExprIdx<K>>>,
    pub data: Vec<Data>,
    _marker: InvariantLifetime<'id>,
}

impl<'id, K: ExprKind, Data> ExprDataBuilder<'id, K, Data> {
    pub fn add_expr(&mut self, head: ExprHead<K, ExprRef<'id, K>>, data: Data) -> ExprRef<'id, K> {
        let idx = self.heads.len();
        self.heads.push(head.map_inner(|expr| expr.0));
        self.data.push(data);
        ExprRef(ExprIdx::new_from_usize(idx), InvariantLifetime::default())
    }
}
