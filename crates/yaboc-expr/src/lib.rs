#![allow(clippy::type_complexity)]
mod fetch;
mod idx_expression;
mod part;
mod shaped_data;
mod zip;

use std::hash::Hash;
use std::iter::{Enumerate, Map};
use std::marker::PhantomData;
use std::ops::Index;
use std::{fmt::Debug, num::NonZeroU32};

pub use fetch::{FetchData, FetchExpr, FetchKindData};
pub use idx_expression::{ExprBuilder, ExprDataBuilder, IdxExpression};
pub use part::{ExprHead, ExprPart, TransposablePart};
pub use shaped_data::{IndexExpr, ShapedData};
pub use smallvec::SmallVec;
pub use zip::{DataExpr, DataRefExpr, ZipExpr};

pub trait ExprKind: Clone + Hash + Eq + Debug {
    type NiladicOp: Clone + Hash + Eq + Debug;
    type MonadicOp: Clone + Hash + Eq + Debug;
    type DyadicOp: Clone + Hash + Eq + Debug;
    type VariadicOp: Clone + Hash + Eq + Debug;
}

impl<'a, K: ExprKind> ExprKind for &'a K {
    type NiladicOp = &'a K::NiladicOp;
    type MonadicOp = &'a K::MonadicOp;
    type DyadicOp = &'a K::DyadicOp;
    type VariadicOp = &'a K::VariadicOp;
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum PartialEval<U, E> {
    Uneval(U),
    Eval(E),
}

impl<U, E> PartialEval<U, E> {
    pub fn map<U2>(self, f: impl FnOnce(U) -> U2) -> PartialEval<U2, E> {
        match self {
            PartialEval::Uneval(u) => PartialEval::Uneval(f(u)),
            PartialEval::Eval(e) => PartialEval::Eval(e),
        }
    }
}

pub trait TakeRef {
    type Ref<'a>
    where
        Self: 'a;
    fn take_ref(&self) -> Self::Ref<'_>;
}

impl<T: TakeRef> TakeRef for std::sync::Arc<T> {
    type Ref<'a> = T::Ref<'a>
    where
        Self: 'a;

    fn take_ref(&self) -> Self::Ref<'_> {
        (**self).take_ref()
    }
}

impl<T: TakeRef> TakeRef for std::rc::Rc<T> {
    type Ref<'a> = T::Ref<'a>
    where
        Self: 'a;

    fn take_ref(&self) -> Self::Ref<'_> {
        (**self).take_ref()
    }
}

#[repr(transparent)]
pub struct ExprIdx<K>(NonZeroU32, PhantomData<K>);

impl<K> Clone for ExprIdx<K> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<K> Copy for ExprIdx<K> {}

impl<K> PartialEq for ExprIdx<K> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<K> Eq for ExprIdx<K> {}

impl<K> Hash for ExprIdx<K> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<K> Debug for ExprIdx<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<K> ExprIdx<K> {
    pub fn as_usize(self) -> usize {
        self.0.get() as usize - 1
    }
    pub fn ignore_shape<ToK>(self) -> ExprIdx<ToK> {
        ExprIdx(self.0, PhantomData)
    }
    fn new(idx: &mut usize) -> Self {
        *idx += 1;
        ExprIdx(NonZeroU32::new(*idx as u32).unwrap(), PhantomData)
    }
    pub fn new_from_usize(idx: usize) -> Self {
        ExprIdx(NonZeroU32::new(idx as u32 + 1).unwrap(), PhantomData)
    }
}

pub enum WriteEvent<'a, K: ExprKind> {
    Niladic(&'a K::NiladicOp),
    OpenMonadic(&'a K::MonadicOp),
    CloseMonadic(&'a K::MonadicOp),
    OpenDyadic(&'a K::DyadicOp),
    InterDyadic(&'a K::DyadicOp),
    CloseDyadic(&'a K::DyadicOp),
    OpenVariadic(&'a K::VariadicOp),
    InterVariadic(&'a K::VariadicOp, usize),
    CloseVariadic(&'a K::VariadicOp),
}

#[derive(Default)]
pub struct InvariantLifetime<'id>(PhantomData<*mut &'id ()>);
pub struct ExprRef<'id, K>(ExprIdx<K>, InvariantLifetime<'id>);

// expression should never be empty
#[allow(clippy::len_without_is_empty)]
pub trait Expression<K: ExprKind>: Sized {
    type Part: ExprPart<K = K, Inner = ExprIdx<K>>;
    type Iter: Iterator<Item = Self::Part>;
    fn iter_parts(self) -> Self::Iter;
    fn len(&self) -> usize;

    fn root(&self) -> ExprIdx<K> {
        ExprIdx::new_from_usize(self.len() - 1)
    }

    type MapOp<ToK: ExprKind>: Expression<ToK>;
    fn map_op_with_state<ToK: ExprKind, State>(
        self,
        state: &mut State,
        map_nil: impl FnMut(&mut State, K::NiladicOp) -> ToK::NiladicOp,
        map_mon: impl FnMut(&mut State, K::MonadicOp) -> ToK::MonadicOp,
        map_dya: impl FnMut(&mut State, K::DyadicOp) -> ToK::DyadicOp,
        map_var: impl FnMut(&mut State, K::VariadicOp) -> ToK::VariadicOp,
    ) -> Self::MapOp<ToK>;

    fn map_op<ToK: ExprKind>(
        self,
        mut map_nil: impl FnMut(K::NiladicOp) -> ToK::NiladicOp,
        mut map_mon: impl FnMut(K::MonadicOp) -> ToK::MonadicOp,
        mut map_dya: impl FnMut(K::DyadicOp) -> ToK::DyadicOp,
        mut map_var: impl FnMut(K::VariadicOp) -> ToK::VariadicOp,
    ) -> Self::MapOp<ToK> {
        self.map_op_with_state(
            &mut (),
            |_, op| map_nil(op),
            |_, op| map_mon(op),
            |_, op| map_dya(op),
            |_, op| map_var(op),
        )
    }

    fn map_niladic<ToK>(
        self,
        map_nil: impl FnMut(K::NiladicOp) -> ToK::NiladicOp,
    ) -> Self::MapOp<ToK>
    where
        ToK: ExprKind<MonadicOp = K::MonadicOp, DyadicOp = K::DyadicOp, VariadicOp = K::VariadicOp>,
    {
        self.map_op(map_nil, |op| op, |op| op, |op| op)
    }

    fn iter_parts_with_idx(
        self,
    ) -> Map<Enumerate<Self::Iter>, fn((usize, Self::Part)) -> (ExprIdx<K>, Self::Part)> {
        self.iter_parts()
            .enumerate()
            .map(|(idx, part)| (ExprIdx::new_from_usize(idx), part))
    }

    fn fold<T>(self, mut f: impl FnMut(<Self::Part as ExprPart>::TransExpr<K, T>) -> T) -> T {
        let mut ret_vec: Vec<Option<T>> = Vec::with_capacity(self.len());
        for part in self.iter_parts() {
            let part = part.map_inner(|idx| ret_vec[idx.as_usize()].take().unwrap());
            ret_vec.push(Some(f(part)));
        }
        ret_vec.pop().unwrap().unwrap()
    }

    fn try_fold<T, E>(
        self,
        mut f: impl FnMut(<Self::Part as ExprPart>::TransExpr<K, T>) -> Result<T, E>,
    ) -> Result<T, E> {
        let mut ret_vec: Vec<Option<T>> = Vec::with_capacity(self.len());
        for part in self.iter_parts() {
            let part = part.map_inner(|idx| ret_vec[idx.as_usize()].take().unwrap());
            ret_vec.push(Some(f(part)?));
        }
        Ok(ret_vec.pop().unwrap().unwrap())
    }

    fn scan<T: Clone>(
        self,
        mut f: impl FnMut(<Self::Part as ExprPart>::TransExpr<K, &T>) -> T,
    ) -> ShapedData<Vec<T>, K> {
        let mut ret_vec: Vec<T> = Vec::with_capacity(self.len());
        for part in self.iter_parts() {
            let part = part.map_inner(|idx| &ret_vec[idx.as_usize()]);
            ret_vec.push(f(part));
        }
        ShapedData {
            data: ret_vec,
            _marker: PhantomData,
        }
    }

    fn try_scan<T: Clone, E>(
        self,
        mut f: impl FnMut(<Self::Part as ExprPart>::TransExpr<K, &T>) -> Result<T, E>,
    ) -> Result<ShapedData<Vec<T>, K>, E> {
        let mut ret_vec: Vec<T> = Vec::with_capacity(self.len());
        for part in self.iter_parts() {
            let part = part.map_inner(|idx| &ret_vec[idx.as_usize()]);
            ret_vec.push(f(part)?);
        }
        Ok(ShapedData {
            data: ret_vec,
            _marker: PhantomData,
        })
    }

    fn for_each(self, mut f: impl FnMut(Self::Part)) {
        for part in self.iter_parts() {
            f(part);
        }
    }

    fn try_for_each<E>(self, mut f: impl FnMut(Self::Part) -> Result<(), E>) -> Result<(), E> {
        for part in self.iter_parts() {
            f(part)?;
        }
        Ok(())
    }

    fn print(&self, mut f: impl FnMut(WriteEvent<K>))
    where
        Self: Index<ExprIdx<K>, Output = ExprHead<K, ExprIdx<K>>>,
    {
        enum Never {}
        let _ = print_impl::<_, _, Never>(self, self.root(), &mut |x| {
            f(x);
            Ok(())
        });
    }
    fn try_print<E>(&self, mut f: impl FnMut(WriteEvent<K>) -> Result<(), E>) -> Result<(), E>
    where
        Self: Index<ExprIdx<K>, Output = ExprHead<K, ExprIdx<K>>>,
    {
        print_impl(self, self.root(), &mut f)
    }

    fn partial_eval<Ev: Clone + Hash + Debug + Eq, ToK: ExprKind>(
        self,
        mut default: impl FnMut(Ev, ExprIdx<K>) -> ToK::NiladicOp,
        mut f: impl for<'id> FnMut(
            InvariantLifetime<'id>,
            <Self::Part as ExprPart>::TransExpr<K, PartialEval<ExprRef<'id, ToK>, (Ev, ExprIdx<K>)>>,
        ) -> PartialEval<
            ExprHead<ToK, PartialEval<ExprRef<'id, ToK>, (Ev, ExprIdx<K>)>>,
            Ev,
        >,
    ) -> ReidxExpr<K, ToK> {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        enum Never {}
        match self.try_partial_eval::<_, _, Never>(
            |ev, idx| Ok(default(ev, idx)),
            |inv, expr| Ok(f(inv, expr)),
        ) {
            Ok(x) => x,
            Err(v) => match v {},
        }
    }

    fn try_partial_eval<Ev: Clone + Hash + Debug + Eq, ToK: ExprKind, Error>(
        self,
        mut default: impl FnMut(Ev, ExprIdx<K>) -> Result<ToK::NiladicOp, Error>,
        mut f: impl for<'id> FnMut(
            InvariantLifetime<'id>,
            <Self::Part as ExprPart>::TransExpr<K, PartialEval<ExprRef<'id, ToK>, (Ev, ExprIdx<K>)>>,
        ) -> Result<
            PartialEval<ExprHead<ToK, PartialEval<ExprRef<'id, ToK>, (Ev, ExprIdx<K>)>>, Ev>,
            Error,
        >,
    ) -> Result<ReidxExpr<K, ToK>, Error> {
        let mut heads: Vec<ExprHead<ToK, ExprIdx<ToK>>> = Vec::with_capacity(self.len());
        let mut results: Vec<(ExprIdx<K>, PartialEval<ExprIdx<ToK>, Ev>)> =
            Vec::with_capacity(self.len());
        let mut reindex: Vec<ExprIdx<K>> = Vec::with_capacity(self.len());
        let mut i = 0;
        let mut add_to_expr = |(idx, expr): (ExprIdx<K>, ExprHead<ToK, ExprIdx<ToK>>)| {
            heads.push(expr);
            reindex.push(idx);
            ExprIdx::new(&mut i)
        };
        for (idx, part) in self.iter_parts_with_idx() {
            let part = part.map_inner(|new_idx| {
                let ret = results[new_idx.as_usize()].clone();
                match ret.1 {
                    PartialEval::Uneval(x) => {
                        PartialEval::Uneval(ExprRef(x, InvariantLifetime::default()))
                    }
                    PartialEval::Eval(x) => PartialEval::Eval((x, ret.0)),
                }
            });
            let evaled = f(InvariantLifetime::default(), part)?;
            let res = match evaled {
                PartialEval::Uneval(x) => {
                    let expr_with_subidx = x
                        .map_inner(|subexpr| {
                            let (ev, subidx) = match subexpr {
                                PartialEval::Uneval(idx) => return Ok(idx.0),
                                PartialEval::Eval(ev) => ev,
                            };
                            Ok(add_to_expr((
                                subidx,
                                ExprHead::Niladic(default(ev, subidx)?),
                            )))
                        })
                        .transpose()?;
                    PartialEval::Uneval(add_to_expr((idx, expr_with_subidx)))
                }
                PartialEval::Eval(e) => PartialEval::Eval(e),
            };
            results.push((idx, res));
        }
        if let (idx, PartialEval::Eval(res)) = results.pop().unwrap() {
            add_to_expr((idx, ExprHead::Niladic(default(res, idx)?)));
        }
        let expr = IdxExpression { heads };
        let reidx = ReidxExpr {
            expr,
            reidx: reindex,
        };
        Ok(reidx)
    }
}

fn print_impl<K: ExprKind, Expr, E>(
    expr: &Expr,
    idx: ExprIdx<K>,
    f: &mut impl FnMut(WriteEvent<K>) -> Result<(), E>,
) -> Result<(), E>
where
    Expr: Index<ExprIdx<K>, Output = ExprHead<K, ExprIdx<K>>> + Expression<K>,
{
    let part = &expr[idx];
    match part {
        ExprHead::Niladic(op) => f(WriteEvent::Niladic(op))?,
        ExprHead::Monadic(op, arg) => {
            f(WriteEvent::OpenMonadic(op))?;
            print_impl(expr, *arg, f)?;
            f(WriteEvent::CloseMonadic(op))?;
        }
        ExprHead::Dyadic(op, [lhs, rhs]) => {
            f(WriteEvent::OpenDyadic(op))?;
            print_impl(expr, *lhs, f)?;
            f(WriteEvent::InterDyadic(op))?;
            print_impl(expr, *rhs, f)?;
            f(WriteEvent::CloseDyadic(op))?;
        }
        ExprHead::Variadic(op, args) => {
            f(WriteEvent::OpenVariadic(op))?;
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    f(WriteEvent::InterVariadic(op, i - 1))?;
                }
                print_impl(expr, *arg, f)?;
            }
            f(WriteEvent::CloseVariadic(op))?;
        }
    };
    Ok(())
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct IdxExprRef<'a, K: ExprKind>(&'a IdxExpression<K>);

impl<'a, K: ExprKind> IdxExprRef<'a, K> {
    pub fn get(&self, idx: ExprIdx<K>) -> &ExprHead<K, ExprIdx<K>> {
        &self.0[idx]
    }

    pub fn zip<D>(self, other: D) -> ZipExpr<Self, D> {
        ZipExpr {
            expr: self,
            data: other,
        }
    }
}

impl<'a, K: ExprKind> Index<ExprIdx<K>> for IdxExprRef<'a, K> {
    type Output = ExprHead<K, ExprIdx<K>>;
    fn index(&self, idx: ExprIdx<K>) -> &Self::Output {
        &self.0[idx]
    }
}

impl<'a, K: ExprKind> IndexExpr<K> for IdxExprRef<'a, K> {
    type Output<'b> = &'b ExprHead<K, ExprIdx<K>>
    where
        Self: 'b;

    fn index_expr(&self, idx: ExprIdx<K>) -> Self::Output<'_> {
        &self.0[idx]
    }
}

impl<'a, K: ExprKind> Expression<K> for IdxExprRef<'a, K> {
    type Part = ExprHead<K, ExprIdx<K>>;
    type Iter = std::iter::Cloned<std::slice::Iter<'a, Self::Part>>;

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
            .0
            .heads
            .iter()
            .map(|h| {
                h.clone()
                    .map_op_with_state(
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
        self.0.heads.iter().cloned()
    }

    fn len(&self) -> usize {
        self.0.len()
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ReidxExpr<FromK: ExprKind, K: ExprKind> {
    expr: IdxExpression<K>,
    reidx: Vec<ExprIdx<FromK>>,
}

impl<FromK: ExprKind, K: ExprKind> ReidxExpr<FromK, K> {
    pub fn migrate_data<D, S>(&self, data: &ShapedData<S, FromK>) -> ShapedData<Vec<D>, K>
    where
        S: Index<usize, Output = D>,
        D: Clone,
    {
        let new_data = self.reidx.iter().map(|idx| data[*idx].clone()).collect();
        ShapedData {
            data: new_data,
            _marker: PhantomData,
        }
    }
    pub fn expr(&self) -> &IdxExpression<K> {
        &self.expr
    }
    pub fn into_expr(self) -> IdxExpression<K> {
        self.expr
    }
}

#[cfg(test)]
mod tests;
