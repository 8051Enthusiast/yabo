use smallvec::SmallVec;

use crate::ExprKind;

pub trait ExprPart: Sized {
    type K: ExprKind;
    type Inner;
    type TransExpr<ToK: ExprKind, ToInner>: ExprPart<K = ToK, Inner = ToInner>;
    fn transitivity<K2: ExprKind, K3: ExprKind, Inner2, Inner3>(
        s: <Self::TransExpr<K2, Inner2> as ExprPart>::TransExpr<K3, Inner3>,
    ) -> Self::TransExpr<K3, Inner3>;
    fn map_core_expr<ToK: ExprKind, ToInner>(
        self,
        f: impl FnOnce(ExprHead<Self::K, Self::Inner>) -> ExprHead<ToK, ToInner>,
    ) -> Self::TransExpr<ToK, ToInner>;
    type AsRef<'a>: ExprPart<K = &'a Self::K, Inner = &'a Self::Inner>
    where
        Self: 'a;
    fn as_ref(&self) -> Self::AsRef<'_>;
    fn map_inner<NewInner>(
        self,
        f: impl FnMut(Self::Inner) -> NewInner,
    ) -> Self::TransExpr<Self::K, NewInner> {
        self.map_core_expr(|head| head.map_inner(f))
    }
}

pub trait TransposablePart: ExprPart<Inner = Result<Self::InnerOutput, Self::InnerError>> {
    type InnerOutput;
    type InnerError;
    type Transposed: ExprPart<
        K = Self::K,
        Inner = Self::InnerOutput,
        TransExpr<Self::K, Result<Self::InnerOutput, Self::InnerError>> = Self,
    >;
    fn transpose(self) -> Result<Self::Transposed, Self::InnerError>;
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ExprHead<K: ExprKind, Inner> {
    Niladic(K::NiladicOp),
    Monadic(K::MonadicOp, Inner),
    Dyadic(K::DyadicOp, [Inner; 2]),
    Variadic(K::VariadicOp, SmallVec<[Inner; 4]>),
}

impl<K: ExprKind, Inner> ExprHead<K, Inner> {
    pub fn map_inner<NewInner>(
        self,
        mut f: impl FnMut(Inner) -> NewInner,
    ) -> ExprHead<K, NewInner> {
        match self {
            ExprHead::Niladic(op) => ExprHead::Niladic(op),
            ExprHead::Monadic(op, inner) => ExprHead::Monadic(op, f(inner)),
            ExprHead::Dyadic(op, inner) => ExprHead::Dyadic(op, inner.map(f)),
            ExprHead::Variadic(op, inner) => {
                ExprHead::Variadic(op, inner.into_iter().map(f).collect())
            }
        }
    }
    pub const fn new_niladic(op: K::NiladicOp) -> Self {
        ExprHead::Niladic(op)
    }
    pub const fn new_monadic(op: K::MonadicOp, inner: Inner) -> Self {
        ExprHead::Monadic(op, inner)
    }
    pub const fn new_dyadic(op: K::DyadicOp, inner: [Inner; 2]) -> Self {
        ExprHead::Dyadic(op, inner)
    }
    pub const fn new_variadic(op: K::VariadicOp, inner: SmallVec<[Inner; 4]>) -> Self {
        ExprHead::Variadic(op, inner)
    }
    pub fn map_op_with_state<ToK: ExprKind, T>(
        self,
        state: T,
        map_nil: impl FnOnce(T, <K as ExprKind>::NiladicOp) -> ToK::NiladicOp,
        map_mon: impl FnOnce(T, <K as ExprKind>::MonadicOp) -> ToK::MonadicOp,
        map_dya: impl FnOnce(T, <K as ExprKind>::DyadicOp) -> ToK::DyadicOp,
        map_var: impl FnOnce(T, <K as ExprKind>::VariadicOp) -> ToK::VariadicOp,
    ) -> ExprHead<ToK, Inner> {
        match self {
            ExprHead::Niladic(op) => ExprHead::Niladic(map_nil(state, op)),
            ExprHead::Monadic(op, inner) => ExprHead::Monadic(map_mon(state, op), inner),
            ExprHead::Dyadic(op, inner) => ExprHead::Dyadic(map_dya(state, op), inner),
            ExprHead::Variadic(op, inner) => ExprHead::Variadic(map_var(state, op), inner),
        }
    }
    pub fn map_op<ToK: ExprKind>(
        self,
        map_nil: impl FnOnce(<K as ExprKind>::NiladicOp) -> ToK::NiladicOp,
        map_mon: impl FnOnce(<K as ExprKind>::MonadicOp) -> ToK::MonadicOp,
        map_dya: impl FnOnce(<K as ExprKind>::DyadicOp) -> ToK::DyadicOp,
        map_var: impl FnOnce(<K as ExprKind>::VariadicOp) -> ToK::VariadicOp,
    ) -> ExprHead<ToK, Inner> {
        self.map_op_with_state(
            (),
            |(), op| map_nil(op),
            |(), op| map_mon(op),
            |(), op| map_dya(op),
            |(), op| map_var(op),
        )
    }
    pub fn inner_refs(&self) -> &[Inner] {
        match self {
            ExprHead::Niladic(_) => &[],
            ExprHead::Monadic(_, inner) => std::slice::from_ref(inner),
            ExprHead::Dyadic(_, inner) => inner,
            ExprHead::Variadic(_, inner) => inner,
        }
    }
}

impl<K: ExprKind, Inner> ExprPart for ExprHead<K, Inner> {
    type K = K;
    type Inner = Inner;
    type TransExpr<ToK: ExprKind, ToInner> = ExprHead<ToK, ToInner>;

    #[inline(always)]
    fn transitivity<K2: ExprKind, K3: ExprKind, Inner2, Inner3>(
        s: <Self::TransExpr<K2, Inner2> as ExprPart>::TransExpr<K3, Inner3>,
    ) -> Self::TransExpr<K3, Inner3> {
        s
    }

    fn map_core_expr<ToK: ExprKind, ToInner>(
        self,
        f: impl FnOnce(ExprHead<K, Inner>) -> ExprHead<ToK, ToInner>,
    ) -> Self::TransExpr<ToK, ToInner> {
        f(self)
    }

    type AsRef<'a> = ExprHead<&'a K, &'a Inner>
    where
        K: 'a,
        Inner: 'a,
        Self: 'a;

    fn as_ref(&self) -> Self::AsRef<'_> {
        match self {
            ExprHead::Niladic(op) => ExprHead::Niladic(op),
            ExprHead::Monadic(op, inner) => ExprHead::Monadic(op, inner),
            ExprHead::Dyadic(op, [inner0, inner1]) => ExprHead::Dyadic(op, [inner0, inner1]),
            ExprHead::Variadic(op, inner) => ExprHead::Variadic(op, inner.iter().collect()),
        }
    }
}

impl<K: ExprKind, Inner, E> TransposablePart for ExprHead<K, Result<Inner, E>> {
    type InnerError = E;
    type InnerOutput = Inner;
    type Transposed = ExprHead<K, Inner>;
    fn transpose(self) -> Result<Self::Transposed, E> {
        match self {
            ExprHead::Niladic(op) => Ok(ExprHead::Niladic(op)),
            ExprHead::Monadic(op, inner) => Ok(ExprHead::Monadic(op, inner?)),
            ExprHead::Dyadic(op, [inner0, inner1]) => Ok(ExprHead::Dyadic(op, [inner0?, inner1?])),
            ExprHead::Variadic(op, inner) => Ok(ExprHead::Variadic(
                op,
                inner.into_iter().collect::<Result<SmallVec<_>, _>>()?,
            )),
        }
    }
}

impl<Expr: ExprPart, Data> ExprPart for (Expr, Data) {
    type K = Expr::K;
    type Inner = Expr::Inner;
    type TransExpr<ToK: ExprKind, ToInner> = (Expr::TransExpr<ToK, ToInner>, Data);

    #[inline(always)]
    fn transitivity<K2: ExprKind, K3: ExprKind, Inner2, Inner3>(
        s: <Self::TransExpr<K2, Inner2> as ExprPart>::TransExpr<K3, Inner3>,
    ) -> Self::TransExpr<K3, Inner3> {
        (Expr::transitivity(s.0), s.1)
    }

    fn map_core_expr<ToK: ExprKind, ToInner>(
        self,
        f: impl FnOnce(ExprHead<Self::K, Self::Inner>) -> ExprHead<ToK, ToInner>,
    ) -> Self::TransExpr<ToK, ToInner> {
        (self.0.map_core_expr(f), self.1)
    }

    type AsRef<'a> = (Expr::AsRef<'a>, &'a Data)
    where
        Self: 'a;

    fn as_ref(&self) -> Self::AsRef<'_> {
        (self.0.as_ref(), &self.1)
    }
}

impl<Expr: TransposablePart, Data> TransposablePart for (Expr, Data) {
    type InnerOutput = Expr::InnerOutput;

    type InnerError = Expr::InnerError;

    type Transposed = (Expr::Transposed, Data);

    fn transpose(self) -> Result<Self::Transposed, Self::InnerError> {
        Ok((self.0.transpose()?, self.1))
    }
}
