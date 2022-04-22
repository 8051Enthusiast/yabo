use crate::interner::FieldName;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::marker::PhantomData;
use std::sync::Arc;
pub trait ExpressionKind: Clone + Hash + Eq + Debug {
    type NiladicOp: Clone + Hash + Eq + Debug;
    type MonadicOp: Clone + Hash + Eq + Debug;
    type DyadicOp: Clone + Hash + Eq + Debug;
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct OpRef<'a, K: ExpressionKind>(PhantomData<&'a K>);

impl<'a, K: ExpressionKind> ExpressionKind for OpRef<'a, K> {
    type NiladicOp = &'a K::NiladicOp;
    type MonadicOp = &'a K::MonadicOp;
    type DyadicOp = &'a K::DyadicOp;
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Monadic<Op, Inner> {
    pub op: Op,
    pub inner: Inner,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Dyadic<Op, Inner> {
    pub op: Op,
    pub inner: [Inner; 2],
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ExpressionHead<K: ExpressionKind, Inner> {
    Niladic(K::NiladicOp),
    Monadic(Monadic<K::MonadicOp, Inner>),
    Dyadic(Dyadic<K::DyadicOp, Inner>),
}

impl<K: ExpressionKind, Inner> ExpressionHead<K, Inner> {
    #[inline]
    pub fn new_niladic(op: K::NiladicOp) -> Self {
        Self::Niladic(op)
    }

    #[inline]
    pub fn new_monadic(op: K::MonadicOp, inner: Inner) -> Self {
        Self::Monadic(Monadic { op, inner })
    }

    #[inline]
    pub fn new_dyadic(op: K::DyadicOp, inner: [Inner; 2]) -> Self {
        Self::Dyadic(Dyadic { op, inner })
    }

    #[inline]
    pub fn map_inner<NewInner>(
        self,
        mut f: impl FnMut(Inner) -> NewInner,
    ) -> ExpressionHead<K, NewInner> {
        match self {
            Self::Niladic(op) => ExpressionHead::new_niladic(op),
            Self::Monadic(Monadic { op, inner }) => ExpressionHead::new_monadic(op, f(inner)),
            Self::Dyadic(Dyadic { op, inner }) => ExpressionHead::new_dyadic(op, inner.map(f)),
        }
    }

    #[inline]
    pub fn map_op<NewKind: ExpressionKind>(
        self,
        niladic: impl FnOnce(K::NiladicOp) -> NewKind::NiladicOp,
        monadic: impl FnOnce(K::MonadicOp) -> NewKind::MonadicOp,
        dyadic: impl FnOnce(K::DyadicOp) -> NewKind::DyadicOp,
    ) -> ExpressionHead<NewKind, Inner> {
        match self {
            Self::Niladic(op) => ExpressionHead::new_niladic(niladic(op)),
            Self::Monadic(Monadic { op, inner }) => ExpressionHead::new_monadic(monadic(op), inner),
            Self::Dyadic(Dyadic { op, inner }) => ExpressionHead::new_dyadic(dyadic(op), inner),
        }
    }

    #[inline]
    pub fn as_ref(&self) -> ExpressionHead<OpRef<K>, &Inner> {
        match self {
            Self::Niladic(op) => ExpressionHead::new_niladic(op),
            Self::Monadic(Monadic { op, inner }) => ExpressionHead::new_monadic(op, inner),
            Self::Dyadic(Dyadic { op, inner }) => {
                ExpressionHead::new_dyadic(op, [&inner[0], &inner[1]])
            }
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
#[repr(transparent)]
pub struct Expression<K: ExpressionKind>(ExpressionHead<K, Box<Self>>);

impl<K: ExpressionKind> Expression<K> {
    pub fn children(&self) -> Vec<&Self> {
        match &self.0 {
            ExpressionHead::Niladic(_) => vec![],
            ExpressionHead::Monadic(m) => vec![&m.inner],
            ExpressionHead::Dyadic(d) => vec![&d.inner[0], &d.inner[1]],
        }
    }

    #[inline]
    pub fn new_niladic(op: K::NiladicOp) -> Self {
        Self(ExpressionHead::new_niladic(op))
    }

    #[inline]
    pub fn new_monadic(op: K::MonadicOp, inner: Self) -> Self {
        Self(ExpressionHead::new_monadic(op, Box::new(inner)))
    }

    #[inline]
    pub fn new_dyadic(op: K::DyadicOp, inner: [Self; 2]) -> Self {
        let [inner0, inner1] = inner;
        Self(ExpressionHead::new_dyadic(
            op,
            [Box::new(inner0), Box::new(inner1)],
        ))
    }

    pub fn fold<T>(&self, f: &mut impl FnMut(ExpressionHead<OpRef<K>, T>) -> T) -> T {
        let inner_folded = self.0.as_ref().map_inner(|inner| inner.fold(f));
        f(inner_folded)
    }

    pub fn convert<ToKind: ExpressionKind>(
        &self,
        niladic: &mut impl FnMut(&K::NiladicOp) -> ToKind::NiladicOp,
        monadic: &mut impl FnMut(&K::MonadicOp, &Expression<ToKind>) -> ToKind::MonadicOp,
        dyadic: &mut impl FnMut(
            &K::DyadicOp,
            &Expression<ToKind>,
            &Expression<ToKind>,
        ) -> ToKind::DyadicOp,
    ) -> Expression<ToKind> {
        match &self.0 {
            ExpressionHead::Niladic(op) => Expression::new_niladic(niladic(op)),
            ExpressionHead::Monadic(Monadic { op, inner }) => {
                let val = inner.convert(niladic, monadic, dyadic);
                Expression::new_monadic(monadic(op, &val), val)
            }
            ExpressionHead::Dyadic(Dyadic { op, inner }) => {
                let val0 = inner[0].convert(niladic, monadic, dyadic);
                let val1 = inner[1].convert(niladic, monadic, dyadic);
                Expression::new_dyadic(dyadic(op, &val0, &val1), [val0, val1])
            }
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct OpWithData<T, Op> {
    pub data: T,
    pub inner: Op,
}

impl<T, Op> OpWithData<T, Op> {
    pub fn new(data: T, inner: Op) -> Self {
        Self { data, inner }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct KindWithData<K: ExpressionKind, T>(PhantomData<(T, K)>);

impl<T: Clone + Hash + Eq + Debug, K: ExpressionKind> ExpressionKind for KindWithData<K, T> {
    type NiladicOp = OpWithData<T, K::NiladicOp>;
    type MonadicOp = OpWithData<T, K::MonadicOp>;
    type DyadicOp = OpWithData<T, K::DyadicOp>;
}

impl<K: ExpressionKind, T: Clone + Hash + Eq + Debug, Inner>
    ExpressionHead<KindWithData<K, T>, Inner>
{
    fn root_data(&self) -> &T {
        match self {
            ExpressionHead::Niladic(op) => &op.data,
            ExpressionHead::Monadic(head) => &head.op.data,
            ExpressionHead::Dyadic(head) => &head.op.data,
        }
    }
}

impl<T: Clone + Hash + Eq + Debug, K: ExpressionKind> Expression<KindWithData<K, T>> {
    pub fn map<ToT: Clone + Hash + Eq + Debug>(
        &self,
        f: &mut impl FnMut(&T) -> ToT,
    ) -> Expression<KindWithData<K, ToT>> {
        match self.0.as_ref().map_inner(|inner| inner.map(f)) {
            ExpressionHead::Niladic(op) => {
                Expression::new_niladic(OpWithData::new(f(&op.data), op.inner.clone()))
            }
            ExpressionHead::Monadic(Monadic { op, inner }) => {
                Expression::new_monadic(OpWithData::new(f(&op.data), op.inner.clone()), inner)
            }
            ExpressionHead::Dyadic(Dyadic { op, inner }) => {
                Expression::new_dyadic(OpWithData::new(f(&op.data), op.inner.clone()), inner)
            }
        }
    }
    pub fn scan<ToT: Clone + Hash + Eq + Debug>(
        &self,
        f: &mut impl FnMut(ExpressionHead<OpRef<KindWithData<K, T>>, &ToT>) -> ToT,
    ) -> Expression<KindWithData<K, ToT>> {
        match &self.0 {
            ExpressionHead::Niladic(op) => Expression::new_niladic(OpWithData::new(
                f(ExpressionHead::new_niladic(op)),
                op.inner.clone(),
            )),
            ExpressionHead::Monadic(Monadic { op, inner }) => {
                let inner = inner.scan(f);
                let new_op = f(ExpressionHead::new_monadic(op, inner.0.root_data()));
                Expression::new_monadic(OpWithData::new(new_op, op.inner.clone()), inner)
            }
            ExpressionHead::Dyadic(Dyadic { op, inner }) => {
                let inner0 = inner[0].scan(f);
                let inner1 = inner[1].scan(f);
                let new_op = f(ExpressionHead::new_dyadic(
                    op,
                    [inner0.0.root_data(), inner1.0.root_data()],
                ));
                Expression::new_dyadic(OpWithData::new(new_op, op.inner.clone()), [inner0, inner1])
            }
        }
    }
    pub fn try_scan<ToT: Clone + Hash + Eq + Debug, E>(
        &self,
        f: &mut impl FnMut(ExpressionHead<OpRef<KindWithData<K, T>>, &ToT>) -> Result<ToT, E>,
    ) -> Result<Expression<KindWithData<K, ToT>>, E> {
        match &self.0 {
            ExpressionHead::Niladic(op) => Ok(Expression::new_niladic(OpWithData::new(
                f(ExpressionHead::new_niladic(op)).map_err(|e| e)?,
                op.inner.clone(),
            ))),
            ExpressionHead::Monadic(Monadic { op, inner }) => {
                let inner = inner.try_scan(f)?;
                let new_op = f(ExpressionHead::new_monadic(op, inner.0.root_data()))?;
                Ok(Expression::new_monadic(
                    OpWithData::new(new_op, op.inner.clone()),
                    inner,
                ))
            }
            ExpressionHead::Dyadic(Dyadic { op, inner }) => {
                let inner0 = inner[0].try_scan(f)?;
                let inner1 = inner[1].try_scan(f)?;
                let new_op = f(ExpressionHead::new_dyadic(
                    op,
                    [inner0.0.root_data(), inner1.0.root_data()],
                ))?;
                Ok(Expression::new_dyadic(
                    OpWithData::new(new_op, op.inner.clone()),
                    [inner0, inner1],
                ))
            }
        }
    }
}

pub struct ExprIter<'a, K: ExpressionKind> {
    child_list: Vec<Vec<&'a Expression<K>>>,
}

impl<'a, K: ExpressionKind> ExprIter<'a, K> {
    pub fn new(start: &'a Expression<K>) -> Self {
        Self {
            child_list: vec![vec![start]],
        }
    }
}

impl<'a, K: ExpressionKind> Iterator for ExprIter<'a, K> {
    type Item = &'a Expression<K>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(&end) = self.child_list.last()?.last() {
            self.child_list.push(end.children())
        }
        // pop the last empty list off the stack
        self.child_list.pop();
        // return the last element of the previous (non-empty) list (this should never be None)
        self.child_list.last_mut()?.pop()
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Atom {
    Field(FieldName),
    Number(String),
    Char(String),
    String(String),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum TypeBinOp {
    Ref,
    ParseArg,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum TypeUnOp<C: ExpressionKind> {
    Wiggle(Arc<Expression<C>>),
    Ref,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ConstraintBinOp {
    And,
    Or,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ConstraintUnOp {
    Not,
    Dot(Atom),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ValBinOp {
    And,
    Xor,
    Or,
    LesserEq,
    Lesser,
    GreaterEq,
    Greater,
    Uneq,
    Equals,
    ShiftR,
    ShiftL,
    Minus,
    Plus,
    Div,
    Modulo,
    Mul,
    Compose,
    ParserApply,
    Else,
}

impl ValBinOp {
    pub fn parse_from_str(s: &str) -> Result<Self, &str> {
        use ValBinOp::*;
        Ok(match s {
            "&" => And,
            "^" => Xor,
            "|" => Or,
            "<=" => LesserEq,
            "<" => Lesser,
            ">=" => GreaterEq,
            ">" => Greater,
            "!=" => Uneq,
            "==" => Equals,
            ">>" => ShiftR,
            "<<" => ShiftL,
            "-" => Minus,
            "+" => Plus,
            "/" => Div,
            "%" => Modulo,
            "*" => Mul,
            "|>" => Compose,
            "*>" => ParserApply,
            "else" => Else,
            otherwise => return Err(otherwise),
        })
    }
}

impl Display for ValBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ValBinOp::And => "&",
                ValBinOp::Xor => "^",
                ValBinOp::Or => "|",
                ValBinOp::LesserEq => "<=",
                ValBinOp::Lesser => "<",
                ValBinOp::GreaterEq => ">=",
                ValBinOp::Greater => ">",
                ValBinOp::Uneq => "!=",
                ValBinOp::Equals => "==",
                ValBinOp::ShiftR => ">>",
                ValBinOp::ShiftL => "<<",
                ValBinOp::Minus => "-",
                ValBinOp::Plus => "+",
                ValBinOp::Div => "/",
                ValBinOp::Modulo => "%",
                ValBinOp::Mul => "*",
                ValBinOp::Compose => "|>",
                ValBinOp::ParserApply => "*>",
                ValBinOp::Else => "else",
            }
        )
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ValUnOp<C: ExpressionKind> {
    Not,
    Neg,
    Pos,
    If,
    Wiggle(Arc<Expression<C>>),
    Dot(Atom),
}
