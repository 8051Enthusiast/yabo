use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::marker::PhantomData;
use yaboc_base::interner::FieldName;

pub trait ExpressionKind: Clone + Hash + Eq + Debug {
    type NiladicOp: Clone + Hash + Eq + Debug;
    type MonadicOp: Clone + Hash + Eq + Debug;
    type DyadicOp: Clone + Hash + Eq + Debug;
    type VariadicOp: Clone + Hash + Eq + Debug;
}

impl<'a, K: ExpressionKind> ExpressionKind for &'a K {
    type NiladicOp = &'a K::NiladicOp;
    type MonadicOp = &'a K::MonadicOp;
    type DyadicOp = &'a K::DyadicOp;
    type VariadicOp = &'a K::VariadicOp;
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Monadic<Op, Inner> {
    pub op: Op,
    pub inner: Inner,
}

pub type MonadicExpr<K> = Monadic<<K as ExpressionKind>::MonadicOp, Box<Expression<K>>>;

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Dyadic<Op, Inner> {
    pub op: Op,
    pub inner: [Inner; 2],
}

pub type DyadicExpr<K> = Dyadic<<K as ExpressionKind>::DyadicOp, Box<Expression<K>>>;

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Variadic<Op, Inner> {
    pub op: Op,
    pub inner: Vec<Inner>,
}

pub type VariadicExpr<K> = Variadic<<K as ExpressionKind>::VariadicOp, Box<Expression<K>>>;

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ExpressionHead<K: ExpressionKind, Inner> {
    Niladic(K::NiladicOp),
    Monadic(Monadic<K::MonadicOp, Inner>),
    Dyadic(Dyadic<K::DyadicOp, Inner>),
    Variadic(Variadic<K::VariadicOp, Inner>),
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
    pub fn new_variadic(op: K::VariadicOp, inner: Vec<Inner>) -> Self {
        Self::Variadic(Variadic { op, inner })
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
            Self::Variadic(Variadic { op, inner }) => {
                ExpressionHead::new_variadic(op, inner.into_iter().map(f).collect())
            }
        }
    }

    #[inline]
    pub fn try_map_inner<NewInner, Error>(
        self,
        f: impl FnMut(Inner) -> Result<NewInner, Error>,
    ) -> Result<ExpressionHead<K, NewInner>, Error> {
        self.map_inner(f).transpose()
    }

    #[inline]
    pub fn as_ref(&self) -> ExpressionHead<&K, &Inner> {
        match self {
            Self::Niladic(op) => ExpressionHead::new_niladic(op),
            Self::Monadic(Monadic { op, inner }) => ExpressionHead::new_monadic(op, inner),
            Self::Dyadic(Dyadic { op, inner }) => {
                ExpressionHead::new_dyadic(op, [&inner[0], &inner[1]])
            }
            Self::Variadic(Variadic { op, inner }) => {
                ExpressionHead::new_variadic(op, inner.iter().collect())
            }
        }
    }

    pub fn unfold(self, f: &mut impl FnMut(Inner) -> ExpressionHead<K, Inner>) -> Expression<K> {
        let inner_mapped = self.map_inner(|inner| Box::new(f(inner).unfold(f)));
        Expression(inner_mapped)
    }
}

impl<K: ExpressionKind, T, E> ExpressionHead<K, Result<T, E>> {
    #[inline]
    pub fn transpose(self) -> Result<ExpressionHead<K, T>, E> {
        match self {
            ExpressionHead::Niladic(f) => Ok(ExpressionHead::new_niladic(f)),
            ExpressionHead::Monadic(Monadic { op, inner }) => {
                Ok(ExpressionHead::new_monadic(op, inner?))
            }
            ExpressionHead::Dyadic(Dyadic {
                op,
                inner: [inner0, inner1],
            }) => Ok(ExpressionHead::new_dyadic(op, [inner0?, inner1?])),
            ExpressionHead::Variadic(Variadic { op, inner }) => {
                let inner = inner.into_iter().collect::<Result<Vec<_>, _>>()?;
                Ok(ExpressionHead::new_variadic(op, inner))
            }
        }
    }
}

impl<'a, K: ExpressionKind, Inner> ExpressionHead<&'a K, Inner> {
    #[inline]
    pub fn clone_op(self) -> ExpressionHead<K, Inner> {
        match self {
            Self::Niladic(op) => ExpressionHead::new_niladic((*op).clone()),
            Self::Monadic(Monadic { op, inner }) => {
                ExpressionHead::new_monadic((*op).clone(), inner)
            }
            Self::Dyadic(Dyadic { op, inner }) => ExpressionHead::new_dyadic((*op).clone(), inner),
            Self::Variadic(Variadic { op, inner }) => {
                ExpressionHead::new_variadic((*op).clone(), inner)
            }
        }
    }
    #[inline]
    pub fn inner_as_ref<'b>(&'b self) -> ExpressionHead<&'a K, &'b Inner> {
        self.as_ref().clone_op()
    }
}

impl<K: ExpressionKind, Inner: Clone> ExpressionHead<&K, &Inner> {
    pub fn make_owned(self) -> ExpressionHead<K, Inner> {
        self.clone_op().map_inner(Clone::clone)
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
#[repr(transparent)]
pub struct Expression<K: ExpressionKind>(pub ExpressionHead<K, Box<Self>>);

impl<K: ExpressionKind> Expression<K> {
    pub fn children(&self) -> Vec<&Self> {
        match &self.0 {
            ExpressionHead::Niladic(_) => vec![],
            ExpressionHead::Monadic(m) => vec![&m.inner],
            ExpressionHead::Dyadic(d) => vec![&d.inner[0], &d.inner[1]],
            ExpressionHead::Variadic(v) => v.inner.iter().map(|x| x.as_ref()).collect(),
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
    #[inline]
    pub fn new_variadic(op: K::VariadicOp, inner: Vec<Self>) -> Self {
        Self(ExpressionHead::new_variadic(
            op,
            inner.into_iter().map(Box::new).collect(),
        ))
    }
    pub fn fold<T>(&self, f: &mut impl FnMut(ExpressionHead<&K, T>) -> T) -> T {
        let inner_folded = self.0.as_ref().map_inner(|inner| inner.fold(f));
        f(inner_folded)
    }
    pub fn try_fold<T, E>(
        self,
        f: &mut impl FnMut(ExpressionHead<K, T>) -> Result<T, E>,
    ) -> Result<T, E> {
        let inner_folded = self.0.map_inner(|inner| inner.try_fold(f)).transpose()?;
        f(inner_folded)
    }
    pub fn convert_no_var<ToKind: ExpressionKind>(
        &self,
        niladic: &mut impl FnMut(&K::NiladicOp) -> ToKind::NiladicOp,
        monadic: &mut impl FnMut(&K::MonadicOp, &Expression<ToKind>) -> ToKind::MonadicOp,
        dyadic: &mut impl FnMut(
            &K::DyadicOp,
            &Expression<ToKind>,
            &Expression<ToKind>,
        ) -> ToKind::DyadicOp,
    ) -> Expression<ToKind>
    where
        K::VariadicOp: Ignorable,
    {
        self.convert(niladic, monadic, dyadic, &mut |x, _| x.ignore())
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
        variadic: &mut impl FnMut(&K::VariadicOp, &Vec<Expression<ToKind>>) -> ToKind::VariadicOp,
    ) -> Expression<ToKind> {
        match self
            .0
            .as_ref()
            .map_inner(|x| x.convert(niladic, monadic, dyadic, variadic))
        {
            ExpressionHead::Niladic(op) => Expression::new_niladic(niladic(op)),
            ExpressionHead::Monadic(Monadic { op, inner }) => {
                Expression::new_monadic(monadic(op, &inner), inner)
            }
            ExpressionHead::Dyadic(Dyadic {
                op,
                inner: [inner0, inner1],
            }) => Expression::new_dyadic(dyadic(op, &inner0, &inner1), [inner0, inner1]),
            ExpressionHead::Variadic(v) => {
                Expression::new_variadic(variadic(v.op, &v.inner), v.inner)
            }
        }
    }
    pub fn convert_niladic<
        ToKind: ExpressionKind<
            MonadicOp = K::MonadicOp,
            DyadicOp = K::DyadicOp,
            VariadicOp = K::VariadicOp,
        >,
        E,
    >(
        &self,
        niladic: &mut impl FnMut(&K::NiladicOp) -> Result<ToKind::NiladicOp, E>,
    ) -> Result<Expression<ToKind>, E> {
        match self
            .0
            .as_ref()
            .try_map_inner(|x| x.convert_niladic(niladic))?
        {
            ExpressionHead::Niladic(op) => Ok(Expression::new_niladic(niladic(op)?)),
            ExpressionHead::Monadic(Monadic { op, inner }) => {
                Ok(Expression::new_monadic(op.clone(), inner))
            }
            ExpressionHead::Dyadic(Dyadic { op, inner }) => {
                Ok(Expression::new_dyadic(op.clone(), inner))
            }
            ExpressionHead::Variadic(Variadic { op, inner }) => {
                Ok(Expression::new_variadic(op.clone(), inner))
            }
        }
    }
}

impl<K: ExpressionKind> From<MonadicExpr<K>> for Expression<K> {
    fn from(monadic: MonadicExpr<K>) -> Self {
        Self(ExpressionHead::Monadic(monadic))
    }
}

impl<K: ExpressionKind, T> From<Monadic<K::MonadicOp, T>> for ExpressionHead<K, T> {
    fn from(monadic: Monadic<K::MonadicOp, T>) -> Self {
        ExpressionHead::Monadic(monadic)
    }
}

impl<K: ExpressionKind> From<DyadicExpr<K>> for Expression<K> {
    fn from(dyadic: DyadicExpr<K>) -> Self {
        Self(ExpressionHead::Dyadic(dyadic))
    }
}

impl<K: ExpressionKind, T> From<Dyadic<K::DyadicOp, T>> for ExpressionHead<K, T> {
    fn from(dyadic: Dyadic<K::DyadicOp, T>) -> Self {
        ExpressionHead::Dyadic(dyadic)
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct OpWithData<Op, T> {
    pub data: T,
    pub inner: Op,
}

impl<Op, T> OpWithData<Op, T> {
    pub fn new(data: T, inner: Op) -> Self {
        Self { data, inner }
    }
    pub fn map_data<NewT, F: FnOnce(T) -> NewT>(self, f: F) -> OpWithData<Op, NewT> {
        OpWithData {
            data: f(self.data),
            inner: self.inner,
        }
    }
    pub fn map_data_ref<NewT, F: FnOnce(&T) -> NewT>(&self, f: F) -> OpWithData<Op, NewT>
    where
        Op: Clone,
    {
        OpWithData {
            data: f(&self.data),
            inner: self.inner.clone(),
        }
    }
    pub fn map_inner<NewOp, F: FnOnce(Op) -> NewOp>(self, f: F) -> OpWithData<NewOp, T> {
        OpWithData {
            data: self.data,
            inner: f(self.inner),
        }
    }
}

impl<Op, T, E> OpWithData<Op, Result<T, E>> {
    pub fn transpose(self) -> Result<OpWithData<Op, T>, E> {
        Ok(OpWithData {
            data: self.data?,
            inner: self.inner,
        })
    }
}

impl<K: ExpressionKind, T: Clone + Hash + Eq + Debug, I> ExpressionHead<KindWithData<K, T>, I> {
    pub fn map_data<NewT, F: FnOnce(T) -> NewT>(
        self,
        f: F,
    ) -> ExpressionHead<KindWithData<K, NewT>, I>
    where
        K: ExpressionKind,
        NewT: Clone + Hash + Eq + Debug,
    {
        match self {
            ExpressionHead::Niladic(op) => ExpressionHead::Niladic(op.map_data(f)),
            ExpressionHead::Monadic(Monadic { op, inner }) => {
                ExpressionHead::new_monadic(op.map_data(f), inner)
            }
            ExpressionHead::Dyadic(Dyadic { op, inner }) => {
                ExpressionHead::new_dyadic(op.map_data(f), inner)
            }
            ExpressionHead::Variadic(Variadic { op, inner }) => {
                ExpressionHead::new_variadic(op.map_data(f), inner)
            }
        }
    }
}

impl<K: ExpressionKind, T: Clone + Hash + Eq + Debug, I> ExpressionHead<&KindWithData<K, T>, I> {
    pub fn map_data_ref<NewT, F: FnOnce(&T) -> NewT>(
        self,
        f: F,
    ) -> ExpressionHead<KindWithData<K, NewT>, I>
    where
        K: ExpressionKind,
        NewT: Clone + Hash + Eq + Debug,
    {
        match self {
            ExpressionHead::Niladic(op) => ExpressionHead::Niladic(op.map_data_ref(f)),
            ExpressionHead::Monadic(Monadic { op, inner }) => {
                ExpressionHead::new_monadic(op.map_data_ref(f), inner)
            }
            ExpressionHead::Dyadic(Dyadic { op, inner }) => {
                ExpressionHead::new_dyadic(op.map_data_ref(f), inner)
            }
            ExpressionHead::Variadic(Variadic { op, inner }) => {
                ExpressionHead::new_variadic(op.map_data_ref(f), inner)
            }
        }
    }
    pub fn try_map_data_ref<NewT, E, F: FnOnce(&T) -> Result<NewT, E>>(
        self,
        f: F,
    ) -> Result<ExpressionHead<KindWithData<K, NewT>, I>, E>
    where
        K: ExpressionKind,
        NewT: Clone + Hash + Eq + Debug,
    {
        Ok(match self {
            ExpressionHead::Niladic(op) => ExpressionHead::Niladic(op.map_data_ref(f).transpose()?),
            ExpressionHead::Monadic(Monadic { op, inner }) => {
                ExpressionHead::new_monadic(op.map_data_ref(f).transpose()?, inner)
            }
            ExpressionHead::Dyadic(Dyadic { op, inner }) => {
                ExpressionHead::new_dyadic(op.map_data_ref(f).transpose()?, inner)
            }
            ExpressionHead::Variadic(Variadic { op, inner }) => {
                ExpressionHead::new_variadic(op.map_data_ref(f).transpose()?, inner)
            }
        })
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct KindWithData<K: ExpressionKind, T>(PhantomData<(T, K)>);

impl<T: Clone + Hash + Eq + Debug, K: ExpressionKind> ExpressionKind for KindWithData<K, T> {
    type NiladicOp = OpWithData<K::NiladicOp, T>;
    type MonadicOp = OpWithData<K::MonadicOp, T>;
    type DyadicOp = OpWithData<K::DyadicOp, T>;
    type VariadicOp = OpWithData<K::VariadicOp, T>;
}

impl<K: ExpressionKind, T: Clone + Hash + Eq + Debug, Inner>
    ExpressionHead<KindWithData<K, T>, Inner>
{
    pub fn root_data(&self) -> &T {
        match self {
            ExpressionHead::Niladic(op) => &op.data,
            ExpressionHead::Monadic(head) => &head.op.data,
            ExpressionHead::Dyadic(head) => &head.op.data,
            ExpressionHead::Variadic(head) => &head.op.data,
        }
    }
}

impl<K: ExpressionKind, T: Clone + Hash + Eq + Debug, Inner>
    ExpressionHead<&KindWithData<K, T>, Inner>
{
    pub fn root_data(&self) -> &T {
        match self {
            ExpressionHead::Niladic(op) => &op.data,
            ExpressionHead::Monadic(head) => &head.op.data,
            ExpressionHead::Dyadic(head) => &head.op.data,
            ExpressionHead::Variadic(head) => &head.op.data,
        }
    }
}

impl<T: Clone + Hash + Eq + Debug, K: ExpressionKind> Expression<KindWithData<K, T>> {
    pub fn map<ToT: Clone + Hash + Eq + Debug>(
        &self,
        f: &impl Fn(&T) -> ToT,
    ) -> Expression<KindWithData<K, ToT>> {
        let inner = self
            .0
            .as_ref()
            .map_inner(|inner| Box::new(inner.map(f)))
            .map_data_ref(f);
        Expression(inner)
    }

    pub fn map_mut<ToT: Clone + Hash + Eq + Debug>(
        &self,
        f: &mut impl FnMut(&T) -> ToT,
    ) -> Expression<KindWithData<K, ToT>> {
        let inner = self
            .0
            .as_ref()
            .map_inner(|inner| Box::new(inner.map_mut(f)))
            .map_data_ref(f);
        Expression(inner)
    }

    pub fn try_map<ToT: Clone + Hash + Eq + Debug, E>(
        &self,
        f: &mut impl FnMut(&T) -> Result<ToT, E>,
    ) -> Result<Expression<KindWithData<K, ToT>>, E> {
        Ok(Expression(
            self.0
                .as_ref()
                .try_map_inner(|inner| Ok(Box::new(inner.try_map(f)?)))?
                .try_map_data_ref(f)?,
        ))
    }

    pub fn scan<ToT: Clone + Hash + Eq + Debug>(
        &self,
        f: &mut impl FnMut(ExpressionHead<&KindWithData<K, T>, &ToT>) -> ToT,
    ) -> Expression<KindWithData<K, ToT>> {
        let inner_scanned = self.0.as_ref().map_inner(|inner| Box::new(inner.scan(f)));
        let new_data = f(inner_scanned
            .inner_as_ref()
            .map_inner(|head| head.0.root_data()));
        Expression(inner_scanned.map_data_ref(|_| new_data))
    }

    pub fn try_scan<ToT: Clone + Hash + Eq + Debug, E>(
        &self,
        f: &mut impl FnMut(ExpressionHead<&KindWithData<K, T>, &ToT>) -> Result<ToT, E>,
    ) -> Result<Expression<KindWithData<K, ToT>>, E> {
        let inner_scanned = self
            .0
            .as_ref()
            .try_map_inner(|inner| Ok(Box::new(inner.try_scan(f)?)))?;
        let new_data = f(inner_scanned
            .inner_as_ref()
            .map_inner(|head| head.0.root_data()))?;
        Ok(Expression(inner_scanned.map_data_ref(|_| new_data)))
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
    Number(i64),
    Char(u32),
    Bool(bool),
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
    Then,
    Index,
    At,
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
            "then" => Then,
            ".[" => Index,
            "at" => At,
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
                ValBinOp::Then => "then",
                ValBinOp::Index => ".[",
                ValBinOp::At => "at",
            }
        )
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum FieldAccessMode {
    Normal,
    Backtrack,
}

impl FieldAccessMode {
    pub fn can_backtrack(self) -> bool {
        matches!(self, FieldAccessMode::Backtrack)
    }
}

impl Display for FieldAccessMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FieldAccessMode::Normal => write!(f, "."),
            FieldAccessMode::Backtrack => write!(f, ".?"),
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum BtMarkKind {
    KeepBt,
    RemoveBt,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ValUnOp<C> {
    Not,
    Neg,
    Array,
    Wiggle(C, WiggleKind),
    Dot(FieldName, FieldAccessMode),
    Size,
    BtMark(BtMarkKind),
}

impl<C> ValUnOp<C> {
    pub fn parse_from_str(s: &str) -> Result<Self, &str> {
        use ValUnOp::*;
        Ok(match s {
            "!" => Not,
            "-" => Neg,
            "[" => Array,
            "sizeof" => Size,
            otherwise => return Err(otherwise),
        })
    }
    pub fn map_expr<D>(&self, f: impl FnOnce(&C) -> D) -> ValUnOp<D> {
        use ValUnOp::*;
        match self {
            Not => Not,
            Neg => Neg,
            Array => Array,
            Wiggle(expr, kind) => Wiggle(f(expr), *kind),
            Dot(atom, acc) => Dot(*atom, *acc),
            Size => Size,
            BtMark(kind) => BtMark(*kind),
        }
    }
    pub fn try_map_expr<D, E>(&self, f: impl FnOnce(&C) -> Result<D, E>) -> Result<ValUnOp<D>, E> {
        use ValUnOp::*;
        Ok(match self {
            Not => Not,
            Neg => Neg,
            Array => Array,
            Wiggle(expr, kind) => Wiggle(f(expr)?, *kind),
            Dot(atom, acc) => Dot(*atom, *acc),
            Size => Size,
            BtMark(kind) => BtMark(*kind),
        })
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(u8)]
pub enum WiggleKind {
    If,
    Try,
}

impl Display for WiggleKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                WiggleKind::If => "if",
                WiggleKind::Try => "try",
            }
        )
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ValVarOp {
    Call,
    PartialApply,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum TypeBinOp {
    Ref,
    ParseArg,
}

impl TypeBinOp {
    pub fn parse_from_str(s: &str) -> Result<Self, &str> {
        use TypeBinOp::*;
        Ok(match s {
            "&>" => Ref,
            "*>" => ParseArg,
            otherwise => return Err(otherwise),
        })
    }
}

impl Display for TypeBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TypeBinOp::Ref => "&>",
                TypeBinOp::ParseArg => "*>",
            }
        )
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum TypeUnOp<C> {
    ByteParser,
    Wiggle(C),
}

impl<C> TypeUnOp<C> {
    pub fn parse_from_str(s: &str) -> Result<Self, &str> {
        use TypeUnOp::*;
        Ok(match s {
            "*" => ByteParser,
            otherwise => return Err(otherwise),
        })
    }
    pub fn map_expr<D>(&self, f: impl FnOnce(&C) -> D) -> TypeUnOp<D> {
        use TypeUnOp::*;
        match self {
            Wiggle(expr) => Wiggle(f(expr)),
            ByteParser => ByteParser,
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum TypeVarOp {
    Call,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ConstraintBinOp {
    And,
    Or,
}

impl ConstraintBinOp {
    pub fn parse_from_str(s: &str) -> Result<Self, &str> {
        use ConstraintBinOp::*;
        Ok(match s {
            "and" => And,
            "or" => Or,
            otherwise => return Err(otherwise),
        })
    }
}

impl Display for ConstraintBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ConstraintBinOp::And => "and",
                ConstraintBinOp::Or => "or",
            }
        )
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ConstraintUnOp {
    Not,
    Dot(Atom),
}

impl ConstraintUnOp {
    pub fn parse_from_str(s: &str) -> Result<Self, &str> {
        use ConstraintUnOp::*;
        Ok(match s {
            "!" => Not,
            otherwise => return Err(otherwise),
        })
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Unused {}

pub trait Ignorable {
    fn ignore(&self) -> !;
    fn ignore_as<T>(&self) -> T {
        self.ignore()
    }
}

impl Ignorable for Unused {
    #[inline(always)]
    fn ignore(&self) -> ! {
        match *self {}
    }
}

impl<Op: Ignorable, Data> Ignorable for OpWithData<Op, Data> {
    #[inline(always)]
    fn ignore(&self) -> ! {
        self.inner.ignore()
    }
}

impl<T: Ignorable> Ignorable for &T {
    #[inline(always)]
    fn ignore(&self) -> ! {
        (*self).ignore()
    }
}

impl<T: Ignorable, Inner> Ignorable for Variadic<T, Inner> {
    #[inline(always)]
    fn ignore(&self) -> ! {
        self.op.ignore()
    }
}
