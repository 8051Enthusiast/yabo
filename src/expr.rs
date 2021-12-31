use crate::interner::Identifier;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::marker::PhantomData;
pub trait ExpressionKind: Clone + Hash + Eq + Debug {
    type BinaryOp: Clone + Hash + Eq + Debug;
    type UnaryOp: Clone + Hash + Eq + Debug;
    type Atom: Clone + Hash + Eq + Debug;
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Expression<K: ExpressionKind> {
    BinaryOp(Box<K::BinaryOp>),
    UnaryOp(Box<K::UnaryOp>),
    Atom(K::Atom),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Atom {
    Id(Identifier),
    Number(String),
    Char(String),
    String(String),
}


#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum TypeBinOp<T: ExpressionKind, C: ExpressionKind, S: Clone + Hash + Eq + Debug> {
    Ref(Expression<T>, Expression<T>, S),
    ParseArg(Expression<T>, Expression<T>, S),
    Wiggle(Expression<T>, Expression<C>, S),
}
impl<T: ExpressionKind, C: ExpressionKind, S: Clone + Hash + Eq + Debug> TypeBinOp<T, C, S> {
    pub fn convert_same<
        ToT: ExpressionKind,
        ToC: ExpressionKind,
        ToS: Clone + Hash + Eq + Debug,
    >(
        &self,
        c: &ExprConverter<T, ToT>,
        c_c: &ExprConverter<C, ToC>,
        mut f: impl FnMut(&S) -> ToS,
    ) -> TypeBinOp<ToT, ToC, ToS> {
        use TypeBinOp::*;
        match self {
            Ref(a, b, t) => Ref(c.convert(a), c.convert(b), f(t)),
            ParseArg(a, b, t) => ParseArg(c.convert(a), c.convert(b), f(t)),
            Wiggle(a, b, t) => Wiggle(c.convert(a), c_c.convert(b), f(t)),
        }
    }
}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum TypeUnOp<T: ExpressionKind, S: Clone + Hash + Eq + Debug> {
    Ref(Expression<T>, S),
}

impl<T: ExpressionKind, S: Clone + Hash + Eq + Debug> TypeUnOp<T, S> {
    pub fn convert_same<ToT: ExpressionKind, ToS: Clone + Hash + Eq + Debug>(
        &self,
        c: &ExprConverter<T, ToT>,
        mut f: impl FnMut(&S) -> ToS,
    ) -> TypeUnOp<ToT, ToS> where {
        use TypeUnOp::*;
        match self {
            Ref(a, t) => Ref(c.convert(a), f(t)),
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ConstraintBinOp<C: ExpressionKind, T: Clone + Hash + Eq + Debug> {
    And(Expression<C>, Expression<C>, T),
    Or(Expression<C>, Expression<C>, T),
    Dot(Expression<C>, Atom, T),
}

impl<C: ExpressionKind, T: Clone + Hash + Eq + Debug> ConstraintBinOp<C, T> {
    pub fn convert_same<ToC: ExpressionKind, ToT: Clone + Hash + Eq + Debug>(
        &self,
        c: &ExprConverter<C, ToC>,
        mut f: impl FnMut(&T) -> ToT,
    ) -> ConstraintBinOp<ToC, ToT> {
        use ConstraintBinOp::*;
        match self {
            And(a, b, t) => And(c.convert(a), c.convert(b), f(t)),
            Or(a, b, t) => Or(c.convert(a), c.convert(b), f(t)),
            Dot(a, b, t) => Dot(c.convert(a), b.clone(), f(t)),
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ConstraintUnOp<C: ExpressionKind, T: Clone + Hash + Eq + Debug> {
    Not(Expression<C>, T),
}

impl<C: ExpressionKind, T: Clone + Hash + Eq + Debug> ConstraintUnOp<C, T> {
    pub fn convert_same<ToC: ExpressionKind, ToT: Clone + Hash + Eq + Debug>(
        &self,
        c: &ExprConverter<C, ToC>,
        mut f: impl FnMut(&T) -> ToT,
    ) -> ConstraintUnOp<ToC, ToT> where {
        use ConstraintUnOp::*;
        match self {
            Not(a, t) => Not(c.convert(a), f(t)),
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ValBinOp<V: ExpressionKind, C: ExpressionKind, T: Clone + Hash + Eq + Debug> {
    Basic(Expression<V>, BasicValBinOp, Expression<V>, T),
    Wiggle(Expression<V>, Expression<C>, T),
    Else(Expression<V>, Expression<V>, T),
    Dot(Expression<V>, Atom, T),
}

impl<V: ExpressionKind, C: ExpressionKind, T: Clone + Hash + Eq + Debug> ValBinOp<V, C, T> {
    pub fn convert_same<
        ToV: ExpressionKind,
        ToC: ExpressionKind,
        ToT: Clone + Hash + Eq + Debug,
    >(
        &self,
        c: &ExprConverter<V, ToV>,
        c_c: &ExprConverter<C, ToC>,
        mut f: impl FnMut(&T) -> ToT,
    ) -> ValBinOp<ToV, ToC, ToT> {
        use ValBinOp::*;
        match self {
            Basic(a, op, b, t) => Basic(c.convert(a), *op, c.convert(b), f(t)),
            Wiggle(a, b, t) => Wiggle(c.convert(a), c_c.convert(b), f(t)),
            Else(a, b, t) => Else(c.convert(a), c.convert(b), f(t)),
            Dot(a, b, t) => Dot(c.convert(a), b.clone(), f(t)),
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum BasicValBinOp {
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
}

impl BasicValBinOp {
    pub fn parse_from_str(s: &str) -> Result<Self, &str> {
        use BasicValBinOp::*;
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
            otherwise => return Err(otherwise),
        })
    }
}

impl Display for BasicValBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BasicValBinOp::And => "&",
                BasicValBinOp::Xor => "^",
                BasicValBinOp::Or => "|",
                BasicValBinOp::LesserEq => "<=",
                BasicValBinOp::Lesser => "<",
                BasicValBinOp::GreaterEq => ">=",
                BasicValBinOp::Greater => ">",
                BasicValBinOp::Uneq => "!=",
                BasicValBinOp::Equals => "==",
                BasicValBinOp::ShiftR => ">>",
                BasicValBinOp::ShiftL => "<<",
                BasicValBinOp::Minus => "-",
                BasicValBinOp::Plus => "+",
                BasicValBinOp::Div => "/",
                BasicValBinOp::Modulo => "%",
                BasicValBinOp::Mul => "*",
                BasicValBinOp::Compose => "|>",
                BasicValBinOp::ParserApply => "*>",
            }
        )
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ValUnOp<V: ExpressionKind, T: Clone + Hash + Eq + Debug> {
    Not(Expression<V>, T),
    Neg(Expression<V>, T),
    Pos(Expression<V>, T),
    If(Expression<V>, T),
}

impl<P: ExpressionKind, T: Clone + Hash + Eq + Debug> ValUnOp<P, T> {
    pub fn convert_same<ToP: ExpressionKind, ToT: Clone + Hash + Eq + Debug>(
        &self,
        c: &ExprConverter<P, ToP>,
        mut f: impl FnMut(&T) -> ToT,
    ) -> ValUnOp<ToP, ToT> {
        use ValUnOp::*;
        match self {
            Not(a, t) => Not(c.convert(a), f(t)),
            Neg(a, t) => Neg(c.convert(a), f(t)),
            Pos(a, t) => Pos(c.convert(a), f(t)),
            If(a, t) => If(c.convert(a), f(t)),
        }
    }
}

pub struct ExprConverter<'a, FromKind: ExpressionKind, ToKind: ExpressionKind> {
    bin_fun: Box<dyn 'a + Fn(&FromKind::BinaryOp, &Self) -> ToKind::BinaryOp>,
    un_fun: Box<dyn 'a + Fn(&FromKind::UnaryOp, &Self) -> ToKind::UnaryOp>,
    atom_fun: Box<dyn 'a + Fn(&FromKind::Atom, &Self) -> ToKind::Atom>,
    _from: PhantomData<FromKind>,
    _to: PhantomData<ToKind>,
}

impl<'a, FromKind: ExpressionKind, ToKind: ExpressionKind> ExprConverter<'a, FromKind, ToKind> {
    pub fn new(
        bin_fun: impl 'a + Fn(&FromKind::BinaryOp, &Self) -> ToKind::BinaryOp,
        un_fun: impl 'a + Fn(&FromKind::UnaryOp, &Self) -> ToKind::UnaryOp,
        atom_fun: impl 'a + Fn(&FromKind::Atom, &Self) -> ToKind::Atom,
    ) -> Self {
        Self {
            bin_fun: Box::new(bin_fun),
            un_fun: Box::new(un_fun),
            atom_fun: Box::new(atom_fun),
            _from: PhantomData,
            _to: PhantomData,
        }
    }
    pub fn convert(&self, expr: &Expression<FromKind>) -> Expression<ToKind> {
        match expr {
            Expression::BinaryOp(b) => Expression::BinaryOp(Box::new((self.bin_fun)(&b, self))),
            Expression::UnaryOp(u) => Expression::UnaryOp(Box::new((self.un_fun)(&u, self))),
            Expression::Atom(a) => Expression::Atom((self.atom_fun)(&a, self)),
        }
    }
}
