use std::borrow::Cow;
use std::iter::FromIterator;
use std::ops::{BitAnd, BitOr};
use std::{collections::BTreeSet, hash::Hash};

use crate::{interner::Identifier, source::FileId};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Int,
    Char,
    For(Box<Type>),
    Each(Box<Type>),
    Constrained(Box<Type>, Constraint),
    Parser(Box<Type>, Box<Type>),
    Named(FileId, Identifier),
    Struct(Struct),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Parser {
    from: Box<Type>,
    to: Box<Type>,
}

#[derive(Clone, Debug)]
pub enum Struct {
    Choice {
        info: StructInfo,
        choices: Vec<Box<Struct>>,
    },
    Sequence {
        info: StructInfo,
        seq: Vec<Box<Struct>>,
    },
    Field {
        id: Option<Identifier>,
        ty: Box<Type>,
    },
}

impl Hash for Struct {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Struct::Choice { choices, .. } => choices.hash(state),
            Struct::Sequence { seq, .. } => seq.hash(state),
            Struct::Field { id, ty } => {
                id.hash(state);
                ty.hash(state);
            }
        }
    }
}

impl PartialEq for Struct {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Choice {
                    choices: l_choices, ..
                },
                Self::Choice {
                    choices: r_choices, ..
                },
            ) => l_choices == r_choices,
            (Self::Sequence { seq: l_seq, .. }, Self::Sequence { seq: r_seq, .. }) => {
                l_seq == r_seq
            }
            (Self::Field { id: l_id, ty: l_ty }, Self::Field { id: r_id, ty: r_ty }) => {
                l_id == r_id && l_ty == r_ty
            }
            _ => false,
        }
    }
}

impl Eq for Struct {}

impl Struct {
    pub fn sequence(self, other: Self) -> Result<Self, Vec<Identifier>> {
        // check for disjoint
        let seq: Vec<_> = (&*self.existent_id() & &*other.existent_id())
            .into_iter()
            .collect();
        if seq.len() > 0 {
            return Err(seq);
        }
        let info = StructInfo {
            present: &*self.present_id() | &*other.present_id(),
            existent: &*self.existent_id() | &*other.existent_id(),
        };
        Ok(match (self, other) {
            (Struct::Sequence { mut seq, .. }, Struct::Sequence { seq: mut seq_r, .. }) => {
                seq.append(&mut seq_r);
                Struct::Sequence { seq, info }
            }
            (Struct::Sequence { mut seq, .. }, second @ _) => {
                seq.push(Box::new(second));
                Struct::Sequence { seq, info }
            }
            (first @ _, Struct::Sequence { mut seq, .. }) => {
                seq.insert(0, Box::new(first));
                Struct::Sequence { seq, info }
            }
            (first @ _, second @ _) => Struct::Sequence {
                seq: vec![Box::new(first), Box::new(second)],
                info,
            },
        })
    }
    pub fn choice(self, other: Self) -> Self {
        let info = StructInfo {
            present: &*self.present_id() & &*other.present_id(),
            existent: &*self.existent_id() | &*other.existent_id(),
        };
        match (self, other) {
            (
                Struct::Choice { mut choices, .. },
                Struct::Choice {
                    choices: mut cho_r, ..
                },
            ) => {
                choices.append(&mut cho_r);
                Struct::Choice { choices, info }
            }
            (Struct::Choice { mut choices, .. }, second @ _) => {
                choices.push(Box::new(second));
                Struct::Choice { choices, info }
            }
            (first @ _, Struct::Choice { mut choices, .. }) => {
                choices.insert(0, Box::new(first));
                Struct::Choice { choices, info }
            }
            (first @ _, second @ _) => Struct::Choice {
                choices: vec![Box::new(first), Box::new(second)],
                info,
            },
        }
    }
    pub fn existent_id(&self) -> Cow<BTreeSet<Identifier>> {
        match self {
            Struct::Choice { info, .. } => Cow::Borrowed(&info.existent),
            Struct::Sequence { info, .. } => Cow::Borrowed(&info.existent),
            Struct::Field { id, .. } => Cow::Owned(BTreeSet::from_iter(id.into_iter().copied())),
        }
    }
    pub fn present_id(&self) -> Cow<BTreeSet<Identifier>> {
        match self {
            Struct::Choice { info, .. } => Cow::Borrowed(&info.present),
            Struct::Sequence { info, .. } => Cow::Borrowed(&info.present),
            Struct::Field { id, .. } => Cow::Owned(BTreeSet::from_iter(id.into_iter().copied())),
        }
    }
}

impl BitOr for Struct {
    type Output = Struct;

    fn bitor(self, rhs: Self) -> Self::Output {
        self.choice(rhs)
    }
}

impl BitAnd for Struct {
    type Output = Result<Struct, Vec<Identifier>>;

    fn bitand(self, rhs: Self) -> Self::Output {
        self.sequence(rhs)
    }
}

#[derive(Clone, Debug)]
pub struct StructInfo {
    existent: BTreeSet<Identifier>,
    present: BTreeSet<Identifier>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Constraint(Vec<ConstraintConjunction>);
impl Constraint {
    pub fn project(&self, id: Identifier) -> Self {
        Constraint(self.0.iter().filter_map(|x| x.project(id)).collect())
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConstraintConjunction(Vec<ConstraintChain>);
impl ConstraintConjunction {
    pub fn project(&self, id: Identifier) -> Option<Self> {
        let ret: Vec<_> = self.0.iter().filter_map(|x| x.project(id)).collect();
        if ret.is_empty() {
            None
        } else {
            Some(ConstraintConjunction(ret))
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConstraintChain {
    Id(Identifier, Option<Box<ConstraintChain>>),
    Number(String),
    NumberRange(String),
    Char(String),
    String(String),
}

impl ConstraintChain {
    pub fn project(&self, id: Identifier) -> Option<Self> {
        match self {
            ConstraintChain::Id(ident, Some(inner)) if ident == &id => Some(*inner.clone()),
            _ => None,
        }
    }
}
