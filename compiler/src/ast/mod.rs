mod convert;
pub mod error;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use crate::{
    error::{SResult, Silencable},
    expr::{Unused, *},
    interner::{FieldName, Identifier, Interner, TypeVar},
    source::{FieldSpan, FileId, Files, IdSpan, Span, Spanned},
};

use convert::ParseResult;

#[salsa::query_group(AstDatabase)]
pub trait Asts: Files + Interner {
    fn ast(&self, fd: FileId) -> ParseResult<Arc<Module>>;
    fn symbols(&self, fd: FileId) -> SResult<Vec<Identifier>>;
    fn top_level_statement(
        &self,
        fd: FileId,
        id: Identifier,
    ) -> SResult<Option<Arc<ParserDefinition>>>;
}

fn ast(db: &dyn Asts, fd: FileId) -> ParseResult<Arc<Module>> {
    convert::parse(db, fd).map(Arc::new)
}

fn symbols(db: &dyn Asts, fd: FileId) -> SResult<Vec<Identifier>> {
    let mut syms: Vec<_> = db
        .ast(fd)
        .silence()?
        .tl_statements
        .iter()
        .map(|st| st.name.inner)
        .collect();
    syms.sort();
    Ok(syms)
}

fn top_level_statement(
    db: &dyn Asts,
    fd: FileId,
    id: Identifier,
) -> SResult<Option<Arc<ParserDefinition>>> {
    Ok(db
        .ast(fd)
        .silence()?
        .tl_statements
        .iter()
        .find(|st| st.name.inner == id)
        .cloned())
}

pub trait AstNode {
    fn span(&self) -> Span;
    fn children(&self) -> Vec<Box<dyn AstNode>>;
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct AstConstraint;
pub type AstConstraintSpanned = KindWithData<AstConstraint, Span>;
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct AstVal;
pub type AstValSpanned = KindWithData<AstVal, Span>;

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct AstType;
pub type AstTypeSpanned = KindWithData<AstType, Span>;

pub type AstConstraintBinOp = OpWithData<ConstraintBinOp, Span>;
pub type AstConstraintUnOp = OpWithData<ConstraintUnOp, Span>;
pub type AstValBinOp = OpWithData<ValBinOp, Span>;
pub type AstValUnOp = OpWithData<ValUnOp<Arc<ConstraintExpression>>, Span>;
pub type AstTypeBinOp = OpWithData<TypeBinOp, Span>;
pub type AstTypeUnOp = OpWithData<TypeUnOp<Arc<ConstraintExpression>>, Span>;

impl ExpressionKind for AstConstraint {
    type NiladicOp = Atom;
    type MonadicOp = ConstraintUnOp;
    type DyadicOp = ConstraintBinOp;
    type VariadicOp = Unused;
}

impl ExpressionKind for AstVal {
    type NiladicOp = ParserAtom;
    type MonadicOp = ValUnOp<Arc<ConstraintExpression>>;
    type DyadicOp = ValBinOp;
    type VariadicOp = ValVarOp;
}

impl ExpressionKind for AstType {
    type NiladicOp = TypeAtom;
    type MonadicOp = TypeUnOp<Arc<ConstraintExpression>>;
    type DyadicOp = TypeBinOp;
    type VariadicOp = Unused;
}

pub type TypeExpression = Expression<AstTypeSpanned>;
pub type ValExpression = Expression<AstValSpanned>;
pub type ConstraintExpression = Expression<AstConstraintSpanned>;

pub type TypeExpressionInner = ExpressionHead<AstTypeSpanned, Box<TypeExpression>>;
pub type ValExpressionInner = ExpressionHead<AstValSpanned, Box<ValExpression>>;
pub type ConstraintExpressionInner =
    ExpressionHead<AstConstraintSpanned, Box<ConstraintExpression>>;

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum TypeAtom {
    ParserDef(Box<ParserDefRef>),
    Primitive(TypePrimitive),
    Array(Box<TypeArray>),
    TypeVar(TypeVar),
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum TypePrimitive {
    Mem,
    Int,
    Bit,
    Char,
}

impl From<ParserDefRef> for TypeAtom {
    fn from(pd: ParserDefRef) -> Self {
        TypeAtom::ParserDef(Box::new(pd))
    }
}

impl From<TypeArray> for TypeAtom {
    fn from(arr: TypeArray) -> Self {
        TypeAtom::Array(Box::new(arr))
    }
}

impl From<TypePrimitive> for TypeAtom {
    fn from(p: TypePrimitive) -> Self {
        TypeAtom::Primitive(p)
    }
}

impl From<TypeVar> for TypeAtom {
    fn from(v: TypeVar) -> Self {
        TypeAtom::TypeVar(v)
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ParserAtom {
    Atom(Atom),
    Single,
    Nil,
    Block(Block),
}

impl From<Atom> for ParserAtom {
    fn from(atom: Atom) -> Self {
        ParserAtom::Atom(atom)
    }
}

impl From<Block> for ParserAtom {
    fn from(block: Block) -> Self {
        ParserAtom::Block(block)
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Module {
    pub tl_statements: Vec<Arc<ParserDefinition>>,
    pub 亘: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Statement {
    Parse(Box<ParseStatement>),
    Let(Box<LetStatement>),
}

impl Statement {
    pub fn field(&self) -> Option<FieldName> {
        match self {
            Statement::Parse(x) => x.name.as_ref().map(|i| i.inner),
            Statement::Let(x) => Some(x.name.inner),
        }
    }
    pub fn span(&self) -> Span {
        match self {
            Statement::Parse(x) => x.span,
            Statement::Let(x) => x.span,
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ArgDefinition {
    pub name: IdSpan,
    pub ty: TypeExpression,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ArgDefList {
    pub args: Vec<ArgDefinition>,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefinition {
    pub qualifier: Option<Qualifier>,
    pub name: IdSpan,
    pub from: TypeExpression,
    pub argdefs: Option<ArgDefList>,
    pub to: ValExpression,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParseStatement {
    pub name: Option<FieldSpan>,
    pub parser: ValExpression,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct LetStatement {
    pub name: FieldSpan,
    pub ty: Option<TypeExpression>,
    pub expr: ValExpression,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Block {
    pub content: Option<ParserSequence>,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ParserSequenceElement {
    Statement(Box<Statement>),
    Choice(Box<ParserChoice>),
}

impl ParserSequenceElement {
    pub fn span(&self) -> Span {
        match self {
            ParserSequenceElement::Statement(x) => x.span(),
            ParserSequenceElement::Choice(x) => x.span,
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserSequence {
    pub content: Vec<ParserSequenceElement>,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserChoice {
    pub content: Vec<ParserSequence>,
    pub span: Span,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub enum Qualifier {
    Export,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefRef {
    pub from: Option<TypeExpression>,
    pub name: IdSpan,
    pub args: Vec<TypeExpression>,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct FunApplication {
    pub applicant: ValExpression,
    pub args: Vec<ValExpression>,
    pub span: Span,
}

impl From<FunApplication> for Variadic<OpWithData<ValVarOp, Span>, Box<ValExpression>> {
    fn from(app: FunApplication) -> Self {
        let mut inner = vec![Box::new(app.applicant)];
        inner.extend(app.args.into_iter().map(Box::new));
        Variadic {
            op: OpWithData {
                data: app.span,
                inner: ValVarOp::Call,
            },
            inner,
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct TypeArray {
    pub direction: Spanned<ArrayKind>,
    pub expr: TypeExpression,
    pub span: Span,
}
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum ArrayKind {
    For,
    Each,
}

impl PartialOrd for ArrayKind {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ArrayKind {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (ArrayKind::For, ArrayKind::Each) => std::cmp::Ordering::Greater,
            (ArrayKind::Each, ArrayKind::For) => std::cmp::Ordering::Less,
            (ArrayKind::For, ArrayKind::For) | (ArrayKind::Each, ArrayKind::Each) => {
                std::cmp::Ordering::Equal
            }
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::Context;
    #[test]
    fn nested_choice() {
        let ctx = Context::mock(
            r#"
def for[u8] *> expr1 = {
  x: u64
  | let b: u64 = 3
  | | let a: u64 = 0
    | let z: u64 = 1
}
        "#,
        );
        let main = FileId::default();
        ctx.db.ast(main).unwrap();
        assert_eq!(ctx.db.symbols(main), Ok(vec![ctx.id("expr1")]));
    }
}
