use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use crate::{
    expr::*,
    interner::{FieldName, Identifier, Interner, TypeVar},
    parse::ParseResult,
    source::{FieldSpan, FileId, Files, IdSpan, Span, Spanned},
};

#[salsa::query_group(AstDatabase)]
pub trait Asts: Files + Interner {
    fn ast(&self, fd: FileId) -> ParseResult<Arc<Module>>;
    fn symbols(&self, fd: FileId) -> Result<Vec<Identifier>, ()>;
    fn top_level_statement(
        &self,
        fd: FileId,
        id: Identifier,
    ) -> Result<Option<Arc<ParserDefinition>>, ()>;
}

fn ast(db: &dyn Asts, fd: FileId) -> ParseResult<Arc<Module>> {
    crate::parse::parse(db, fd).map(Arc::new)
}

fn symbols(db: &dyn Asts, fd: FileId) -> Result<Vec<Identifier>, ()> {
    let mut syms: Vec<_> = db
        .ast(fd)
        .map_err(|_| ())?
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
) -> Result<Option<Arc<ParserDefinition>>, ()> {
    Ok(db
        .ast(fd)
        .map_err(|_| ())?
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
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct AstVal;

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct AstType;
pub type AstConstraintBinOp = ConstraintBinOp<AstConstraint, Span>;
pub type AstConstraintUnOp = ConstraintUnOp<AstConstraint, Span>;
pub type AstValBinOp = ValBinOp<AstVal, AstConstraint, Span>;
pub type AstValUnOp = ValUnOp<AstVal, Span>;
pub type AstTypeBinOp = TypeBinOp<AstType, AstConstraint, Span>;
pub type AstTypeUnOp = TypeUnOp<AstType, Span>;

impl ExpressionKind for AstConstraint {
    type BinaryOp = AstConstraintBinOp;
    type UnaryOp = AstConstraintUnOp;
    type Atom = Spanned<Atom>;
}

impl<K: ExpressionKind, T: ExpressionComponent<K>> ExpressionComponent<K> for Spanned<T> {
    fn children(&self) -> Vec<&Expression<K>> {
        self.inner.children()
    }
}

impl ExpressionComponent<AstVal> for ParserAtom {
    fn children(&self) -> Vec<&Expression<AstVal>> {
        match self {
            ParserAtom::Atom(_) | ParserAtom::Block(_) => vec![],
            ParserAtom::Array(a) => vec![&a.expr],
        }
    }
}

impl ExpressionComponent<AstType> for TypeAtom {
    fn children(&self) -> Vec<&Expression<AstType>> {
        match self {
            TypeAtom::Array(a) => vec![&a.expr],
            _ => vec![],
        }
    }
}

impl ExpressionKind for AstVal {
    type BinaryOp = AstValBinOp;
    type UnaryOp = AstValUnOp;
    type Atom = Spanned<ParserAtom>;
}

impl ExpressionKind for AstType {
    type BinaryOp = AstTypeBinOp;
    type UnaryOp = AstTypeUnOp;
    type Atom = Spanned<TypeAtom>;
}

pub type TypeExpression = Expression<AstType>;
pub type ValExpression = Expression<AstVal>;
pub type ConstraintExpression = Expression<AstConstraint>;

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
    Array(Box<ParserArray>),
    Block(Block),
}

impl From<Atom> for ParserAtom {
    fn from(atom: Atom) -> Self {
        ParserAtom::Atom(atom)
    }
}

impl From<ParserArray> for ParserAtom {
    fn from(pa: ParserArray) -> Self {
        ParserAtom::Array(Box::new(pa))
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
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Statement {
    ParserDef(Box<ParserDefinition>),
    Parse(Box<ParseStatement>),
    Let(Box<LetStatement>),
}

impl Statement {
    pub fn field(&self) -> Option<FieldName> {
        match self {
            Statement::ParserDef(x) => Some(FieldName::Ident(x.name.inner)),
            Statement::Parse(x) => x.name.as_ref().map(|i| i.id),
            Statement::Let(x) => Some(x.name.id),
        }
    }
    pub fn span(&self) -> Span {
        match self {
            Statement::ParserDef(x) => x.span,
            Statement::Parse(x) => x.span,
            Statement::Let(x) => x.span,
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefinition {
    pub name: IdSpan,
    pub from: TypeExpression,
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
    pub ty: TypeExpression,
    pub expr: ValExpression,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Block {
    pub content: Option<BlockContent>,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum BlockContent {
    Statement(Box<Statement>),
    Sequence(Box<ParserSequence>),
    Choice(Box<ParserChoice>),
}

impl BlockContent {
    pub fn span(&self) -> Span {
        match self {
            BlockContent::Statement(x) => x.span(),
            BlockContent::Sequence(x) => x.span,
            BlockContent::Choice(x) => x.span,
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserSequence {
    pub content: Vec<BlockContent>,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserChoice {
    pub left: BlockContent,
    pub right: BlockContent,
    pub span: Span,
}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefRef {
    pub from: Option<TypeExpression>,
    pub name: IdSpan,
    pub args: Vec<TypeExpression>,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct TypeArray {
    pub direction: Spanned<ArrayKind>,
    pub expr: TypeExpression,
    pub span: Span,
}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserArray {
    pub direction: Spanned<ArrayKind>,
    pub expr: ValExpression,
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
    (
        let b: u64 = 3,
    |
	    let _: u64 = 0,
    )
}
        "#,
        );
        let main = FileId::default();
        ctx.db.ast(main).unwrap();
        assert_eq!(ctx.db.symbols(main), Ok(vec![ctx.id("expr1")]));
    }
}
