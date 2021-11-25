use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use crate::{
    expr::*,
    interner::{Identifier, Interner},
    parse::ParseResult,
    source::{FileId, Files, IdSpan, Span, Spanned},
};

#[salsa::query_group(AstDatabase)]
pub trait Asts: Files + Interner {
    fn ast(&self, fd: FileId) -> ParseResult<Arc<Module>>;
    fn symbols(&self, fd: FileId) -> Result<Vec<Identifier>, ()>;
    fn top_level_statement(&self, fd: FileId, id: Identifier)
        -> Result<Option<Arc<ParserDefinition>>, ()>;
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
        .map(|st| st.name.id)
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
        .find(|st| st.name.id == id)
        .cloned())
}

pub trait AstNode {
    fn span(&self) -> Span;
    fn children(&self) -> Vec<Box<dyn AstNode>>;
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct AstConstraint {}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct AstParse {}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct AstVal;
pub type AstConstraintBinOp = ConstraintBinOp<AstConstraint, Span>;
pub type AstConstraintUnOp = ConstraintUnOp<AstConstraint, Span>;
pub type AstParseBinOp = ParseBinOp<AstParse, AstConstraint, Span>;
pub type AstParseUnOp = ParseUnOp<AstParse, Span>;
pub type AstValBinOp = ValBinOp<AstVal, AstParse, Span>;
pub type AstValUnOp = ValUnOp<AstVal, Span>;

impl ExpressionKind for AstConstraint {
    type BinaryOp = AstConstraintBinOp;
    type UnaryOp = AstConstraintUnOp;
    type Atom = Spanned<Atom>;
}

impl ExpressionKind for AstParse {
    type BinaryOp = AstParseBinOp;
    type UnaryOp = AstParseUnOp;
    type Atom = Spanned<ParserAtom>;
}

impl ExpressionKind for AstVal {
    type BinaryOp = AstValBinOp;
    type UnaryOp = AstValUnOp;
    type Atom = Spanned<Atom>;
}

pub type ParseExpression = Expression<AstParse>;
pub type ValExpression = Expression<AstVal>;
pub type ConstraintExpression = Expression<AstConstraint>;

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
    pub fn id(&self) -> Option<Identifier> {
        match self {
            Statement::ParserDef(x) => Some(x.name.id),
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
    pub from: ParseExpression,
    pub to: ParseExpression,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParseStatement {
    pub name: Option<IdSpan>,
    pub parser: ParseExpression,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct LetStatement {
    pub name: IdSpan,
    pub ty: ParseExpression,
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
pub struct ParserArray {
    pub direction: Spanned<ArrayKind>,
    pub expr: ParseExpression,
    pub span: Span,
}
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum ArrayKind {
    For,
    Each,
    Rof,
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::Context;
    #[test]
    fn nested_choice() {
        let ctx = Context::mock(
            r#"
parser expr1 = for [u8] *> {
    (
        let b: u64 = 3;
    |
	    let _: u64 = 0;
    )
}
        "#,
        );
        let main = FileId::default();
        ctx.db.ast(main).unwrap();
        assert_eq!(ctx.db.symbols(main), Ok(vec![ctx.id("expr1")]));
    }
}
