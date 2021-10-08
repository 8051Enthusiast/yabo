use std::hash::Hash;
use std::sync::Arc;

use crate::{
    interner::{Identifier, Interner},
    parse::ParseResult,
    source::{FileId, Files, Span},
};

#[salsa::query_group(AstDatabase)]
pub trait Asts: Files + Interner {
    fn ast(&self, fd: FileId) -> ParseResult<Arc<Module>>;
    fn symbols(&self, fd: FileId) -> Result<Vec<Identifier>, ()>;
    fn top_level_statement(&self, fd: FileId, id: Identifier)
        -> Result<Option<Arc<Statement>>, ()>;
}

fn ast(db: &dyn Asts, fd: FileId) -> ParseResult<Arc<Module>> {
    crate::parse::parse(db, fd).map(Arc::new)
}

fn symbols(db: &dyn Asts, fd: FileId) -> Result<Vec<Identifier>, ()> {
    Ok(
        db.ast(fd)
            .map_err(|_| ())?
            .tl_statements
            .iter()
            .filter_map(|st| st.id())
            .collect(),
    )
}

fn top_level_statement(
    db: &dyn Asts,
    fd: FileId,
    id: Identifier,
) -> Result<Option<Arc<Statement>>, ()> {
    Ok(db
        .ast(fd)
        .map_err(|_| ())?
        .tl_statements
        .iter()
        .find(|st| st.id() == Some(id))
        .cloned())
}

pub trait AstNode {
    fn span(&self) -> Span;
    fn children(&self) -> Vec<Box<dyn AstNode>>;
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct IdSpan {
    pub id: Identifier,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Spanned<T: Clone + Hash + PartialEq + Eq> {
    pub inner: T,
    pub span: Span,
}

pub trait ExpressionKind {
    type BinaryOp: Clone + Hash + PartialEq + Eq;
    type UnaryOp: Clone + Hash + PartialEq + Eq;
    type Atom: Clone + Hash + PartialEq + Eq + From<Atom>;
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Constraint {}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Parse {}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Val {}

impl ExpressionKind for Constraint {
    type BinaryOp = ConstraintBinOp;
    type UnaryOp = ConstraintUnOp;
    type Atom = Atom;
}

impl ExpressionKind for Parse {
    type BinaryOp = ParseBinOp;
    type UnaryOp = ParseUnOp;
    type Atom = ParserAtom;
}

impl ExpressionKind for Val {
    type BinaryOp = ValBinOp;
    type UnaryOp = ValUnOp;
    type Atom = Atom;
}

pub type ParseExpression = Expression<Parse>;
pub type ValExpression = Expression<Val>;
pub type ConstraintExpression = Expression<Constraint>;

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Expression<K: ExpressionKind> {
    BinaryOp(Box<Spanned<K::BinaryOp>>),
    UnaryOp(Box<Spanned<K::UnaryOp>>),
    Atom(Spanned<K::Atom>),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Atom {
    Id(IdSpan),
    Number(String),
    Char(String),
    String(String),
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
pub enum ConstraintBinOp {
    And(Expression<Constraint>, Expression<Constraint>),
    Or(Expression<Constraint>, Expression<Constraint>),
    Dot(Expression<Constraint>, Atom),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ConstraintUnOp {
    Not(Expression<Constraint>),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ParseBinOp {
    Pipe(Expression<Parse>, Expression<Parse>),
    Wiggle(Expression<Parse>, Expression<Constraint>),
    Dot(Expression<Parse>, Atom),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ParseUnOp {
    If(Expression<Parse>),
    Try(Expression<Parse>),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ValBinOp {
    And(Expression<Val>, Expression<Val>),
    Xor(Expression<Val>, Expression<Val>),
    Or(Expression<Val>, Expression<Val>),
    LesserEq(Expression<Val>, Expression<Val>),
    Lesser(Expression<Val>, Expression<Val>),
    GreaterEq(Expression<Val>, Expression<Val>),
    Greater(Expression<Val>, Expression<Val>),
    Uneq(Expression<Val>, Expression<Val>),
    Equals(Expression<Val>, Expression<Val>),
    ShiftR(Expression<Val>, Expression<Val>),
    ShiftL(Expression<Val>, Expression<Val>),
    Minus(Expression<Val>, Expression<Val>),
    Plus(Expression<Val>, Expression<Val>),
    Div(Expression<Val>, Expression<Val>),
    Modulo(Expression<Val>, Expression<Val>),
    Mul(Expression<Val>, Expression<Val>),
    Pipe(Expression<Val>, Expression<Parse>),
    Else(Expression<Val>, Expression<Val>),
    Dot(Expression<Val>, Atom),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ValUnOp {
    Not(Expression<Val>),
    Neg(Expression<Val>),
    Pos(Expression<Val>),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Module {
    pub tl_statements: Vec<Arc<Statement>>,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Statement {
    ParserDef(Box<ParserDefinition>),
    Parse(Box<ParseStatement>),
    Let(Box<LetStatement>),
}

impl Statement {
    fn id(&self) -> Option<Identifier> {
        match self {
            Statement::ParserDef(x) => Some(x.name.id),
            Statement::Parse(x) => x.name.as_ref().map(|i| i.id),
            Statement::Let(x) => Some(x.name.id),
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefinition {
    pub name: IdSpan,
    pub from: Expression<Parse>,
    pub to: Expression<Parse>,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParseStatement {
    pub name: Option<IdSpan>,
    pub parser: Expression<Parse>,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct LetStatement {
    pub name: IdSpan,
    pub ty: Expression<Parse>,
    pub expr: Expression<Val>,
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
    pub direction: Spanned<ArrayDirection>,
    pub expr: Expression<Parse>,
    pub span: Span,
}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ArrayDirection {
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
