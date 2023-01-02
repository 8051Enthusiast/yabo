mod convert;
pub mod error;
pub mod expr;
pub mod import;
mod represent;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use expr::{Unused, *};

use yaboc_base::{
    error::{SResult, Silencable},
    interner::{FieldName, Identifier, Interner, TypeVar, Regex},
    source::{FieldSpan, FileId, Files, IdSpan, Span, Spanned},
};

use convert::ParseResult;

#[salsa::query_group(AstDatabase)]
pub trait Asts: Files + Interner {
    fn ast(&self, fd: FileId) -> ParseResult<Arc<Module>>;
    fn symbols(&self, fd: FileId) -> SResult<Vec<(Identifier, bool)>>;
    fn imports(&self, fd: FileId) -> SResult<Vec<IdSpan>>;
    fn top_level_statement(&self, fd: FileId, id: Identifier)
        -> SResult<Option<TopLevelStatement>>;
    #[salsa::input]
    fn import_id(&self, fd: FileId, id: Identifier) -> SResult<FileId>;
}

fn ast(db: &dyn Asts, fd: FileId) -> ParseResult<Arc<Module>> {
    convert::parse(db, fd).map(Arc::new)
}

fn symbols(db: &dyn Asts, fd: FileId) -> SResult<Vec<(Identifier, bool)>> {
    let mut syms: Vec<_> = db
        .ast(fd)
        .silence()?
        .tl_statements
        .iter()
        .map(|st| {
            (
                st.name(),
                matches!(st, TopLevelStatement::ParserDefinition(_)),
            )
        })
        .collect();
    syms.sort_unstable();
    Ok(syms)
}

fn imports(db: &dyn Asts, fd: FileId) -> SResult<Vec<IdSpan>> {
    let mut syms: Vec<_> = db
        .ast(fd)
        .silence()?
        .tl_statements
        .iter()
        .filter_map(|st| match st {
            TopLevelStatement::Import(imp) => Some(imp.name.clone()),
            _ => None,
        })
        .collect();
    syms.sort_unstable_by_key(|x| x.inner);
    Ok(syms)
}

fn top_level_statement(
    db: &dyn Asts,
    fd: FileId,
    id: Identifier,
) -> SResult<Option<TopLevelStatement>> {
    Ok(db
        .ast(fd)
        .silence()?
        .tl_statements
        .iter()
        .find(|st| st.name() == id)
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
    type NiladicOp = ConstraintAtom;
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
    type VariadicOp = TypeVarOp;
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

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ConstraintAtom {
    Atom(Atom),
    Range(i64, i64),
}

impl From<Atom> for ConstraintAtom {
    fn from(a: Atom) -> Self {
        ConstraintAtom::Atom(a)
    }
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
    Array,
    Regex(Regex, bool),
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
    pub tl_statements: Vec<TopLevelStatement>,
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
pub enum TopLevelStatement {
    ParserDefinition(Arc<ParserDefinition>),
    Import(Import),
}

impl TopLevelStatement {
    fn name(&self) -> Identifier {
        match self {
            TopLevelStatement::ParserDefinition(x) => x.name.inner,
            TopLevelStatement::Import(i) => i.name.inner,
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Import {
    pub name: IdSpan,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefinition {
    pub qualifier: Option<Qualifier>,
    pub thunky: bool,
    pub name: IdSpan,
    pub from: Option<TypeExpression>,
    pub argdefs: Option<ArgDefList>,
    pub to: ValExpression,
    pub ret_ty: Option<TypeExpression>,
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
    pub name: Vec<IdSpan>,
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
pub struct TypeFunApplication {
    pub result: TypeExpression,
    pub args: Vec<TypeExpression>,
    pub span: Span,
}

impl From<TypeFunApplication> for Variadic<OpWithData<TypeVarOp, Span>, Box<TypeExpression>> {
    fn from(app: TypeFunApplication) -> Self {
        let mut inner = vec![Box::new(app.result)];
        inner.extend(app.args.into_iter().map(Box::new));
        Variadic {
            op: OpWithData {
                data: app.span,
                inner: TypeVarOp::Call,
            },
            inner,
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Range {
    pub start: i64,
    pub end: i64,
    pub span: Span,
}

impl From<Range> for ConstraintAtom {
    fn from(value: Range) -> Self {
        ConstraintAtom::Range(value.start, value.end)
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
    use yaboc_base::{
        config::ConfigDatabase, interner::InternerDatabase, source::FileDatabase, Context,
    };

    use super::*;
    use crate::import::Import;
    #[salsa::database(InternerDatabase, ConfigDatabase, AstDatabase, FileDatabase)]
    #[derive(Default)]
    pub struct AstTestDatabase {
        storage: salsa::Storage<AstTestDatabase>,
    }

    impl salsa::Database for AstTestDatabase {}
    #[test]
    fn nested_choice() {
        let ctx = Context::<AstTestDatabase>::mock(
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
        assert_eq!(ctx.db.symbols(main), Ok(vec![(ctx.id("expr1"), true)]));
    }
}
