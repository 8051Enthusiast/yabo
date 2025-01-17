use tree_sitter::{Node, Parser, TreeCursor};
use tree_sitter_yabo::language;
use yaboc_base::interner::RegexKind;

use super::*;
use yaboc_base::error::SilencedError;
use yaboc_base::interner::IdentifierName;
use yaboc_base::source::Spanned;

macro_rules! inner_string {
    ($n:ident) => {stringify!($n)};
    ($_:ident ..) => {_};
    ($f:ident $n:tt) => {inner_string!$n};
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParseError {
    Generic(Span),
    InvalidChar(Span),
    InvalidString(Span),
    NumberTooBig(Span),
}

impl From<ParseError> for SilencedError {
    fn from(_val: ParseError) -> Self {
        SilencedError::new()
    }
}

pub type ParseResult<T> = Result<T, Vec<ParseError>>;

fn combine_errors<T: std::fmt::Debug, U: std::fmt::Debug, V, F: FnOnce(T, U) -> V>(
    a: ParseResult<T>,
    b: ParseResult<U>,
    f: F,
) -> ParseResult<V> {
    match (a, b) {
        (Ok(a), Ok(b)) => Ok(f(a, b)),
        (Ok(_), Err(e)) | (Err(e), Ok(_)) => Err(e),
        (Err(mut a), Err(mut b)) => {
            a.append(&mut b);
            Err(a)
        }
    }
}

pub fn parse(db: &dyn Asts, fd: FileId) -> Result<Module, Vec<ParseError>> {
    let mut parser = Parser::new();
    let language = language();
    parser
        .set_language(language)
        .expect("Incompatible language version");
    let tree = parser.parse(db.file_content(fd).as_ref(), None).unwrap();
    let c = tree.walk();
    module(db, fd, c)
}

fn span_from_node(fd: FileId, node: &Node) -> Span {
    Span {
        file: fd,
        lo: node.range().start_byte as u32,
        hi: node.range().end_byte as u32,
    }
}

fn check_error<'a>(_: &dyn Asts, fd: FileId, node: Node<'a>) -> ParseResult<Node<'a>> {
    if node.is_error() {
        let span = span_from_node(fd, &node);
        Err(vec![ParseError::Generic(span)])
    } else {
        Ok(node)
    }
}

const IGNORED_NAMES: &[&str] = &["block_open", "block_close", "comment"];

fn iter_children<'a, F: FnMut(Node<'a>, TreeCursor<'a>) -> ParseResult<()>>(
    db: &dyn Asts,
    fd: FileId,
    mut c: TreeCursor<'a>,
    mut fun: F,
) -> ParseResult<()> {
    let mut ret = Ok(());
    if c.goto_first_child() {
        loop {
            let node = match check_error(db, fd, c.node()) {
                Ok(n) => n,
                e @ Err(_) => {
                    ret = combine_errors(ret, e, |a, _| a);
                    if !c.goto_next_sibling() {
                        break;
                    }
                    continue;
                }
            };
            if IGNORED_NAMES.contains(&node.kind()) {
                if !c.goto_next_sibling() {
                    break;
                }
                continue;
            }
            let res = fun(node, c.clone());
            ret = combine_errors(ret, res, |a, _| a);
            if !c.goto_next_sibling() {
                break;
            }
        }
    }
    ret
}

fn module(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<Module> {
    let node = check_error(db, fd, c.node())?;
    let mut statements = Vec::new();
    iter_children(db, fd, c, |_, cursor| {
        statements.push(top_level_statement(db, fd, cursor)?);
        Ok(())
    })?;
    Ok(Module {
        tl_statements: statements,
        亘: span_from_node(fd, &node),
    })
}

struct OpInfo<'a> {
    left: Option<TreeCursor<'a>>,
    left_parens: bool,
    op: Spanned<String>,
    right: TreeCursor<'a>,
    right_parens: bool,
}

fn get_op<'a>(db: &dyn Asts, fd: FileId, c: TreeCursor<'a>) -> ParseResult<OpInfo<'a>> {
    let mut left = None;
    let mut right = None;
    let mut left_parens = false;
    let mut right_parens = false;
    let mut op = None;
    iter_children(db, fd, c, |_, cursor| {
        let field = cursor.field_name();
        match field {
            Some("left") if cursor.node().is_named() => left = Some(cursor),
            Some("right") if cursor.node().is_named() => right = Some(cursor),
            Some("left") if matches!(cursor.node().kind(), "(" | ")") => left_parens = true,
            Some("right") if matches!(cursor.node().kind(), "(" | ")") => right_parens = true,
            Some("op") => op = Some(cursor),
            Some(other) => panic!("Unknown field {other}"),
            None => (),
        }
        Ok(())
    })?;
    let right = right.unwrap();
    let op = spanned(node_to_string)(db, fd, op.unwrap())?;
    Ok(OpInfo {
        left,
        left_parens,
        op,
        right,
        right_parens,
    })
}

fn binary_type_expression(
    db: &dyn Asts,
    fd: FileId,
    c: TreeCursor,
) -> ParseResult<DyadicExpr<AstTypeSpanned>> {
    dyadic(
        |_, x, _| TypeBinOp::parse_from_str(x).unwrap(),
        expression(type_expression),
    )(db, fd, c)
}

fn unary_type_expression(
    db: &dyn Asts,
    fd: FileId,
    c: TreeCursor,
) -> ParseResult<MonadicExpr<AstTypeSpanned>> {
    monadic(
        |x| TypeUnOp::parse_from_str(x).unwrap(),
        expression(type_expression),
    )(db, fd, c)
}
fn binary_constraint_expression(
    db: &dyn Asts,
    fd: FileId,
    c: TreeCursor,
) -> ParseResult<DyadicExpr<AstConstraintSpanned>> {
    dyadic(
        |_, x, _| ConstraintBinOp::parse_from_str(x).unwrap(),
        expression(constraint_expression),
    )(db, fd, c)
}

fn unary_constraint_expression(
    db: &dyn Asts,
    fd: FileId,
    c: TreeCursor,
) -> ParseResult<MonadicExpr<AstConstraintSpanned>> {
    monadic(
        |x| ConstraintUnOp::parse_from_str(x).unwrap(),
        expression(constraint_expression),
    )(db, fd, c)
}

fn binary_expression(
    db: &dyn Asts,
    fd: FileId,
    c: TreeCursor,
) -> ParseResult<DyadicExpr<AstValSpanned>> {
    dyadic(
        |left, x, right| (left, ValBinOp::parse_from_str(x).unwrap(), right),
        expression(val_expression),
    )(db, fd, c)
}

fn unary_expression(
    db: &dyn Asts,
    fd: FileId,
    c: TreeCursor,
) -> ParseResult<MonadicExpr<AstValSpanned>> {
    monadic(
        |x| ValUnOp::parse_from_str(x).unwrap(),
        expression(val_expression),
    )(db, fd, c)
}

fn dyadic<Kind: ExpressionKind>(
    mut f: impl FnMut(bool, &str, bool) -> Kind::DyadicOp,
    mut sub_expr: impl FnMut(
        &dyn Asts,
        FileId,
        TreeCursor,
    ) -> ParseResult<Expression<KindWithData<Kind, Span>>>,
) -> impl FnMut(&dyn Asts, FileId, TreeCursor) -> ParseResult<DyadicExpr<KindWithData<Kind, Span>>>
{
    move |db, fd, c| {
        let op_info = get_op(db, fd, c)?;
        let left = op_info.left.unwrap();
        let right = op_info.right;
        let span = op_info.op.span;

        Ok(Dyadic {
            op: OpWithData {
                data: span,
                inner: f(op_info.left_parens, &op_info.op.inner, op_info.right_parens),
            },
            inner: [sub_expr(db, fd, left)?, sub_expr(db, fd, right)?].map(Box::new),
        })
    }
}

fn monadic<Kind: ExpressionKind>(
    mut f: impl FnMut(&str) -> Kind::MonadicOp,
    mut sub_expr: impl FnMut(
        &dyn Asts,
        FileId,
        TreeCursor,
    ) -> ParseResult<Expression<KindWithData<Kind, Span>>>,
) -> impl FnOnce(&dyn Asts, FileId, TreeCursor) -> ParseResult<MonadicExpr<KindWithData<Kind, Span>>>
{
    move |db, fd, c| {
        let OpInfo { op, right, .. } = get_op(db, fd, c)?;
        let span = op.span;

        Ok(Monadic {
            op: OpWithData {
                data: span,
                inner: f(&op.inner),
            },
            inner: Box::new(sub_expr(db, fd, right)?),
        })
    }
}

fn maybe_last<T>(mut t: Vec<T>) -> Option<T> {
    t.pop()
}
fn last<T>(mut t: Vec<T>) -> T {
    t.pop().unwrap()
}

macro_rules! maybe_unwrap {
    (?) => {
        maybe_last
    };
    (!) => {
        last
    };
    (*) => {
        std::convert::identity
    };
}

macro_rules! callable {
    ($n:ident) => {into($n)};
    ($n:ident ..) => {into($n)};
    ($f:ident $n:tt) => {$f(callable!$n)};
}

macro_rules! astify {
    {struct $new:ident = $node:ident {$($name:ident: $fun:ident$(($fun_inner:tt))? [$require:tt]),+$(,)?};} => {
        fn $new(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<$node> {
//            eprintln!("Enter {}", stringify!($new));
            let node = c.node();
            $(let mut $name = Vec::new();)+
            iter_children(db, fd, c, |_, cursor| {
                let field = cursor.field_name();
                if cursor.node().kind() == "(" || cursor.node().kind() == ")" {
                    return Ok(());
                }
                match field {
                    $(Some(stringify!($name)) => $name.push($fun$(($fun_inner))?(db, fd, cursor.clone())?)),+,
                    Some(other) => panic!("Unknown field {} in struct {}", other, stringify!($new)),
                    None => (),
                }
                Ok(())
            })?;
//            eprintln!("Leave {}", stringify!($new));
            Ok($node {
                span: span_from_node(fd, &node),
                $($name: maybe_unwrap!($require)($name)),+
            })
        }
    };
    {enum $new:ident = $node:ident {$($name:ident($fun:ident$($args:tt)?)),+$(,)?};} => {
        fn $new(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<$node> {
//            eprintln!("Enter {}", stringify!($new));
            let node = c.node();
            let ret = Ok(match node.kind() {
                $(inner_string!($fun$($args)?) => <$node>::$name(callable!($fun$($args)?)(db, fd, c)?)),+,
                #[allow(unreachable_patterns)]
                otherwise => panic!(
                    "Internal Compiler Error: Unexpected type {} in enum {}",
                    otherwise,
                    stringify!($new),
                ),
            });
//            eprintln!("Leave {}", stringify!($new));
            ret
        }
    };
}

#[inline]
fn boxed<T, F: FnMut(&'_ dyn Asts, FileId, TreeCursor) -> ParseResult<T>>(
    mut f: F,
) -> impl FnMut(&'_ dyn Asts, FileId, TreeCursor) -> ParseResult<Box<T>> {
    #[inline]
    move |db: &dyn Asts, fd: FileId, c: TreeCursor| f(db, fd, c).map(Box::new)
}

#[inline]
fn into<T: Into<U>, U, F: FnMut(&'_ dyn Asts, FileId, TreeCursor) -> ParseResult<T>>(
    mut f: F,
) -> impl FnMut(&'_ dyn Asts, FileId, TreeCursor) -> ParseResult<U> {
    #[inline]
    move |db: &dyn Asts, fd: FileId, c: TreeCursor| f(db, fd, c).map(T::into)
}

#[inline]
fn spanned<T, F>(
    mut f: F,
) -> impl FnMut(&'_ dyn Asts, FileId, TreeCursor) -> ParseResult<Spanned<T>>
where
    T: Clone + core::hash::Hash + Eq,
    F: FnMut(&'_ dyn Asts, FileId, TreeCursor) -> ParseResult<T>,
{
    move |db: &dyn Asts, fd: FileId, c: TreeCursor| {
        let node = c.node();
        f(db, fd, c).map(|o| Spanned {
            inner: o,
            span: span_from_node(fd, &node),
        })
    }
}

#[inline]
fn expression<K: ExpressionKind, F>(
    mut f: F,
) -> impl FnMut(&'_ dyn Asts, FileId, TreeCursor) -> ParseResult<Expression<K>>
where
    F: FnMut(
        &'_ dyn Asts,
        FileId,
        TreeCursor,
    ) -> ParseResult<ExpressionHead<K, Box<Expression<K>>>>,
{
    #[inline]
    move |db: &dyn Asts, fd: FileId, c: TreeCursor| f(db, fd, c).map(Expression)
}

fn with_span_data<T, F>(
    mut f: F,
) -> impl FnMut(&'_ dyn Asts, FileId, TreeCursor) -> ParseResult<OpWithData<T, Span>>
where
    T: Clone + core::hash::Hash + Eq,
    F: FnMut(&'_ dyn Asts, FileId, TreeCursor) -> ParseResult<T>,
{
    move |db: &dyn Asts, fd: FileId, c: TreeCursor| {
        let node = c.node();
        f(db, fd, c).map(|o| OpWithData {
            inner: o,
            data: span_from_node(fd, &node),
        })
    }
}

astify! {
    enum top_level_statement = TopLevelStatement {
        ParserDefinition(parser_definition),
        Import(import),
    };
}

astify! {
    struct import = Import {
        name: idspan[!],
    };
}

astify! {
    struct parser_definition = ParserDefinition {
        qualifier: quali[?],
        kind: def_kind[!],
        name: idspan[!],
        argdefs: arg_def_list[?],
        type_params: generic_param_list[?],
        from: expression(type_expression)[?],
        to: expression(val_expression)[!],
        ret_ty: expression(type_expression)[?],
    };
}

astify! {
    struct generic_param_list = GenericsList {
        args: identifier[*],
    };
}

astify! {
    struct lambda = Lambda {
        argdefs: lambda_arg_def_list[!],
        expr: expression(val_expression)[!],
    };
}

astify! {
    struct arg_definition = ArgDefinition {
        name: idspan[!],
        ty: expression(type_expression)[?],
    };
}

astify! {
    struct arg_def_list = ArgDefList {
        args: arg_definition[*],
    };
}

astify! {
    struct lambda_arg_def_list = ArgDefList {
        args: arg_definition[*],
    };
}

astify! {
    struct parse_statement = ParseStatement {
        name: fieldspan[?],
        parser: expression(val_expression)[!],
    };
}

astify! {
    struct let_statement = LetStatement {
        name: fieldspan[!],
        ty: expression(type_expression)[?],
        expr: expression(val_expression)[!],
    };
}

astify! {
    struct type_array = TypeArray {
        expr: expression(type_expression)[!],
    };
}

struct ParserBlock {
    content: Option<ParserSequence>,
    span: Span,
}

impl From<ParserBlock> for ParserAtom {
    fn from(value: ParserBlock) -> Self {
        ParserAtom::Block(Block {
            content: value.content,
            is_parser: true,
            span: value.span,
        })
    }
}

astify! {
    struct parser_block = ParserBlock {
        content: parser_sequence[?],
    };
}

struct FBlock {
    content: Option<ParserSequence>,
    span: Span,
}

impl From<FBlock> for ParserAtom {
    fn from(value: FBlock) -> Self {
        ParserAtom::Block(Block {
            content: value.content,
            is_parser: false,
            span: value.span,
        })
    }
}

astify! {
    struct block = FBlock {
        content: parser_sequence[?],
    };
}

astify! {
    struct parser_choice = ParserChoice {
        content: parser_sequence[*],
    };
}

astify! {
    struct parser_sequence = ParserSequence {
        content: parser_sequence_element[*],
    };
}

astify! {
    enum parser_sequence_element = ParserSequenceElement {
        Choice(boxed(parser_choice)),
        Statement(statement..),
    };
}

astify! {
    struct parserdef_ref = ParserDefRef {
        name: idspan[*],
        args: expression(type_expression)[*],
    };
}

astify! {
    enum statement = Statement {
        Parse(boxed(parse_statement)),
        Let(boxed(let_statement)),
    };
}

astify! {
    struct range = Range {
        start: integer[!],
        end: integer[!],
    };
}

enum IntLiteral {
    Number(i64),
}

astify! {
    enum int_literal = IntLiteral {
        Number(number_literal),
        Number(char_literal),
    };
}

astify! {
    enum atom = Atom {
        Field(identifier),
        Field(retvrn),
        Bool(bool_literal),
        Number(integer..),
    };
}

astify! {
    enum val_expression = ValExpressionInner {
        Variadic(fun_application),
        Dyadic(binary_expression),
        Monadic(unary_expression),
        Monadic(constraint_apply),
        Monadic(single_constraint_apply),
        Monadic(val_dot),
        Monadic(bt_mark),
        Niladic(with_span_data(parser_block)),
        Niladic(with_span_data(block)),
        Niladic(with_span_data(lambda)),
        Niladic(with_span_data(single)),
        Niladic(with_span_data(array_fill)),
        Niladic(parser_span),
        Niladic(with_span_data(regex_literal)),
        Niladic(with_span_data(string_literal)),
        Niladic(with_span_data(atom..)),
    };
}

astify! {
    enum constraint_expression = ConstraintExpressionInner {
        Dyadic(binary_constraint_expression),
        Monadic(unary_constraint_expression),
        Niladic(with_span_data(range)),
        Niladic(with_span_data(not_eof)),
        Niladic(with_span_data(atom..)),
    };
}

astify! {
    enum type_expression = TypeExpressionInner {
        Variadic(type_fun_application),
        Dyadic(binary_type_expression),
        Monadic(unary_type_expression),
        Monadic(type_constraint),
        Niladic(with_span_data(primitive_type)),
        Niladic(with_span_data(parserdef_ref)),
        Niladic(with_span_data(type_array)),
        Niladic(with_span_data(byte_slice)),
        Niladic(with_span_data(placeholder)),
    };
}

struct ParserSpan {
    start: FieldName,
    end: Option<FieldName>,
    span: Span,
}

impl From<ParserSpan> for OpWithData<ParserAtom, Span> {
    fn from(value: ParserSpan) -> Self {
        OpWithData {
            inner: ParserAtom::Span(value.start, value.end.unwrap_or(value.start)),
            data: value.span,
        }
    }
}

astify! {
    struct parser_span = ParserSpan {
        start: field_name[!],
        end: field_name[?],
    };
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct SingleConstraint {
    op: Spanned<WiggleKind>,
    constraint: ConstraintExpression,
    span: Span,
}

impl From<SingleConstraint> for MonadicExpr<AstValSpanned> {
    fn from(constraint: SingleConstraint) -> MonadicExpr<AstValSpanned> {
        MonadicExpr {
            op: OpWithData {
                data: constraint.op.span,
                inner: ValUnOp::Wiggle(Arc::new(constraint.constraint), constraint.op.inner),
            },
            inner: Box::new(Expression::<AstValSpanned>::new_niladic(OpWithData {
                inner: ParserAtom::Single,
                data: constraint.span,
            })),
        }
    }
}

astify! {
    struct single_constraint_apply = SingleConstraint {
        op: spanned(wiggle_kind)[!],
        constraint: expression(constraint_expression)[!]
    };
}

astify! {
    struct fun_application = FunApplication {
        applicant: expression(val_expression)[!],
        args: expression(val_expression)[*],
        partial: partial_indicator[?],
    };
}

astify! {
    struct type_fun_application = TypeFunApplication {
        result: expression(type_expression)[!],
        args: expression(type_expression)[*],
    };
}

struct ConstraintApply {
    left: ValExpression,
    op: Spanned<WiggleKind>,
    right: ConstraintExpression,
    #[allow(dead_code)]
    span: Span,
}

astify! {
    struct constraint_apply = ConstraintApply {
        left: expression(val_expression)[!],
        op: spanned(wiggle_kind)[!],
        right: expression(constraint_expression)[!],
    };
}

impl From<ConstraintApply> for MonadicExpr<AstValSpanned> {
    fn from(val: ConstraintApply) -> Self {
        Monadic {
            op: OpWithData {
                data: val.op.span,
                inner: ValUnOp::Wiggle(Arc::new(val.right), val.op.inner),
            },
            inner: Box::new(val.left),
        }
    }
}

struct TypeConstraint {
    left: TypeExpression,
    op: Spanned<String>,
    right: ConstraintExpression,
    #[allow(dead_code)]
    span: Span,
}

astify! {
    struct type_constraint = TypeConstraint {
        left: expression(type_expression)[!],
        op: spanned(node_to_string)[!],
        right: expression(constraint_expression)[!],
    };
}

impl From<TypeConstraint> for MonadicExpr<AstTypeSpanned> {
    fn from(val: TypeConstraint) -> Self {
        Monadic {
            op: OpWithData {
                data: val.op.span,
                inner: TypeUnOp::Wiggle(Arc::new(val.right)),
            },
            inner: Box::new(val.left),
        }
    }
}

struct ValDot {
    left: ValExpression,
    op: String,
    right: Spanned<FieldName>,
    #[allow(dead_code)]
    span: Span,
}

astify! {
    struct val_dot = ValDot {
        left: expression(val_expression)[!],
        op: node_to_string[!],
        right: spanned(field_name)[!],
    };
}

impl From<ValDot> for MonadicExpr<AstValSpanned> {
    fn from(val: ValDot) -> Self {
        let access_mode = if val.op == "." {
            FieldAccessMode::Normal
        } else if val.op == ".?" {
            FieldAccessMode::Backtrack
        } else {
            panic!("unknown field access mode: {}", val.op)
        };
        Monadic {
            op: OpWithData {
                data: val.right.span,
                inner: ValUnOp::Dot(val.right.inner, access_mode),
            },
            inner: Box::new(val.left),
        }
    }
}

struct BtMark {
    left: ValExpression,
    op: Spanned<BtMarkKind>,
    #[allow(dead_code)]
    span: Span,
}

astify! {
    struct bt_mark = BtMark {
        left: expression(val_expression)[!],
        op: spanned(bt_mark_kind)[!],
    };
}

impl From<BtMark> for MonadicExpr<AstValSpanned> {
    fn from(val: BtMark) -> Self {
        Monadic {
            op: OpWithData {
                data: val.op.span,
                inner: ValUnOp::BtMark(val.op.inner),
            },
            inner: Box::new(val.left),
        }
    }
}

fn wiggle_kind(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<WiggleKind> {
    let str = node_to_string(db, fd, c)?;
    match &*str {
        "is" => Ok(WiggleKind::Is),
        "try" => Ok(WiggleKind::Try),
        _ => panic!("unknown wiggle kind: {str}"),
    }
}

fn quali(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<Qualifier> {
    let str = node_to_string(db, fd, c)?;
    if str == "export" {
        Ok(Qualifier::Export)
    } else {
        panic!("unknown qualifier '{str}'");
    }
}

fn byte_slice(_: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<TypeAtom> {
    let span = span_from_node(fd, &c.node());
    let byte_expr = OpWithData {
        inner: TypeAtom::Primitive(TypePrimitive::U8),
        data: span,
    };
    let array = TypeArray {
        expr: TypeExpression::new_niladic(byte_expr),
        span,
    };
    Ok(TypeAtom::Array(Box::new(array)))
}

fn single(_: &dyn Asts, _: FileId, _: TreeCursor) -> ParseResult<ParserAtom> {
    Ok(ParserAtom::Single)
}

fn array_fill(_: &dyn Asts, _: FileId, _: TreeCursor) -> ParseResult<ParserAtom> {
    Ok(ParserAtom::ArrayFill)
}

fn not_eof(_: &dyn Asts, _: FileId, _: TreeCursor) -> ParseResult<ConstraintAtom> {
    Ok(ConstraintAtom::NotEof)
}

fn retvrn(_: &dyn Asts, _: FileId, _: TreeCursor) -> ParseResult<FieldName> {
    Ok(FieldName::Return)
}

fn placeholder(_: &dyn Asts, _: FileId, _: TreeCursor) -> ParseResult<TypeAtom> {
    Ok(TypeAtom::Placeholder)
}

fn partial_indicator(_: &dyn Asts, _: FileId, _: TreeCursor) -> ParseResult<()> {
    Ok(())
}

fn bt_mark_kind(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<BtMarkKind> {
    let str = node_to_string(db, fd, c)?;
    match &*str {
        "!" => Ok(BtMarkKind::RemoveBt),
        "?" => Ok(BtMarkKind::KeepBt),
        _ => panic!("unknown backtrack mark kind: {str}"),
    }
}

fn identifier(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<Identifier> {
    let str = spanned(node_to_string)(db, fd, c)?;
    let id = IdentifierName { name: str.inner };
    Ok(db.intern_identifier(id))
}

fn fieldspan(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<FieldSpan> {
    spanned(field_name)(db, fd, c)
}

fn field_name(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<FieldName> {
    match c.node().kind() {
        "identifier" => {
            let id = identifier(db, fd, c)?;
            Ok(FieldName::Ident(id))
        }
        "retvrn" => Ok(FieldName::Return),
        otherwise => panic!("unknown field name {otherwise}"),
    }
}

fn idspan(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<IdSpan> {
    let str = spanned(node_to_string)(db, fd, c)?;
    let id = IdentifierName { name: str.inner };
    Ok(IdSpan {
        inner: db.intern_identifier(id),
        span: str.span,
    })
}

fn integer(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<i64> {
    let IntLiteral::Number(n) = int_literal(db, fd, c)?;
    Ok(n)
}

fn number_literal(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<i64> {
    let Spanned { inner, span } = spanned(node_to_string)(db, fd, c)?;
    let (num, radix) = if let Some(hex) = inner.strip_prefix("0x") {
        (hex, 16)
    } else if let Some(oct) = inner.strip_prefix("0o") {
        (oct, 8)
    } else if let Some(bin) = inner.strip_prefix("0b") {
        (bin, 2)
    } else {
        (inner.as_str(), 10)
    };
    i64::from_str_radix(num, radix).map_err(|_| vec![ParseError::NumberTooBig(span)])
}

fn char_literal(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<i64> {
    let Spanned { inner, span } = spanned(node_to_string)(db, fd, c)?;
    let without_quotes = inner
        .strip_prefix('\'')
        .and_then(|s| s.strip_suffix('\''))
        .ok_or_else(|| vec![ParseError::InvalidChar(span)])?;
    let mut it = without_quotes.chars();
    match it.next() {
        Some('\\') => {
            let c = it
                .next()
                .ok_or_else(|| vec![ParseError::InvalidChar(span)])?;
            let val = match c {
                'n' => b'\n' as i64,
                'r' => b'\r' as i64,
                't' => b'\t' as i64,
                '\\' => b'\\' as i64,
                '\'' => b'\'' as i64,
                '0' => 0,
                _ => return Err(vec![ParseError::InvalidChar(span)]),
            };
            Ok(val)
        }
        Some(c) => Ok(c as i64),
        None => Err(vec![ParseError::InvalidChar(span)]),
    }
}

fn string_literal(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<ParserAtom> {
    let Spanned { inner, span } = spanned(node_to_string)(db, fd, c)?;
    let without_quotes = inner
        .strip_prefix('\"')
        .and_then(|s| s.strip_suffix('\"'))
        .ok_or_else(|| vec![ParseError::InvalidString(span)])?;
    let mut ret = String::new();
    let mut it = without_quotes.chars();
    while let Some(c) = it.next() {
        let next = match c {
            '\\' => {
                let c = it
                    .next()
                    .ok_or_else(|| vec![ParseError::InvalidString(span)])?;
                match c {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '\"' => '\"',
                    '0' => '\0',
                    _ => return Err(vec![ParseError::InvalidString(span)]),
                }
            }
            c => c,
        };
        ret.push(next)
    }
    Ok(ParserAtom::String(ret))
}

fn bool_literal(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<bool> {
    let str = node_to_string(db, fd, c)?;
    match &*str {
        "true" => Ok(true),
        "false" => Ok(false),
        _ => panic!("unknown bool literal: {str}"),
    }
}

fn regex_literal(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<ParserAtom> {
    let node = spanned(node_to_string)(db, fd, c)?;
    let (inner, kind) = match node.inner.strip_prefix('h') {
        Some(inner) => (inner, RegexKind::Hexagex),
        None => (node.inner.as_str(), RegexKind::Regular),
    };
    let without_slashes = inner
        .strip_prefix('/')
        .and_then(|s| s.strip_suffix('/'))
        .expect("invalid regex literal");
    let mut unescaped_slashes = String::with_capacity(without_slashes.len());
    let mut backslash = false;
    for c in without_slashes.chars() {
        if backslash {
            if c == '/' {
                unescaped_slashes.push(c);
            } else {
                unescaped_slashes.push('\\');
                unescaped_slashes.push(c);
            }
            backslash = false;
        } else if c == '\\' {
            backslash = true;
        } else {
            unescaped_slashes.push(c);
        }
    }
    assert!(!backslash);
    let regex = db.intern_regex(yaboc_base::interner::RegexData {
        kind,
        regex: unescaped_slashes,
    });
    Ok(ParserAtom::Regex(regex))
}

fn node_to_string(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<String> {
    let node = check_error(db, fd, c.node())?;
    let text = node
        .utf8_text(db.file_content(fd).as_bytes())
        .unwrap()
        .to_string();
    Ok(text)
}

fn def_kind(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<DefKind> {
    let str = node_to_string(db, fd, c)?;
    Ok(match str.as_ref() {
        "def" => DefKind::Def,
        "fun" => DefKind::Fun,
        "static" => DefKind::Static,
        otherwise => panic!("Unknown thunk {otherwise}"),
    })
}

fn primitive_type(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<TypePrimitive> {
    let str = node_to_string(db, fd, c)?;
    Ok(match str.as_ref() {
        "int" => TypePrimitive::Int,
        "bit" => TypePrimitive::Bit,
        "char" => TypePrimitive::Char,
        "u8" => TypePrimitive::U8,
        otherwise => panic!("Unknown type primitive: {otherwise}"),
    })
}
