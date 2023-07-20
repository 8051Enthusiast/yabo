use std::sync::Arc;

use tree_sitter::{Node, Parser, TreeCursor};
use tree_sitter_yabo::language;

use super::*;
use yaboc_base::error::SilencedError;
use yaboc_base::interner::FieldName;
use yaboc_base::interner::Identifier;
use yaboc_base::interner::IdentifierName;
use yaboc_base::interner::TypeVar;
use yaboc_base::interner::TypeVarName;
use yaboc_base::source::FieldSpan;
use yaboc_base::source::{FileId, IdSpan, Span, Spanned};

macro_rules! inner_string {
    ($n:ident) => {stringify!($n)};
    ($_:ident ..) => {_};
    ($f:ident $n:tt) => {inner_string!$n};
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GenericParseError {
    pub(super) loc: Span,
}

impl From<GenericParseError> for SilencedError {
    fn from(_val: GenericParseError) -> Self {
        SilencedError::new()
    }
}

pub type ParseResult<T> = Result<T, Vec<GenericParseError>>;

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

pub fn parse(db: &dyn Asts, fd: FileId) -> Result<Module, Vec<GenericParseError>> {
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
        Err(vec![GenericParseError { loc: span }])
    } else {
        Ok(node)
    }
}

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

fn parser_sequence(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<ParserSequence> {
    let node = check_error(db, fd, c.node())?;
    let mut statements = Vec::new();
    let mut current_choice: Option<ParserChoice> = None;
    let clear_choice_and_push_statement =
        |statements: &mut Vec<ParserSequenceElement>, current_choice: &mut Option<ParserChoice>| {
            if let Some(choice) = current_choice.take() {
                statements.push(ParserSequenceElement::Choice(Box::new(choice)));
            }
        };
    iter_children(db, fd, c, |node, cursor| {
        // we want to skip parens
        if node.is_named() {
            match parser_sequence_element(db, fd, cursor)? {
                ParserSequenceElement::Choice(mut c) => match current_choice {
                    Some(ref mut prev_choice) => {
                        prev_choice.content.append(&mut c.content);
                    }
                    None => {
                        current_choice = Some(*c);
                    }
                },
                otherwise => {
                    clear_choice_and_push_statement(&mut statements, &mut current_choice);
                    statements.push(otherwise)
                }
            }
        }
        Ok(())
    })?;
    clear_choice_and_push_statement(&mut statements, &mut current_choice);

    Ok(ParserSequence {
        content: statements,
        span: span_from_node(fd, &node),
    })
}

fn get_op<'a>(
    db: &dyn Asts,
    fd: FileId,
    c: TreeCursor<'a>,
) -> ParseResult<(Option<TreeCursor<'a>>, Spanned<String>, TreeCursor<'a>)> {
    let mut left = None;
    let mut right = None;
    let mut op = None;
    iter_children(db, fd, c, |_, cursor| {
        let field = cursor.field_name();
        match field {
            // hack: we want to skip parens
            Some("left") if cursor.node().is_named() => left = Some(cursor),
            Some("right") if cursor.node().is_named() => right = Some(cursor),
            Some("op") => op = Some(cursor),
            Some("left" | "right") => (),
            Some(other) => panic!("Unknown field {other}"),
            None => (),
        }
        Ok(())
    })?;
    let right = right.unwrap();
    let op = spanned(node_to_string)(db, fd, op.unwrap())?;
    Ok((left, op, right))
}

fn binary_type_expression(
    db: &dyn Asts,
    fd: FileId,
    c: TreeCursor,
) -> ParseResult<DyadicExpr<AstTypeSpanned>> {
    dyadic(
        |x| TypeBinOp::parse_from_str(x).unwrap(),
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
        |x| ConstraintBinOp::parse_from_str(x).unwrap(),
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
        |x| ValBinOp::parse_from_str(x).unwrap(),
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
    mut f: impl FnMut(&str) -> Kind::DyadicOp,
    mut sub_expr: impl FnMut(
        &dyn Asts,
        FileId,
        TreeCursor,
    ) -> ParseResult<Expression<KindWithData<Kind, Span>>>,
) -> impl FnMut(&dyn Asts, FileId, TreeCursor) -> ParseResult<DyadicExpr<KindWithData<Kind, Span>>>
{
    move |db, fd, c| {
        let (left, op, right) = get_op(db, fd, c)?;
        let left = left.unwrap();
        let span = op.span;

        Ok(Dyadic {
            op: OpWithData {
                data: span,
                inner: f(&op.inner),
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
        let (_, op, right) = get_op(db, fd, c)?;
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
        thunky: thunky_sign[!],
        name: idspan[!],
        argdefs: arg_def_list[?],
        from: expression(type_expression)[?],
        to: expression(val_expression)[!],
        ret_ty: expression(type_expression)[?],
    };
}

astify! {
    struct arg_definition = ArgDefinition {
        name: idspan[!],
        ty: expression(type_expression)[!],
    };
}

astify! {
    struct arg_def_list = ArgDefList {
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

astify! {
    struct parser_block = Block {
        content: parser_sequence[?],
    };
}

astify! {
    struct parser_choice = ParserChoice {
        content: parser_sequence[*],
    };
}

astify! {
    struct parserdef_ref = ParserDefRef {
        from: expression(type_expression)[?],
        name: idspan[*],
        args: expression(type_expression)[*],
    };
}

astify! {
    enum parser_sequence_element = ParserSequenceElement {
        Choice(boxed(parser_choice)),
        Statement(statement..),
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
        start: number_literal[!],
        end: number_literal[!],
    };
}

astify! {
    struct bt_name = BtName {
        name: field_name[!],
        backtrack: question_mark[?],
    };
}

astify! {
    enum atom = Atom {
        Field(bt_name),
        Number(number_literal),
        Number(char_literal),
        Bool(bool_literal),
    };
}

astify! {
    enum val_expression = ValExpressionInner {
        Variadic(fun_application),
        Dyadic(binary_expression),
        Monadic(unary_expression),
        Monadic(constraint_apply),
        Monadic(val_dot),
        Niladic(with_span_data(parser_block)),
        Niladic(with_span_data(single)),
        Niladic(with_span_data(nil)),
        Niladic(with_span_data(array)),
        Niladic(with_span_data(regex_literal)),
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
        Niladic(with_span_data(type_var)),
    };
}

astify! {
    struct fun_application = FunApplication {
        applicant: expression(val_expression)[!],
        args: expression(val_expression)[*],
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
    op: Spanned<String>,
    right: (FieldName, bool),
    #[allow(dead_code)]
    span: Span,
}

astify! {
    struct val_dot = ValDot {
        left: expression(val_expression)[!],
        op: spanned(node_to_string)[!],
        right: into(bt_name)[!],
    };
}

impl From<ValDot> for MonadicExpr<AstValSpanned> {
    fn from(val: ValDot) -> Self {
        Monadic {
            op: OpWithData {
                data: val.op.span,
                inner: ValUnOp::Dot(val.right.0, val.right.1),
            },
            inner: Box::new(val.left),
        }
    }
}

fn wiggle_kind(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<WiggleKind> {
    let str = node_to_string(db, fd, c)?;
    match &*str {
        "if" => Ok(WiggleKind::If),
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

fn single(_: &dyn Asts, _: FileId, _: TreeCursor) -> ParseResult<ParserAtom> {
    Ok(ParserAtom::Single)
}

fn nil(_: &dyn Asts, _: FileId, _: TreeCursor) -> ParseResult<ParserAtom> {
    Ok(ParserAtom::Nil)
}

fn array(_: &dyn Asts, _: FileId, _: TreeCursor) -> ParseResult<ParserAtom> {
    Ok(ParserAtom::Array)
}

fn not_eof(_: &dyn Asts, _: FileId, _: TreeCursor) -> ParseResult<ConstraintAtom> {
    Ok(ConstraintAtom::NotEof)
}

fn question_mark(_: &dyn Asts, _: FileId, _: TreeCursor) -> ParseResult<()> {
    Ok(())
}

fn identifier(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<Identifier> {
    let str = spanned(node_to_string)(db, fd, c)?;
    let id = IdentifierName { name: str.inner };
    Ok(db.intern_identifier(id))
}

fn type_var(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<TypeVar> {
    let str = spanned(node_to_string)(db, fd, c)?;
    let id = TypeVarName::new(str.inner);
    Ok(db.intern_type_var(id))
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
    i64::from_str_radix(num, radix).map_err(|_| vec![GenericParseError { loc: span }])
}

fn char_literal(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<i64> {
    let Spanned { inner, span } = spanned(node_to_string)(db, fd, c)?;
    let without_quotes = inner
        .strip_prefix('\'')
        .and_then(|s| s.strip_suffix('\''))
        .ok_or_else(|| vec![GenericParseError { loc: span }])?;
    let mut it = without_quotes.chars();
    let first = it
        .next()
        .filter(|_| it.next().is_none())
        .ok_or_else(|| vec![GenericParseError { loc: span }])?;
    Ok(first as i64)
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
    let (has_questionmark, str) = if let Some(without_questionmark) = node.inner.strip_suffix('?') {
        (true, without_questionmark)
    } else {
        (false, node.inner.as_str())
    };
    let without_slashes = str
        .strip_prefix('/')
        .and_then(|s| s.strip_suffix('/'))
        .ok_or_else(|| vec![GenericParseError { loc: node.span }])?;
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
    if backslash {
        return Err(vec![GenericParseError { loc: node.span }]);
    }
    let regex = db.intern_regex(unescaped_slashes);
    Ok(ParserAtom::Regex(regex, has_questionmark))
}

fn node_to_string(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<String> {
    let node = check_error(db, fd, c.node())?;
    let text = node
        .utf8_text(db.file_content(fd).as_bytes())
        .unwrap()
        .to_string();
    Ok(text)
}

fn thunky_sign(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<bool> {
    let str = node_to_string(db, fd, c)?;
    Ok(match str.as_ref() {
        "def" => true,
        "fun" => false,
        otherwise => panic!("Unknown thunk {otherwise}"),
    })
}

fn primitive_type(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<TypePrimitive> {
    let str = node_to_string(db, fd, c)?;
    Ok(match str.as_ref() {
        "mem" => TypePrimitive::Mem,
        "int" => TypePrimitive::Int,
        "bit" => TypePrimitive::Bit,
        "char" => TypePrimitive::Char,
        otherwise => panic!("Unknown type primitive: {otherwise}"),
    })
}
