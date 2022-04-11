use std::sync::Arc;

use tree_sitter::{Node, Parser, TreeCursor};
use tree_sitter_yabo::language;

use crate::ast::*;
use crate::error::SilencedError;
use crate::expr::*;
use crate::interner::FieldName;
use crate::interner::Identifier;
use crate::interner::IdentifierName;
use crate::interner::TypeVar;
use crate::interner::TypeVarName;
use crate::source::FieldSpan;
use crate::source::{FileId, IdSpan, Span, Spanned};

macro_rules! inner_string {
    ($n:ident) => {stringify!($n)};
    ($_:ident ..) => {_};
    ($f:ident $n:tt) => {inner_string!$n};
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GenericParseError {
    pub(super) loc: Span,
}

impl Into<SilencedError> for GenericParseError {
    fn into(self) -> SilencedError {
        SilencedError
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
        statements.push(Arc::new(parser_definition(db, fd, cursor)?));
        Ok(())
    })?;
    Ok(Module {
        tl_statements: statements,
        span: span_from_node(fd, &node),
    })
}

fn parser_sequence(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<ParserSequence> {
    let node = check_error(db, fd, c.node())?;
    let mut statements = Vec::new();
    iter_children(db, fd, c, |node, cursor| {
        // we want to skip parens
        if node.is_named() {
            statements.push(block_content(db, fd, cursor)?);
        }
        Ok(())
    })?;
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
            Some(other) => panic!("Unknown field {}", other),
            None => (),
        }
        Ok(())
    })?;
    let right = right.unwrap();
    let op = spanned(node_to_string)(db, fd, op.unwrap())?;
    Ok((left, op, right))
}

fn binary_type_expression(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<AstTypeBinOp> {
    use TypeBinOp::*;
    let (left, op, right) = get_op(db, fd, c)?;
    let left = left.unwrap();
    let ty = |x| type_expression(db, fd, x);
    let constr = |x| constraint_expression(db, fd, x);
    let span = op.span;
    Ok(match op.inner.as_str() {
        "&>" => Ref(ty(left)?, ty(right)?, span),
        "*>" => ParseArg(ty(left)?, ty(right)?, span),
        "~" => Wiggle(ty(left)?, constr(right)?, span),
        otherwise => panic!("Invalid constrain operator \"{}\"", otherwise),
    })
}

fn unary_type_expression(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<AstTypeUnOp> {
    use TypeUnOp::*;
    let (_, op, right) = get_op(db, fd, c)?;
    let ty = |x| type_expression(db, fd, x);
    let span = op.span;
    Ok(match op.inner.as_str() {
        "&" => Ref(ty(right)?, span),
        otherwise => panic!("Invalid constrain operator \"{}\"", otherwise),
    })
}
fn binary_constraint_expression(
    db: &dyn Asts,
    fd: FileId,
    c: TreeCursor,
) -> ParseResult<AstConstraintBinOp> {
    use ConstraintBinOp::*;
    let (left, op, right) = get_op(db, fd, c)?;
    let left = left.unwrap();
    let constr = |x| constraint_expression(db, fd, x);
    let span = op.span;
    Ok(match op.inner.as_str() {
        "and" => And(constr(left)?, constr(right)?, span),
        "or" => Or(constr(left)?, constr(right)?, span),
        "." => Dot(constr(left)?, atom(db, fd, right)?, span),
        otherwise => panic!("Invalid constrain operator \"{}\"", otherwise),
    })
}

fn unary_constraint_expression(
    db: &dyn Asts,
    fd: FileId,
    c: TreeCursor,
) -> ParseResult<AstConstraintUnOp> {
    use ConstraintUnOp::*;
    let (_, op, right) = get_op(db, fd, c)?;
    let constr = |x| constraint_expression(db, fd, x);
    let span = op.span;
    Ok(match op.inner.as_str() {
        "!" => Not(constr(right)?, span),
        otherwise => panic!("Invalid constrain operator \"{}\"", otherwise),
    })
}

fn binary_expression(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<AstValBinOp> {
    use ValBinOp::*;
    let (left, op, right) = get_op(db, fd, c)?;
    let left = left.unwrap();
    let val = |x| val_expression(db, fd, x);
    let constr = |x| constraint_expression(db, fd, x);
    let span = op.span;

    Ok(match BasicValBinOp::parse_from_str(&op.inner) {
        Ok(op) => Basic(val(left)?, op, val(right)?, span),
        Err("~") => Wiggle(val(left)?, constr(right)?, span),
        Err("else") => Else(val(left)?, val(right)?, span),
        Err(".") => Dot(val(left)?, atom(db, fd, right)?, span),
        Err(otherwise) => panic!("Invalid constrain operator \"{}\"", otherwise),
    })
}

fn unary_expression(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<AstValUnOp> {
    use ValUnOp::*;
    let (_, op, right) = get_op(db, fd, c)?;
    let val = |x| val_expression(db, fd, x);
    let span = op.span;
    Ok(match op.inner.as_str() {
        "!" => Not(val(right)?, span),
        "-" => Neg(val(right)?, span),
        "+" => Pos(val(right)?, span),
        otherwise => panic!("Invalid constrain operator \"{}\"", otherwise),
    })
}

macro_rules! maybe_unwrap {
    (?) => {
        |mut t: Vec<_>| t.pop()
    };
    (!) => {
        |mut t: Vec<_>| t.pop().unwrap()
    };
    (*) => {
        |t| t
    };
}

macro_rules! callable {
    ($n:ident) => {into($n)};
    ($n:ident ..) => {into($n)};
    ($f:ident $n:tt) => {$f(callable!$n)};
}

macro_rules! astify {
    {struct $new:ident = $node:ident {$($name:ident: $fun:ident $require:tt),+$(,)?};} => {
        fn $new(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<$node> {
//            eprintln!("Enter {}", stringify!($new));
            let node = c.node();
            $(let mut $name = Vec::new();)+
            iter_children(db, fd, c, |_, cursor| {
                let field = cursor.field_name();
                match field {
                    $(Some(stringify!($name)) => $name.push($fun(db, fd, cursor.clone())?)),+,
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
    {$($t:tt $new:ident = $node:ident $rest:tt;)+} => {
        $(astify!{
            $t $new = $node $rest;
        })+
    };
}

fn boxed<T, F: FnMut(&'_ dyn Asts, FileId, TreeCursor) -> ParseResult<T>>(
    mut f: F,
) -> impl FnMut(&'_ dyn Asts, FileId, TreeCursor) -> ParseResult<Box<T>> {
    move |db: &dyn Asts, fd: FileId, c: TreeCursor| f(db, fd, c).map(Box::new)
}

fn into<T, U: From<T>, F: FnMut(&'_ dyn Asts, FileId, TreeCursor) -> ParseResult<T>>(
    mut f: F,
) -> impl FnMut(&'_ dyn Asts, FileId, TreeCursor) -> ParseResult<U> {
    move |db: &dyn Asts, fd: FileId, c: TreeCursor| f(db, fd, c).map(U::from)
}

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

astify! {
    struct parser_definition = ParserDefinition {
        name: idspan!,
        from: type_expression!,
        to: val_expression!,
    };

    struct parse_statement = ParseStatement {
        name: fieldspan?,
        parser: val_expression!,
    };

    struct let_statement = LetStatement {
        name: fieldspan!,
        ty: type_expression!,
        expr: val_expression!,
    };

    struct parser_array = ParserArray {
        direction: array_direction!,
        expr: val_expression!,
    };

    struct type_array = TypeArray {
        direction: array_direction!,
        expr: type_expression!,
    };

    struct parser_block = Block {
        content: block_content?,
    };

    struct parser_choice = ParserChoice {
        left: block_content!,
        right: block_content!,
    };

    struct parserdef_ref = ParserDefRef {
        from: type_expression?,
        name: idspan!,
        args: type_expression*,
    };

    enum block_content = BlockContent {
        Sequence(boxed(parser_sequence)),
        Choice(boxed(parser_choice)),
        Statement(statement..),
    };

    enum statement = Statement {
        ParserDef(boxed(parser_definition)),
        Parse(boxed(parse_statement)),
        Let(boxed(let_statement)),
    };

    enum atom = Atom {
        Field(identifier),
        Number(number_literal),
        Char(char_literal),
        String(string_literal),
    };

    enum val_expression = ValExpression {
        BinaryOp(boxed(binary_expression)),
        UnaryOp(boxed(unary_expression)),
        Atom(spanned(parser_block)),
        Atom(spanned(parser_array)),
        Atom(spanned(single)),
        Atom(spanned(atom..)),
    };

    enum constraint_expression = ConstraintExpression {
        BinaryOp(boxed(binary_constraint_expression)),
        UnaryOp(boxed(unary_constraint_expression)),
        Atom(spanned(atom..)),
    };

    enum type_expression = TypeExpression {
        BinaryOp(boxed(binary_type_expression)),
        UnaryOp(boxed(unary_type_expression)),
        Atom(spanned(primitive_type)),
        Atom(spanned(parserdef_ref)),
        Atom(spanned(type_array)),
        Atom(spanned(type_var)),
    };
}

fn single(_: &dyn Asts, _: FileId, _: TreeCursor) -> ParseResult<ParserAtom> {
    Ok(ParserAtom::Single)
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
    idspan(db, fd, c).map(|x| FieldSpan {
        id: FieldName::Ident(x.inner),
        span: x.span,
    })
}

fn idspan(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<IdSpan> {
    let str = spanned(node_to_string)(db, fd, c)?;
    let id = IdentifierName { name: str.inner };
    Ok(IdSpan {
        inner: db.intern_identifier(id),
        span: str.span,
    })
}

fn number_literal(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<String> {
    node_to_string(db, fd, c)
}

fn char_literal(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<String> {
    node_to_string(db, fd, c)
}

fn string_literal(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<String> {
    node_to_string(db, fd, c)
}

fn node_to_string(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<String> {
    let node = check_error(db, fd, c.node())?;
    let text = node
        .utf8_text(db.file_content(fd).as_bytes())
        .unwrap()
        .to_string();
    Ok(text)
}

fn array_direction(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<Spanned<ArrayKind>> {
    let str = spanned(node_to_string)(db, fd, c)?;
    Ok(Spanned {
        inner: match str.inner.as_str() {
            "for" => ArrayKind::For,
            "each" => ArrayKind::Each,
            otherwise => panic!("Unknown loop {}", otherwise),
        },
        span: str.span,
    })
}

fn primitive_type(db: &dyn Asts, fd: FileId, c: TreeCursor) -> ParseResult<TypePrimitive> {
    let str = node_to_string(db, fd, c)?;
    Ok(match str.as_ref() {
        "mem" => TypePrimitive::Mem,
        "int" => TypePrimitive::Int,
        "bit" => TypePrimitive::Bit,
        "char" => TypePrimitive::Char,
        otherwise => panic!("Unknown type primitive: {}", otherwise),
    })
}