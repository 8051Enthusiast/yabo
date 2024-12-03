use std::sync::Arc;

use crate::{refs, RegexError};
use hir::{ExprId, HirConstraintId};
use yaboc_ast::expr::{self, Atom, BtMarkKind, FieldAccessMode, WiggleKind};
use yaboc_base::error::{SResult, SilencedError};
use yaboc_base::interner::{DefId, FieldName, Regex};
use yaboc_base::source::SpanIndex;
use yaboc_expr::{
    DataRefExpr, ExprHead, ExprIdx, ExprKind, Expression, FetchExpr, FetchKindData, IdxExpression,
    IndexExpr, PartialEval, ReidxExpr, ShapedData, SmallVec, TakeRef, ZipExpr,
};
use yaboc_hir as hir;
use yaboc_hir::HirIdWrapper;

use super::{ResolveError, Resolves};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedAtom {
    Val(DefId),
    Captured(DefId),
    ParserDef(hir::ParserDefId),
    Global(hir::ParserDefId),
    Span(DefId, DefId),
    Regex(Regex),
    String(String),
    Number(i64),
    Char(u32),
    Bool(bool),
    Single,
    Array,
    ArrayFill,
    Block(hir::BlockId, hir::BlockKind),
    Lambda(hir::LambdaId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValBinOp {
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
    ParserApply,
    Else,
    Then,
    Range,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Origin {
    User,
    Compose,
    Index,
    Array,
    ArrayFill,
}

impl TryFrom<expr::ValBinOp> for ValBinOp {
    type Error = expr::ValBinOp;

    // everything except Index and Compose stays the same
    fn try_from(value: expr::ValBinOp) -> Result<Self, Self::Error> {
        match value {
            expr::ValBinOp::And => Ok(ValBinOp::And),
            expr::ValBinOp::Xor => Ok(ValBinOp::Xor),
            expr::ValBinOp::Or => Ok(ValBinOp::Or),
            expr::ValBinOp::LesserEq => Ok(ValBinOp::LesserEq),
            expr::ValBinOp::Lesser => Ok(ValBinOp::Lesser),
            expr::ValBinOp::GreaterEq => Ok(ValBinOp::GreaterEq),
            expr::ValBinOp::Greater => Ok(ValBinOp::Greater),
            expr::ValBinOp::Uneq => Ok(ValBinOp::Uneq),
            expr::ValBinOp::Equals => Ok(ValBinOp::Equals),
            expr::ValBinOp::ShiftR => Ok(ValBinOp::ShiftR),
            expr::ValBinOp::ShiftL => Ok(ValBinOp::ShiftL),
            expr::ValBinOp::Minus => Ok(ValBinOp::Minus),
            expr::ValBinOp::Plus => Ok(ValBinOp::Plus),
            expr::ValBinOp::Div => Ok(ValBinOp::Div),
            expr::ValBinOp::Modulo => Ok(ValBinOp::Modulo),
            expr::ValBinOp::Mul => Ok(ValBinOp::Mul),
            expr::ValBinOp::ParserApply => Ok(ValBinOp::ParserApply),
            expr::ValBinOp::Else => Ok(ValBinOp::Else),
            expr::ValBinOp::Then => Ok(ValBinOp::Then),
            expr::ValBinOp::Range => Ok(ValBinOp::Range),
            expr::ValBinOp::Index(p) => Err(expr::ValBinOp::Index(p)),
            expr::ValBinOp::Compose => Err(expr::ValBinOp::Compose),
            expr::ValBinOp::At => Err(expr::ValBinOp::At),
            expr::ValBinOp::Array => Err(expr::ValBinOp::Array),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValUnOp<C> {
    Not,
    Neg,
    Wiggle(C, WiggleKind),
    Dot(FieldName, FieldAccessMode),
    Size,
    BtMark(BtMarkKind),
    EvalFun,
    GetAddr,
}

impl<C> TryFrom<expr::ValUnOp<C>> for ValUnOp<C> {
    type Error = expr::ValUnOp<C>;

    // everything except array stays the same
    fn try_from(value: expr::ValUnOp<C>) -> Result<Self, Self::Error> {
        match value {
            expr::ValUnOp::Not => Ok(ValUnOp::Not),
            expr::ValUnOp::Neg => Ok(ValUnOp::Neg),
            expr::ValUnOp::Wiggle(c, k) => Ok(ValUnOp::Wiggle(c, k)),
            expr::ValUnOp::Dot(f, m) => Ok(ValUnOp::Dot(f, m)),
            expr::ValUnOp::Size => Ok(ValUnOp::Size),
            expr::ValUnOp::BtMark(k) => Ok(ValUnOp::BtMark(k)),
            expr::ValUnOp::Array => Err(expr::ValUnOp::Array),
            expr::ValUnOp::ArrayFill => Err(expr::ValUnOp::ArrayFill),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValVarOp {
    PartialApply(Origin),
}

impl TryFrom<expr::ValVarOp> for ValVarOp {
    type Error = expr::ValVarOp;

    fn try_from(value: expr::ValVarOp) -> Result<Self, Self::Error> {
        match value {
            expr::ValVarOp::PartialApply => Ok(ValVarOp::PartialApply(Origin::User)),
            expr::ValVarOp::Call => Err(expr::ValVarOp::Call),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Resolved;

impl ExprKind for Resolved {
    type NiladicOp = ResolvedAtom;
    type MonadicOp = ValUnOp<HirConstraintId>;
    type DyadicOp = ValBinOp;
    type VariadicOp = ValVarOp;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedButSugared;

impl ExprKind for ResolvedButSugared {
    type NiladicOp = ResolvedAtom;
    type MonadicOp = expr::ValUnOp<HirConstraintId>;
    type DyadicOp = expr::ValBinOp;
    type VariadicOp = expr::ValVarOp;
}

pub type ResolvedExpr =
    ZipExpr<Arc<IdxExpression<Resolved>>, Arc<ShapedData<Vec<SpanIndex>, Resolved>>>;

impl<DB: Resolves + ?Sized> FetchExpr<hir::ExprId, DB> for Resolved {
    type Expr = Arc<IdxExpression<Resolved>>;
    type Err = SilencedError;

    fn fetch_expr(db: &DB, id: ExprId) -> Result<Self::Expr, Self::Err> {
        Ok(db.resolve_expr(id)?.expr)
    }
}

impl<DB: Resolves + ?Sized> FetchKindData<SpanIndex, ExprId, DB> for Resolved {
    type Data = Arc<ShapedData<Vec<SpanIndex>, Resolved>>;
    type Err = SilencedError;

    fn fetch_kind_data(db: &DB, id: ExprId) -> Result<Self::Data, Self::Err> {
        Ok(db.resolve_expr(id)?.data)
    }
}

fn resolve_span(
    db: &dyn Resolves,
    start: FieldName,
    end: FieldName,
    expr_id: ExprId,
    span: SpanIndex,
) -> Result<(DefId, DefId), ResolveError> {
    let refs::Resolved::Value(start, refs::VarType::Value) =
        refs::resolve_var_ref(db, expr_id.0, start, false, false)?
    else {
        return Err(ResolveError::Unresolved(expr_id, span, start));
    };
    if !db.hir_node(start)?.is_kind(hir::HirNodeKind::Parse.into()) {
        return Err(ResolveError::NonParserRefInSpan(expr_id, span));
    }
    let refs::Resolved::Value(end, refs::VarType::Value) =
        refs::resolve_var_ref(db, expr_id.0, end, false, false)?
    else {
        return Err(ResolveError::Unresolved(expr_id, span, end));
    };
    if !db.hir_node(end)?.is_kind(hir::HirNodeKind::Parse.into()) {
        return Err(ResolveError::NonParserRefInSpan(expr_id, span));
    }
    if start == end {
        return Ok((start, end));
    }
    let next: fn(_, _) -> _ = |db: &dyn Resolves, id| -> SResult<Option<DefId>> {
        match db.hir_node(id)? {
            hir::HirNode::Parse(hir::ParseStatement { back: next, .. })
            | hir::HirNode::Choice(hir::StructChoice {
                endpoints: Some([_, next]),
                ..
            }) => Ok(match next {
                hir::ParserPredecessor::ChildOf(ctx) => ctx.lookup(db)?.parent_choice.map(|x| x.0),
                hir::ParserPredecessor::After(next_id) => Some(next_id),
            }),
            _ => Ok(None),
        }
    };
    let prev = |db: &dyn Resolves, id| -> SResult<Option<DefId>> {
        match db.hir_node(id)? {
            hir::HirNode::Parse(hir::ParseStatement { front: prev, .. })
            | hir::HirNode::Choice(hir::StructChoice {
                endpoints: Some([prev, _]),
                ..
            }) => Ok(match prev {
                hir::ParserPredecessor::ChildOf(ctx) => ctx.lookup(db)?.parent_choice.map(|x| x.0),
                hir::ParserPredecessor::After(prev_id) => Some(prev_id),
            }),
            _ => Ok(None),
        }
    };
    for (first, step, last) in [(start, next, end), (end, prev, start)] {
        let mut current = first;
        while let Some(next) = step(db, current)? {
            if next == last {
                return Ok((start, end));
            }
            current = next;
        }
    }
    Err(ResolveError::UnorderedSpan(expr_id, span))
}

fn convert_regex_error(
    err: &regex_syntax::Error,
    expr_id: ExprId,
    span: SpanIndex,
) -> ResolveError {
    let (subspan, msg) = match err {
        regex_syntax::Error::Parse(parse_error) => {
            let span = parse_error.span();
            (span, parse_error.kind().to_string())
        }
        regex_syntax::Error::Translate(translate_error) => {
            let span = translate_error.span();
            (span, translate_error.kind().to_string())
        }
        otherwise => return ResolveError::OtherRegexError(otherwise.to_string(), expr_id, span),
    };
    // we need to add 1 to account for the leading '/'
    let start = subspan.start.offset + 1;
    let end = subspan.end.offset + 1;
    ResolveError::RegexParseError(msg, expr_id, span, [start as u32, end as u32])
}

fn resolve_expr_modules(
    db: &dyn Resolves,
    expr_id: ExprId,
    expr: DataRefExpr<hir::HirVal, SpanIndex>,
) -> Result<ReidxExpr<hir::HirVal, ResolvedButSugared>, ResolveError> {
    let parent_closure = db.hir_parent_closure(expr_id.0)?;

    let new_resolved_atom = |loc, name, span, use_core| {
        let (id, kind) = match refs::resolve_var_ref(db, loc, name, use_core, true)? {
            refs::Resolved::Value(id, kind) => (id, kind),
            refs::Resolved::Module(m) => return Ok(PartialEval::Eval(m)),
            refs::Resolved::Unresolved => {
                return Err(ResolveError::Unresolved(expr_id, span, name))
            }
        };
        Ok(PartialEval::Uneval(match kind {
            refs::VarType::ParserDef => ResolvedAtom::ParserDef(hir::ParserDefId(id)),
            refs::VarType::Static => ResolvedAtom::Global(hir::ParserDefId(id)),
            refs::VarType::Value => {
                let is_captured = parent_closure
                    .map(|x| !x.is_ancestor_of(db, id))
                    .unwrap_or(false);
                let is_arg = db.hir_node(id)?.is_kind(hir::HirNodeKind::ArgDef.into());
                if is_captured || is_arg {
                    ResolvedAtom::Captured(id)
                } else {
                    ResolvedAtom::Val(id)
                }
            }
        }))
    };

    expr.try_partial_eval::<hir::ModuleId, ResolvedButSugared, _>(
        |_, idx| {
            Err(ResolveError::ModuleInExpression(
                expr_id,
                *expr.data.index_expr(idx),
            ))
        },
        |_, (head, span)| {
            Ok(PartialEval::Uneval(match head {
                ExprHead::Niladic(n) => ExprHead::Niladic(match n {
                    hir::ParserAtom::Atom(Atom::Number(s)) => ResolvedAtom::Number(s),
                    hir::ParserAtom::Atom(Atom::Char(s)) => ResolvedAtom::Char(s),
                    hir::ParserAtom::Atom(Atom::Bool(s)) => ResolvedAtom::Bool(s),
                    hir::ParserAtom::Single => ResolvedAtom::Single,
                    hir::ParserAtom::ArrayFill => ResolvedAtom::ArrayFill,
                    hir::ParserAtom::Regex(r) => {
                        match db.resolve_regex(r) {
                            Err(RegexError::Syntax(e)) => {
                                return Err(convert_regex_error(&e, expr_id, *span))
                            }
                            Err(RegexError::Opaque(e)) => {
                                return Err(ResolveError::OtherRegexError(e, expr_id, *span))
                            }
                            _ => (),
                        }
                        ResolvedAtom::Regex(r)
                    }
                    hir::ParserAtom::String(str) => ResolvedAtom::String(str),
                    hir::ParserAtom::Block(b, kind) => ResolvedAtom::Block(b, kind),
                    hir::ParserAtom::Lambda(l) => ResolvedAtom::Lambda(l),
                    hir::ParserAtom::Span(start, end) => {
                        let (start, end) = resolve_span(db, start, end, expr_id, *span)?;
                        ResolvedAtom::Span(start, end)
                    }
                    hir::ParserAtom::Atom(Atom::Field(f)) => {
                        match new_resolved_atom(expr_id.0, f, *span, true)? {
                            PartialEval::Uneval(k) => k,
                            PartialEval::Eval(m) => return Ok(PartialEval::Eval(m)),
                        }
                    }
                }),
                // TODO(8051): throw an error or warning if the field access is fallible on a module
                ExprHead::Monadic(expr::ValUnOp::Dot(field, _), PartialEval::Eval((m, _))) => {
                    match new_resolved_atom(m.0, field, *span, false)? {
                        PartialEval::Uneval(k) => ExprHead::Niladic(k),
                        PartialEval::Eval(m) => return Ok(PartialEval::Eval(m)),
                    }
                }
                ExprHead::Monadic(op, inner) => ExprHead::Monadic(op, inner),
                ExprHead::Dyadic(op, inner) => ExprHead::Dyadic(op, inner),
                ExprHead::Variadic(op, inner) => ExprHead::Variadic(op, inner),
            }))
        },
    )
}

pub fn resolve_expr_error(
    db: &dyn Resolves,
    expr_id: hir::ExprId,
) -> Result<ResolvedExpr, ResolveError> {
    type Idx = ExprIdx<ResolvedButSugared>;
    #[derive(Clone, Copy)]
    enum Continue {
        Cont(Idx),
        Item(Idx, hir::CoreItem),
        Block(Idx, hir::BlockId, hir::BlockKind),
        N(Idx, fn(Idx, &ResolvedAtom) -> ExprHead<Resolved, Continue>),
        M(
            Idx,
            fn(Idx, &expr::ValUnOp<HirConstraintId>, Idx) -> ExprHead<Resolved, Continue>,
        ),
        D(
            Idx,
            fn(Idx, &expr::ValBinOp, [Idx; 2]) -> ExprHead<Resolved, Continue>,
        ),
        V(
            Idx,
            fn(Idx, &expr::ValVarOp, &[Idx]) -> ExprHead<Resolved, Continue>,
        ),
    }
    use Continue::*;

    let expr = expr_id.lookup(db)?.expr;
    let resolved = resolve_expr_modules(db, expr_id, expr.take_ref())?;
    let sugar_spans = resolved.migrate_data(&expr.data);
    let zip_expr = resolved.into_expr().zip(sugar_spans);
    let core_items = db.core_items()?;
    let desugared = ZipExpr::new_from_unfold(Cont(zip_expr.root()), |cont| {
        let (Cont(id) | Item(id, _) | N(id, _) | M(id, _) | D(id, _) | V(id, _) | Block(id, ..)) =
            cont;
        let (head, span) = zip_expr.index_expr(id);
        let desugared = match (cont, head) {
            (Item(_, item), _) => ExprHead::Niladic(ResolvedAtom::ParserDef(core_items[item])),
            (Block(_, id, kind), _) => ExprHead::Niladic(ResolvedAtom::Block(id, kind)),
            (N(_, f), ExprHead::Niladic(atom)) => f(id, atom),
            (M(_, f), ExprHead::Monadic(op, inner)) => f(id, op, *inner),
            (D(_, f), ExprHead::Dyadic(op, [lhs, rhs])) => f(id, op, [*lhs, *rhs]),
            (V(_, f), ExprHead::Variadic(op, inner)) => f(id, op, inner),
            (N(..) | M(..) | D(..) | V(..), _) => unreachable!(),
            (Cont(_), ExprHead::Dyadic(expr::ValBinOp::Compose, _)) => ExprHead::Monadic(
                ValUnOp::EvalFun,
                D(id, |id, _, [lhs, rhs]| {
                    ExprHead::Variadic(
                        ValVarOp::PartialApply(Origin::Compose),
                        SmallVec::from(
                            &[Item(id, hir::CoreItem::Compose), Cont(lhs), Cont(rhs)][..],
                        ),
                    )
                }),
            ),
            (Cont(_), ExprHead::Dyadic(expr::ValBinOp::At, [lhs, _])) => ExprHead::Dyadic(
                ValBinOp::ParserApply,
                [
                    D(id, |_, _, [_, rhs]| {
                        ExprHead::Monadic(ValUnOp::GetAddr, Cont(rhs))
                    }),
                    Cont(*lhs),
                ],
            ),
            (Cont(_), ExprHead::Dyadic(expr::ValBinOp::Index(_), [lhs, _])) => ExprHead::Dyadic(
                ValBinOp::ParserApply,
                [
                    Cont(*lhs),
                    D(id, |id, op, _| {
                        let expr::ValBinOp::Index(bt) = op else {
                            unreachable!()
                        };
                        ExprHead::Monadic(
                            ValUnOp::BtMark(*bt),
                            D(id, |id, _, _| {
                                ExprHead::Monadic(
                                    ValUnOp::EvalFun,
                                    D(id, |id, _, [_, rhs]| {
                                        ExprHead::Variadic(
                                            ValVarOp::PartialApply(Origin::Index),
                                            SmallVec::from(
                                                &[Item(id, hir::CoreItem::Index), Cont(rhs)][..],
                                            ),
                                        )
                                    }),
                                )
                            }),
                        )
                    }),
                ],
            ),
            (Cont(_), ExprHead::Monadic(expr::ValUnOp::Array, _)) => ExprHead::Monadic(
                ValUnOp::EvalFun,
                M(id, |id, _, inner| {
                    ExprHead::Variadic(
                        ValVarOp::PartialApply(Origin::Array),
                        SmallVec::from(
                            &[
                                M(id, |_, _, _| ExprHead::Niladic(ResolvedAtom::Array)),
                                M(id, |_, _, _| ExprHead::Niladic(ResolvedAtom::Single)),
                                Cont(inner),
                            ][..],
                        ),
                    )
                }),
            ),
            (Cont(_), ExprHead::Dyadic(expr::ValBinOp::Array, _)) => ExprHead::Monadic(
                ValUnOp::EvalFun,
                D(id, |id, _, [lhs, rhs]| {
                    ExprHead::Variadic(
                        ValVarOp::PartialApply(Origin::Array),
                        SmallVec::from(
                            &[
                                D(id, |_, _, _| ExprHead::Niladic(ResolvedAtom::Array)),
                                Cont(lhs),
                                Cont(rhs),
                            ][..],
                        ),
                    )
                }),
            ),
            (Cont(_), ExprHead::Monadic(expr::ValUnOp::ArrayFill, _)) => ExprHead::Monadic(
                ValUnOp::EvalFun,
                M(id, |id, _, inner| {
                    ExprHead::Variadic(
                        ValVarOp::PartialApply(Origin::ArrayFill),
                        SmallVec::from(
                            &[
                                M(id, |_, _, _| ExprHead::Niladic(ResolvedAtom::ArrayFill)),
                                Cont(inner),
                            ][..],
                        ),
                    )
                }),
            ),
            (Cont(_), ExprHead::Niladic(ResolvedAtom::ArrayFill)) => ExprHead::Monadic(
                ValUnOp::EvalFun,
                N(id, |id, _| {
                    ExprHead::Variadic(
                        ValVarOp::PartialApply(Origin::ArrayFill),
                        SmallVec::from(
                            &[
                                N(id, |_, _| ExprHead::Niladic(ResolvedAtom::ArrayFill)),
                                N(id, |_, _| ExprHead::Niladic(ResolvedAtom::Single)),
                            ][..],
                        ),
                    )
                }),
            ),
            (Cont(_), ExprHead::Variadic(expr::ValVarOp::Call, _)) => ExprHead::Monadic(
                ValUnOp::EvalFun,
                V(id, |_, _, args| {
                    ExprHead::Variadic(
                        ValVarOp::PartialApply(Origin::User),
                        args.iter().copied().map(Cont).collect(),
                    )
                }),
            ),
            (Cont(id), ExprHead::Niladic(ResolvedAtom::Block(b, hir::BlockKind::Inline))) => {
                ExprHead::Monadic(ValUnOp::EvalFun, Block(id, *b, hir::BlockKind::Inline))
            }
            (Cont(_), otherwise) => otherwise
                .clone()
                .map_op(
                    |nil| nil,
                    |mon| mon.try_into().unwrap(),
                    |dya| dya.try_into().unwrap(),
                    |var| var.try_into().unwrap(),
                )
                .map_inner(Cont),
        };
        (desugared, *span)
    });
    Ok(ZipExpr {
        expr: Arc::new(desugared.expr),
        data: Arc::new(desugared.data),
    })
}
