use std::sync::Arc;

use crate::refs;
use hir::{ExprId, HirConstraintId};
use yaboc_ast::expr::{self, Atom, BtMarkKind, FieldAccessMode, WiggleKind};
use yaboc_base::error::SilencedError;
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
    Regex(Regex),
    Number(i64),
    Char(u32),
    Bool(bool),
    Single,
    Nil,
    Array,
    Block(hir::BlockId),
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
            expr::ValBinOp::Index => Err(expr::ValBinOp::Index),
            expr::ValBinOp::Compose => Err(expr::ValBinOp::Compose),
            expr::ValBinOp::At => Err(expr::ValBinOp::At),
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
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValVarOp {
    PartialApply,
}

impl TryFrom<expr::ValVarOp> for ValVarOp {
    type Error = expr::ValVarOp;

    fn try_from(value: expr::ValVarOp) -> Result<Self, Self::Error> {
        match value {
            expr::ValVarOp::PartialApply => Ok(ValVarOp::PartialApply),
            expr::ValVarOp::Call => Err(expr::ValVarOp::Call),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

fn resolve_expr_modules(
    db: &dyn Resolves,
    expr_id: ExprId,
    expr: DataRefExpr<hir::HirVal, SpanIndex>,
) -> Result<ReidxExpr<hir::HirVal, ResolvedButSugared>, ResolveError> {
    let parent_block = db.hir_parent_block(expr_id.0)?;

    let new_resolved_atom = |loc, name, span, use_core| {
        let (id, kind) = match refs::resolve_var_ref(db, loc, name, use_core)? {
            refs::Resolved::Value(id, kind) => (id, kind),
            refs::Resolved::Module(m) => return Ok(PartialEval::Eval(m)),
            refs::Resolved::Unresolved => {
                return Err(ResolveError::Unresolved(expr_id, span, name))
            }
        };
        Ok(PartialEval::Uneval(match kind {
            refs::VarType::ParserDef => ResolvedAtom::ParserDef(hir::ParserDefId(id)),
            refs::VarType::Value => {
                let is_captured = parent_block
                    .map(|x| !x.0.is_ancestor_of(db, id))
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
                    hir::ParserAtom::Nil => ResolvedAtom::Nil,
                    hir::ParserAtom::Array => ResolvedAtom::Array,
                    hir::ParserAtom::Regex(r) => ResolvedAtom::Regex(r),
                    hir::ParserAtom::Block(b) => ResolvedAtom::Block(b),
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
        M(Idx, fn(Idx, Idx) -> ExprHead<Resolved, Continue>),
        D(Idx, fn(Idx, [Idx; 2]) -> ExprHead<Resolved, Continue>),
        V(Idx, fn(Idx, &[Idx]) -> ExprHead<Resolved, Continue>),
    }
    use Continue::*;

    let expr = expr_id.lookup(db)?.expr;
    let resolved = resolve_expr_modules(db, expr_id, expr.take_ref())?;
    let sugar_spans = resolved.migrate_data(&expr.data);
    let zip_expr = resolved.into_expr().zip(sugar_spans);
    let core_items = db.core_items()?;
    let desugared = ZipExpr::new_from_unfold(Cont(zip_expr.root()), |cont| {
        let (Cont(id) | Item(id, _) | M(id, _) | D(id, _) | V(id, _)) = cont;
        let (head, span) = zip_expr.index_expr(id);
        let desugared = match (cont, head) {
            (Item(_, item), _) => ExprHead::Niladic(ResolvedAtom::ParserDef(core_items[item])),
            (M(_, f), ExprHead::Monadic(_, inner)) => f(id, *inner),
            (D(_, f), ExprHead::Dyadic(_, [lhs, rhs])) => f(id, [*lhs, *rhs]),
            (V(_, f), ExprHead::Variadic(_, inner)) => f(id, inner),
            (M(..) | D(..) | V(..), _) => unreachable!(),
            (Cont(_), ExprHead::Dyadic(expr::ValBinOp::Compose, _)) => ExprHead::Monadic(
                ValUnOp::EvalFun,
                D(id, |id, [lhs, rhs]| {
                    ExprHead::Variadic(
                        ValVarOp::PartialApply,
                        SmallVec::from(
                            &[Item(id, hir::CoreItem::Compose), Cont(lhs), Cont(rhs)][..],
                        ),
                    )
                }),
            ),
            (Cont(_), ExprHead::Dyadic(expr::ValBinOp::At, [lhs, _])) => ExprHead::Dyadic(
                ValBinOp::ParserApply,
                [
                    D(id, |_, [_, rhs]| {
                        ExprHead::Monadic(ValUnOp::GetAddr, Cont(rhs))
                    }),
                    Cont(*lhs),
                ],
            ),
            (Cont(_), ExprHead::Dyadic(expr::ValBinOp::Index, [lhs, _])) => ExprHead::Dyadic(
                ValBinOp::ParserApply,
                [
                    Cont(*lhs),
                    D(id, |id, _| {
                        ExprHead::Monadic(
                            ValUnOp::EvalFun,
                            D(id, |id, [_, rhs]| {
                                ExprHead::Variadic(
                                    ValVarOp::PartialApply,
                                    SmallVec::from(
                                        &[Item(id, hir::CoreItem::Index), Cont(rhs)][..],
                                    ),
                                )
                            }),
                        )
                    }),
                ],
            ),
            (Cont(_), ExprHead::Monadic(expr::ValUnOp::Array, inner)) => ExprHead::Variadic(
                ValVarOp::PartialApply,
                SmallVec::from(
                    &[
                        M(id, |_, _| ExprHead::Niladic(ResolvedAtom::Array)),
                        Cont(*inner),
                    ][..],
                ),
            ),
            (Cont(_), ExprHead::Variadic(expr::ValVarOp::Call, _)) => ExprHead::Monadic(
                ValUnOp::EvalFun,
                V(id, |_, args| {
                    ExprHead::Variadic(
                        ValVarOp::PartialApply,
                        args.iter().copied().map(Cont).collect(),
                    )
                }),
            ),
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
