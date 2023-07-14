use std::sync::Arc;

use crate::refs;
use hir::{ExprId, HirConstraintId};
use yaboc_ast::expr::{Atom, ValBinOp, ValUnOp, ValVarOp};
use yaboc_base::error::SilencedError;
use yaboc_base::interner::{DefId, Regex};
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
    Val(DefId, bool),
    Captured(DefId, bool),
    ParserDef(hir::ParserDefId, bool),
    Regex(Regex, bool),
    Number(i64),
    Char(u32),
    Bool(bool),
    Single,
    Nil,
    Array,
    Block(hir::BlockId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Resolved;

impl ExprKind for Resolved {
    type NiladicOp = ResolvedAtom;
    type MonadicOp = ValUnOp<HirConstraintId>;
    type DyadicOp = ValBinOp;
    type VariadicOp = ValVarOp;
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
) -> Result<ReidxExpr<hir::HirVal, Resolved>, ResolveError> {
    let parent_block = db.hir_parent_block(expr_id.0)?;

    let new_resolved_atom = |loc, name, bt, span| {
        let (id, kind) = match refs::resolve_var_ref(db, loc, name)? {
            refs::Resolved::Value(id, kind) => (id, kind),
            refs::Resolved::Module(m) => return Ok(PartialEval::Eval(m)),
            refs::Resolved::Unresolved => {
                return Err(ResolveError::Unresolved(expr_id, span, name))
            }
        };
        Ok(PartialEval::Uneval(match kind {
            refs::VarType::ParserDef => ResolvedAtom::ParserDef(hir::ParserDefId(id), bt),
            refs::VarType::Value => {
                let is_captured = parent_block
                    .map(|x| !x.0.is_ancestor_of(db, id))
                    .unwrap_or(false);
                let is_arg = db.hir_node(id)?.is_kind(hir::HirNodeKind::ArgDef.into());
                if is_captured || is_arg {
                    ResolvedAtom::Captured(id, bt)
                } else {
                    ResolvedAtom::Val(id, bt)
                }
            }
        }))
    };

    expr.try_partial_eval::<hir::ModuleId, Resolved, _>(
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
                    hir::ParserAtom::Regex(r, bt) => ResolvedAtom::Regex(r, bt),
                    hir::ParserAtom::Block(b) => ResolvedAtom::Block(b),
                    hir::ParserAtom::Atom(Atom::Field((f, bt))) => {
                        match new_resolved_atom(expr_id.0, f, bt, *span)? {
                            PartialEval::Uneval(k) => k,
                            PartialEval::Eval(m) => return Ok(PartialEval::Eval(m)),
                        }
                    }
                }),
                ExprHead::Monadic(ValUnOp::Dot(field, bt), PartialEval::Eval((m, _))) => {
                    match new_resolved_atom(m.0, field, bt, *span)? {
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
    #[derive(Clone, Copy)]
    enum DesugarContinue {
        Cont(ExprIdx<Resolved>),
        Compose(SpanIndex),
        Array(SpanIndex),
    }
    use DesugarContinue::*;

    let expr = expr_id.lookup(db)?.expr;
    let resolved = resolve_expr_modules(db, expr_id, expr.take_ref())?;
    let sugar_spans = resolved.migrate_data(&expr.data);
    let zip_expr = resolved.into_expr().zip(sugar_spans);
    let compose = ExprHead::Niladic(ResolvedAtom::ParserDef(
        db.std_item(hir::StdItem::Compose)?,
        true,
    ));
    let array = ExprHead::Niladic(ResolvedAtom::Array);
    let desugared = ZipExpr::new_from_unfold(Cont(zip_expr.root()), |id| {
        let id = match id {
            Cont(id) => id,
            Compose(span) => return (compose.clone(), span),
            Array(span) => return (array.clone(), span),
        };
        let (head, span) = zip_expr.index_expr(id);
        if let ExprHead::Dyadic(ValBinOp::Compose, [lhs, rhs]) = head {
            return (
                ExprHead::Variadic(
                    ValVarOp::Call,
                    SmallVec::from_slice(&[Compose(*span), Cont(*lhs), Cont(*rhs)]),
                ),
                *span,
            );
        } else if let ExprHead::Monadic(ValUnOp::Array, inner) = head {
            return (
                ExprHead::Variadic(
                    ValVarOp::Call,
                    SmallVec::from_slice(&[Array(*span), Cont(*inner)]),
                ),
                *span,
            );
        }
        (head.clone().map_inner(Cont), *span)
    });
    Ok(ZipExpr {
        expr: Arc::new(desugared.expr),
        data: Arc::new(desugared.data),
    })
}
