use std::sync::Arc;

use crate::refs;
use yaboc_ast::expr::ExpressionKind;
use yaboc_ast::expr::{
    Atom, Expression, ExpressionHead, KindWithData, OpWithData, ValBinOp, ValUnOp, ValVarOp,
};
use yaboc_base::interner::DefId;
use yaboc_base::source::SpanIndex;
use yaboc_hir as hir;
use yaboc_hir::HirIdWrapper;

use super::{ResolveError, Resolves};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedAtom {
    Val(DefId, bool),
    Captured(DefId, bool),
    ParserDef(hir::ParserDefId, bool),
    Number(i64),
    Char(u32),
    Bool(bool),
    Single,
    Nil,
    Block(hir::BlockId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ResolvedKind;

impl ExpressionKind for ResolvedKind {
    type NiladicOp = ResolvedAtom;
    type MonadicOp = ValUnOp<Arc<Expression<hir::HirConstraintSpanned>>>;
    type DyadicOp = ValBinOp;
    type VariadicOp = ValVarOp;
}

pub type ResolvedExpr = Expression<KindWithData<ResolvedKind, SpanIndex>>;

pub fn resolve_expr_error(
    db: &dyn Resolves,
    expr_id: hir::ExprId,
) -> Result<Arc<ResolvedExpr>, ResolveError> {
    let expr = expr_id.lookup(db)?.expr;
    let parent_block = db.hir_parent_block(expr_id.0)?;
    let new_resolved_atom = |loc, name, span| {
        let (id, kind) = match refs::resolve_var_ref(db, loc, name)? {
            refs::Resolved::Value(id, kind) => (id, kind),
            refs::Resolved::Module(m) => return Ok(Err(m)),
            refs::Resolved::Unresolved => {
                return Err(ResolveError::Unresolved(expr_id, span, name))
            }
        };
        Ok(Ok(match kind {
            refs::VarType::ParserDef => ResolvedAtom::ParserDef(hir::ParserDefId(id), true),
            refs::VarType::Value => {
                let is_captured = parent_block
                    .map(|x| !x.0.is_ancestor_of(db, id))
                    .unwrap_or(false);
                let is_arg = db.hir_node(id)?.is_kind(hir::HirNodeKind::ArgDef.into());
                if is_captured || is_arg {
                    ResolvedAtom::Captured(id, true)
                } else {
                    ResolvedAtom::Val(id, true)
                }
            }
        }))
    };
    let root_span = *expr.0.root_data();
    let expr = expr.try_fold(&mut |head| -> Result<
        Result<ResolvedExpr, hir::ModuleId>,
        ResolveError,
    > {
        match &head {
            ExpressionHead::Niladic(nil) => {
                let new = match &nil.inner {
                    hir::ParserAtom::Atom(Atom::Number(s)) => ResolvedAtom::Number(*s),
                    hir::ParserAtom::Atom(Atom::Char(s)) => ResolvedAtom::Char(*s),
                    hir::ParserAtom::Atom(Atom::Bool(s)) => ResolvedAtom::Bool(*s),
                    hir::ParserAtom::Single => ResolvedAtom::Single,
                    hir::ParserAtom::Nil => ResolvedAtom::Nil,
                    hir::ParserAtom::Block(b) => ResolvedAtom::Block(*b),
                    hir::ParserAtom::Atom(Atom::Field(f)) => {
                        match new_resolved_atom(expr_id.0, *f, nil.data)? {
                            Ok(k) => k,
                            Err(m) => return Ok(Err(m)),
                        }
                    }
                };
                return Ok(Ok(Expression::new_niladic(OpWithData {
                    data: nil.data,
                    inner: new,
                })));
            }
            ExpressionHead::Monadic(m) => {
                if let ValUnOp::Dot(name) = &m.op.inner {
                    if let Err(module) = m.inner {
                        match new_resolved_atom(module.0, *name, m.op.data)? {
                            Ok(new) => {
                                return Ok(Ok(Expression::new_niladic(OpWithData {
                                    data: m.op.data,
                                    inner: new,
                                })))
                            }
                            Err(m) => return Ok(Err(m)),
                        };
                    };
                }
            }
            _ => {}
        };
        let root_span = *head.root_data();
        let r = head.try_map_inner(|f| {
            f.map_err(|_| ResolveError::ModuleInExpression(expr_id, root_span))
        })?;
        match r {
            ExpressionHead::Niladic(_) => unreachable!(),
            ExpressionHead::Monadic(m) => Ok(Ok(Expression::new_monadic(
                OpWithData {
                    data: m.op.data,
                    inner: m.op.inner,
                },
                m.inner,
            ))),
            ExpressionHead::Dyadic(d) => Ok(Ok(Expression::new_dyadic(
                OpWithData {
                    data: d.op.data,
                    inner: d.op.inner,
                },
                d.inner,
            ))),
            ExpressionHead::Variadic(v) => Ok(Ok(Expression::new_variadic(
                OpWithData {
                    data: v.op.data,
                    inner: v.op.inner,
                },
                v.inner,
            ))),
        }
    })?;
    match expr {
        Ok(x) => Ok(Arc::new(x)),
        Err(_) => Err(ResolveError::ModuleInExpression(expr_id, root_span)),
    }
}
