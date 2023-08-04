use std::sync::Arc;

use super::Constraints;
use fxhash::FxHashMap;
use hir::HirIdWrapper;
use yaboc_ast::expr::{ValBinOp, ValUnOp, ValVarOp};
use yaboc_base::{
    error::{SResult, Silencable, SilencedError},
    interner::DefId,
};
use yaboc_dependents::{BlockSerialization, SubValue, SubValueKind};
use yaboc_expr::{ExprHead, ExprIdx, Expression, FetchExpr, TakeRef};
use yaboc_hir as hir;
use yaboc_hir_types::FullTypeId;
use yaboc_len::{SizeExpr, Term};
use yaboc_resolve::expr::{Resolved, ResolvedAtom};
use yaboc_types::{NominalKind, NominalTypeHead, Type, TypeId};

pub struct SizeTermBuilder<'a> {
    db: &'a dyn Constraints,
    terms: SizeExpr<hir::ParserDefId>,
    call_arities: Vec<usize>,
    term_spans: Vec<Origin>,
    vals: FxHashMap<SubValue, usize>,
    block_locs: FxHashMap<hir::BlockId, usize>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Origin {
    Expr(hir::ExprId, ExprIdx<Resolved>),
    Node(DefId),
}

fn is_definite(db: &(impl Constraints + ?Sized), ty: TypeId) -> SResult<bool> {
    Ok(matches!(
        db.lookup_intern_type(db.least_deref_type(ty)?),
        Type::Primitive(_)
            | Type::Nominal(NominalTypeHead {
                kind: NominalKind::Block,
                ..
            })
            | Type::Loop(..)
    ))
}

impl<'a> SizeTermBuilder<'a> {
    pub fn new(db: &'a dyn Constraints) -> Self {
        Self {
            db,
            terms: SizeExpr::new(),
            call_arities: Vec::new(),
            term_spans: Vec::new(),
            vals: FxHashMap::default(),
            block_locs: FxHashMap::default(),
        }
    }

    fn push_term(&mut self, term: Term<hir::ParserDefId>, definite: bool, span: Origin) -> usize {
        let index = self.terms.push(term, definite);
        self.term_spans.push(span);
        self.call_arities.push(0);
        index
    }

    fn create_block(&mut self, bid: hir::BlockId) -> SResult<usize> {
        let ser: BlockSerialization = self.db.block_serialization(bid).silence()?;
        let mut whole_result = None;
        for val_loc in ser.eval_order.iter() {
            let loc = Origin::Node(val_loc.val.id);
            let is_definite = || -> SResult<bool> {
                let ty = self.db.parser_type_at(val_loc.val.id)?;
                is_definite(self.db, ty)
            };
            let val = match (self.db.hir_node(val_loc.val.id)?, val_loc.val.kind) {
                (_, SubValueKind::Bt) => continue,
                (hir::HirNode::Let(l), _) => {
                    let old_val = self.vals[&SubValue::new_val(l.expr.0)];
                    self.push_term(Term::Copy(old_val), is_definite()?, loc)
                }
                (hir::HirNode::Expr(e), _) => self.create_expr(e.id)?,
                (
                    hir::HirNode::Parse(hir::ParseStatement { front: pred, .. })
                    | hir::HirNode::Choice(hir::StructChoice {
                        endpoints: Some([pred, _]),
                        ..
                    }),
                    SubValueKind::Front,
                ) => match pred {
                    hir::ParserPredecessor::ChildOf(id) => {
                        self.push_term(Term::Const(0), true, Origin::Node(id.0))
                    }
                    hir::ParserPredecessor::After(id) => self.vals[&SubValue::new_back(id)],
                },
                (hir::HirNode::Parse(_), SubValueKind::Val) => {
                    self.push_term(Term::Opaque, is_definite()?, loc)
                }
                (hir::HirNode::Parse(p), SubValueKind::Back) => {
                    let front = self.vals[&SubValue::new_front(p.id.0)];
                    let len = self.vals[&SubValue::new_val(p.expr.0)];
                    self.push_term(Term::Add([front, len]), true, loc)
                }
                (hir::HirNode::Block(_), SubValueKind::Front) => {
                    self.push_term(Term::Const(0), true, loc)
                }
                (hir::HirNode::Block(_), SubValueKind::Val) => {
                    self.push_term(Term::Opaque, true, loc)
                }
                (hir::HirNode::Block(b), SubValueKind::Back) => {
                    let x = if let Some((_, last)) = b.root_context.lookup(self.db)?.endpoints {
                        let old_val = self.vals[&SubValue::new_back(last)];
                        self.push_term(Term::Copy(old_val), true, loc)
                    } else {
                        self.push_term(Term::Const(0), true, loc)
                    };
                    whole_result = Some(x);
                    x
                }
                (hir::HirNode::ChoiceIndirection(c), SubValueKind::Val) => {
                    let mut current_term = self.vals[&SubValue::new_val(c.choices[0].1)];
                    for &(_, id) in c.choices.iter().skip(1) {
                        let term = self.vals[&SubValue::new_val(id)];
                        let loc = Origin::Node(id);
                        current_term = self.push_term(
                            Term::UnifyDyn([current_term, term]),
                            is_definite()?,
                            loc,
                        );
                    }
                    current_term
                }
                (hir::HirNode::Choice(c), SubValueKind::Back) => {
                    let mut current_term = None;
                    let front = self.vals[&SubValue::new_front(c.id.0)];
                    for subctx in c.subcontexts.iter() {
                        let endpoints = subctx.lookup(self.db)?.endpoints;
                        let loc = Origin::Node(subctx.0);
                        let term = if let Some((_, end)) = endpoints {
                            let inner_len = self.vals[&SubValue::new_back(end)];
                            self.push_term(Term::Add([front, inner_len]), true, loc)
                        } else {
                            self.push_term(Term::Copy(front), true, loc)
                        };
                        current_term = match current_term {
                            // since we can only evaluate a choice without arg by evaluating
                            // every branch, we don't have a definite value, hence the "false"
                            Some(t) => Some(self.push_term(Term::UnifyDyn([t, term]), false, loc)),
                            None => Some(term),
                        };
                    }
                    current_term.expect("choice should have at least one subcontext")
                }
                _ => continue,
            };
            self.vals.insert(val_loc.val, val);
        }
        let res = whole_result.expect("block is never terminated");
        self.block_locs.insert(bid, res);
        Ok(res)
    }

    fn create_expr(&mut self, expr_id: hir::ExprId) -> SResult<usize> {
        let expr = Resolved::expr_with_data::<(ExprIdx<Resolved>, FullTypeId)>(self.db, expr_id)?;
        let mapped = expr
            .take_ref()
            .map(|(idx, ty)| (Origin::Expr(expr_id, idx), ty));
        mapped.try_fold(|(head, (src, ty))| match head {
            ExprHead::Niladic(n) => match n {
                ResolvedAtom::Val(id, _) | ResolvedAtom::Captured(id, _) => {
                    let referencing = self.vals[&SubValue::new_val(id)];
                    Ok(self.push_term(Term::Copy(referencing), is_definite(self.db, *ty)?, src))
                }
                ResolvedAtom::ParserDef(pd, _) => {
                    Ok(self.push_term(Term::Pd(pd), is_definite(self.db, *ty)?, src))
                }
                ResolvedAtom::Regex(r, _) => {
                    let len = self.db.regex_len(r).map_err(|_| SilencedError::new())?;
                    if let Some(len) = len {
                        Ok(self.push_term(Term::Const(len), false, src))
                    } else {
                        Ok(self.push_term(Term::Opaque, false, src))
                    }
                }
                ResolvedAtom::Number(n) => Ok(self.push_term(Term::Const(n.into()), true, src)),
                ResolvedAtom::Char(c) => Ok(self.push_term(Term::Const(c.into()), true, src)),
                ResolvedAtom::Bool(b) => Ok(self.push_term(Term::Const(b.into()), true, src)),
                ResolvedAtom::Single => Ok(self.push_term(Term::Const(1), false, src)),
                ResolvedAtom::Nil => Ok(self.push_term(Term::Const(0), false, src)),
                ResolvedAtom::Array => Ok(self.push_term(Term::Arr, false, src)),
                ResolvedAtom::Block(bid) => self.create_block(bid),
            },
            ExprHead::Monadic(m, inner) => match m {
                ValUnOp::Neg => Ok(self.push_term(Term::Neg(inner), true, src)),
                ValUnOp::Wiggle(_, _) => Ok(inner),
                ValUnOp::Size => Ok(self.push_term(Term::Size(inner), true, src)),
                ValUnOp::Array => unreachable!(),
                ValUnOp::Dot(_, _) => {
                    Ok(self.push_term(Term::OpaqueUn(inner), is_definite(self.db, *ty)?, src))
                }
                ValUnOp::Not => Ok(self.push_term(Term::OpaqueUn(inner), true, src)),
            },
            ExprHead::Dyadic(d, [lhs, rhs]) => match d {
                ValBinOp::Mul => Ok(self.push_term(Term::Mul([lhs, rhs]), true, src)),
                ValBinOp::Plus => Ok(self.push_term(Term::Add([lhs, rhs]), true, src)),
                ValBinOp::Minus => {
                    let rhs = self.push_term(Term::Neg(rhs), true, src);
                    Ok(self.push_term(Term::Add([lhs, rhs]), true, src))
                }
                ValBinOp::Else => {
                    Ok(self.push_term(Term::Unify([lhs, rhs]), is_definite(self.db, *ty)?, src))
                }
                ValBinOp::Then => Ok(rhs),
                ValBinOp::Compose | ValBinOp::Index => unreachable!(),
                ValBinOp::And
                | ValBinOp::Xor
                | ValBinOp::Or
                | ValBinOp::LesserEq
                | ValBinOp::Lesser
                | ValBinOp::GreaterEq
                | ValBinOp::Greater
                | ValBinOp::Uneq
                | ValBinOp::Equals
                | ValBinOp::ShiftR
                | ValBinOp::ShiftL
                | ValBinOp::Div
                | ValBinOp::Modulo
                | ValBinOp::ParserApply => Ok(self.push_term(
                    Term::OpaqueBin([lhs, rhs]),
                    is_definite(self.db, *ty)?,
                    src,
                )),
            },
            ExprHead::Variadic(ValVarOp::Call, args) => {
                let f = args[0];
                let mut ret = f;
                for arg in args[1..].iter() {
                    ret = self.push_term(Term::Apply([ret, *arg]), false, src);
                }
                *self.call_arities.last_mut().unwrap() = args.len() - 1;
                Ok(ret)
            }
        })
    }

    fn create_pd(&mut self, pd: hir::ParserDefId) -> SResult<usize> {
        let parserdef = pd.lookup(self.db)?;
        for (arg_idx, arg) in parserdef
            .args
            .into_iter()
            .flat_map(IntoIterator::into_iter)
            .enumerate()
        {
            let loc = Origin::Node(arg.0);
            let arg_ty = self.db.parser_type_at(arg.0)?;
            let term = self.push_term(
                Term::Arg(arg_idx.try_into().unwrap()),
                is_definite(self.db, arg_ty)?,
                loc,
            );
            let val_loc = SubValue::new_val(arg.0);
            self.vals.insert(val_loc, term);
        }
        self.create_expr(parserdef.to)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct PdLenTerm {
    pub expr: SizeExpr<hir::ParserDefId>,
    pub call_arities: Vec<usize>,
    pub term_spans: Vec<Origin>,
    pub vals: Arc<FxHashMap<SubValue, usize>>,
    pub root: usize,
    pub block_locs: FxHashMap<hir::BlockId, usize>,
}

pub fn len_term(db: &dyn Constraints, pd: hir::ParserDefId) -> SResult<Arc<PdLenTerm>> {
    let mut ctx = SizeTermBuilder::new(db);
    let root = ctx.create_pd(pd)?;
    let call_arities = ctx.call_arities;
    let expr = ctx.terms;
    let term_spans = ctx.term_spans;
    let block_locs = ctx.block_locs;
    let vals = Arc::new(ctx.vals);
    Ok(Arc::new(PdLenTerm {
        expr,
        root,
        call_arities,
        vals,
        term_spans,
        block_locs,
    }))
}
