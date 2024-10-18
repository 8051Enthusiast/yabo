use std::sync::Arc;

use crate::{arg_ranks, lambda_arg_ranks, Origin};

use super::Constraints;
use fxhash::FxHashMap;
use hir::HirIdWrapper;
use yaboc_ast::expr::{FieldAccessMode, WiggleKind};
use yaboc_base::{
    error::{SResult, Silencable},
    interner::FieldName,
};
use yaboc_dependents::{
    requirements::ExprDepData, BacktrackStatus, BlockSerialization, SubValue, SubValueKind,
};
use yaboc_expr::{ExprHead, ExprIdx, Expression, FetchExpr, ShapedData, SmallVec, TakeRef};
use yaboc_hir::{self as hir, BlockReturnKind};
use yaboc_hir_types::FullTypeId;
use yaboc_len::{depvec::SmallBitVec, ArgRank, ScopeInfo, ScopeInfoIdx, ScopeKind, SizeExpr, Term};
use yaboc_req::NeededBy;
use yaboc_resolve::expr::{Resolved, ResolvedAtom};
use yaboc_resolve::expr::{ValBinOp, ValUnOp, ValVarOp};
use yaboc_types::Type;

pub struct SizeTermBuilder<'a> {
    db: &'a dyn Constraints,
    terms: SizeExpr<hir::ParserDefId>,
    term_spans: Vec<Origin>,
    vals: FxHashMap<SubValue, usize>,
    block_locs: FxHashMap<hir::BlockId, usize>,
    expr_locs: FxHashMap<hir::ExprId, ShapedData<Vec<usize>, Resolved>>,
    arg_depth: u32,
}

impl<'a> SizeTermBuilder<'a> {
    pub fn new(db: &'a dyn Constraints) -> Self {
        Self {
            db,
            terms: SizeExpr::new(),
            term_spans: Vec::new(),
            vals: FxHashMap::default(),
            block_locs: FxHashMap::default(),
            expr_locs: FxHashMap::default(),
            arg_depth: 0,
        }
    }

    fn push_term(&mut self, term: Term<hir::ParserDefId>, span: Origin) -> usize {
        let index = self.terms.push(term, self.arg_depth);
        self.term_spans.push(span);
        index
    }

    fn new_block(&mut self, bid: hir::BlockId) -> SResult<ScopeInfoIdx> {
        let mut captures = SmallBitVec::default();
        for id in self.db.captures(bid).iter() {
            let val = self.vals[&SubValue::new_val(*id)];
            captures.set(val);
        }
        let kind = match bid.lookup(self.db)?.kind {
            hir::BlockKind::Inline => ScopeKind::AllValUse,
            hir::BlockKind::Parser => ScopeKind::PartialLenUse,
        };
        let block_info = ScopeInfo {
            captures,
            kind,
            params: SmallVec::from_slice(&[ArgRank(0)]),
        };
        let idx = ScopeInfoIdx::new(self.terms.scope_info.len() as u32);
        self.terms.scope_info.push(block_info);
        Ok(idx)
    }

    fn create_block(&mut self, bid: hir::BlockId) -> SResult<usize> {
        let ser: BlockSerialization = self.db.block_serialization(bid).silence()?;
        let mut whole_result = None;
        let idx = self.new_block(bid)?;
        self.arg_depth += 1;
        self.push_term(Term::ScopeIntro(idx), Origin::Node(bid.0));
        for val_loc in ser.eval_order.iter() {
            let loc = Origin::Node(val_loc.val.id);
            let val = match (self.db.hir_node(val_loc.val.id)?, val_loc.val.kind) {
                (_, SubValueKind::Bt) => continue,
                (hir::HirNode::Let(l), _) => {
                    let old_val = self.vals[&SubValue::new_val(l.expr.0)];
                    self.push_term(Term::Copy(old_val), loc)
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
                        self.push_term(Term::Const(0), Origin::Node(id.0))
                    }
                    hir::ParserPredecessor::After(id) => self.vals[&SubValue::new_back(id)],
                },
                (hir::HirNode::Parse(_), SubValueKind::Val) => self.push_term(Term::Parsed, loc),
                (hir::HirNode::Parse(p), SubValueKind::Back) => {
                    let front = self.vals[&SubValue::new_front(p.id.0)];
                    let parser = self.vals[&SubValue::new_val(p.expr.0)];
                    self.push_term(Term::Cat([front, parser]), loc)
                }
                (hir::HirNode::Block(_), SubValueKind::Front) => {
                    self.push_term(Term::Const(0), loc)
                }
                (hir::HirNode::Block(b), SubValueKind::Val) => {
                    let val = if matches!(b.returns, BlockReturnKind::Returns) {
                        let ctx = b.root_context.lookup(self.db)?;
                        let ret = *ctx.vars.get(FieldName::Return).unwrap().inner();
                        self.vals[&SubValue::new_val(ret)]
                    } else {
                        self.push_term(Term::Opaque, loc)
                    };
                    if b.kind == hir::BlockKind::Inline {
                        whole_result = Some((val, loc));
                    }
                    val
                }
                (hir::HirNode::Block(b), SubValueKind::Back) => {
                    let x = if let Some((_, last)) = b.root_context.lookup(self.db)?.endpoints {
                        self.vals[&SubValue::new_back(last)]
                    } else {
                        self.push_term(Term::Const(0), loc)
                    };
                    whole_result = Some((x, loc));
                    x
                }
                (hir::HirNode::ChoiceIndirection(_), SubValueKind::Val) => {
                    // TODO: lower choice indirections in mir so that this can be lowered
                    // as UnifyDyn instead
                    self.push_term(Term::Parsed, loc)
                }
                (hir::HirNode::Choice(c), SubValueKind::Back) => {
                    let mut current_term = None;
                    let front = self.vals[&SubValue::new_front(c.id.0)];
                    for subctx in c.subcontexts.iter() {
                        let endpoints = subctx.lookup(self.db)?.endpoints;
                        let loc = Origin::Node(subctx.0);
                        let term = if let Some((_, end)) = endpoints {
                            let inner_len = self.vals[&SubValue::new_back(end)];
                            self.push_term(Term::Cat([front, inner_len]), loc)
                        } else {
                            self.push_term(Term::Copy(front), loc)
                        };
                        current_term = match current_term {
                            Some(t) => Some(self.push_term(Term::UnifyDyn([t, term]), loc)),
                            None => Some(term),
                        };
                    }
                    current_term.expect("choice should have at least one subcontext")
                }
                _ => continue,
            };
            self.vals.insert(val_loc.val, val);
        }
        let (res, loc) = whole_result.expect("block is never terminated");
        self.arg_depth -= 1;
        let res = self.push_term(Term::BlockEnd(idx, res), loc);
        self.block_locs.insert(bid, res);
        Ok(res)
    }

    fn new_lambda(&mut self, lambda_id: hir::LambdaId) -> SResult<ScopeInfoIdx> {
        let mut captures = SmallBitVec::default();
        for id in self.db.lambda_captures(lambda_id).iter() {
            let val = self.vals[&SubValue::new_val(*id)];
            captures.set(val);
        }
        let kind = ScopeKind::AllValUse;
        let params = lambda_arg_ranks(self.db, lambda_id)?;
        let block_info = ScopeInfo {
            captures,
            kind,
            params,
        };
        let idx = ScopeInfoIdx::new(self.terms.scope_info.len() as u32);
        self.terms.scope_info.push(block_info);
        Ok(idx)
    }

    fn create_lambda(&mut self, lambda_id: hir::LambdaId) -> SResult<usize> {
        let lambda = lambda_id.lookup(self.db)?;
        let old_depth = self.arg_depth;
        self.arg_depth += lambda.args.len() as u32;
        let idx = self.new_lambda(lambda_id)?;
        self.push_term(Term::ScopeIntro(idx), Origin::Node(lambda_id.0));
        for (arg_idx, arg) in lambda.args.iter().enumerate() {
            let arg_idx = old_depth + arg_idx as u32;
            let loc = Origin::Node(arg.0);
            let term = self.push_term(Term::Arg(arg_idx), loc);
            let val_loc = SubValue::new_val(arg.0);
            self.vals.insert(val_loc, term);
        }
        let res = self.create_expr(lambda.expr)?;
        self.arg_depth -= lambda.args.len() as u32;
        let lam = self.push_term(Term::FunctionEnd(idx, res), Origin::Node(lambda_id.0));
        Ok(lam)
    }

    fn create_expr(&mut self, expr_id: hir::ExprId) -> SResult<usize> {
        let expr = Resolved::expr_with_data::<(
            (ExprIdx<Resolved>, FullTypeId),
            (ExprDepData, BacktrackStatus),
        )>(self.db, expr_id)?;
        let mapped = expr.take_ref().map(|((idx, ty), (dep, bt))| {
            (
                Origin::Expr(expr_id, idx),
                *ty,
                dep.reqs,
                bt.can_backtrack(),
            )
        });
        let locs = mapped
            .try_scan(|(head, (src, ty, dep, bt))| -> SResult<_> {
                let needs_bt = (dep * !!NeededBy::Val).contains(NeededBy::Backtrack);
                let ret = match head {
                    ExprHead::Niladic(n) => match n {
                        ResolvedAtom::Val(id) | ResolvedAtom::Captured(id) => {
                            let referencing = self.vals[&SubValue::new_val(id)];
                            self.push_term(Term::Copy(referencing), src)
                        }
                        ResolvedAtom::ParserDef(pd) | ResolvedAtom::Global(pd) => {
                            self.push_term(Term::Pd(pd), src)
                        }
                        ResolvedAtom::Regex(r) => {
                            let len = self.db.regex_len(r)?;
                            if let Some(len) = len {
                                self.push_term(Term::Const(len), src)
                            } else {
                                self.push_term(Term::Opaque, src)
                            }
                        }
                        ResolvedAtom::ArrayFill => self.push_term(Term::Opaque, src),
                        ResolvedAtom::Number(n) => self.push_term(Term::Const(n.into()), src),
                        ResolvedAtom::Char(c) => self.push_term(Term::Const(c.into()), src),
                        ResolvedAtom::Bool(b) => self.push_term(Term::Const(b.into()), src),
                        ResolvedAtom::Single => self.push_term(Term::Const(1), src),
                        ResolvedAtom::Nil => self.push_term(Term::Const(0), src),
                        ResolvedAtom::Array => self.push_term(Term::Arr, src),
                        ResolvedAtom::Span(..) => self.push_term(Term::Span, src),
                        ResolvedAtom::Block(bid, _) => self.create_block(bid)?,
                        ResolvedAtom::Lambda(lam) => self.create_lambda(lam)?,
                    },
                    ExprHead::Monadic(m, &(inner, inner_bt, inner_ty)) => match m {
                        ValUnOp::Neg => self.push_term(Term::Neg(inner), src),
                        ValUnOp::Wiggle(_, WiggleKind::Is) if needs_bt => {
                            self.push_term(Term::Backtracking(inner), src)
                        }
                        ValUnOp::EvalFun if inner_bt && needs_bt => {
                            self.push_term(Term::Backtracking(inner), src)
                        }
                        ValUnOp::Wiggle(_, _) | ValUnOp::BtMark(_) | ValUnOp::EvalFun => inner,
                        ValUnOp::Size => {
                            let ldt = self
                                .db
                                .lookup_intern_type(self.db.least_deref_type(inner_ty)?);
                            match ldt {
                                Type::ParserArg { .. } => {
                                    self.push_term(Term::Size(true, inner), src)
                                }
                                Type::Loop(..) => {
                                    // for arrays we handle .sizeof as an opaque op
                                    self.push_term(Term::OpaqueUn(inner), src)
                                }
                                otherwise => {
                                    panic!("unexpected type for .sizeof: {:?}", &otherwise)
                                }
                            }
                        }
                        ValUnOp::Dot(_, FieldAccessMode::Backtrack) if needs_bt => {
                            let res = self.push_term(Term::OpaqueUn(inner), src);
                            self.push_term(Term::Backtracking(res), src)
                        }
                        ValUnOp::Dot(..) => self.push_term(Term::OpaqueUn(inner), src),
                        ValUnOp::Not => self.push_term(Term::OpaqueUn(inner), src),
                        ValUnOp::GetAddr => self.push_term(Term::OpaqueUn(inner), src),
                    },
                    ExprHead::Dyadic(d, [&(lhs, _, _), &(rhs, rhs_bt, _)]) => match d {
                        ValBinOp::Mul => self.push_term(Term::Mul([lhs, rhs]), src),
                        ValBinOp::Plus => self.push_term(Term::Add([lhs, rhs]), src),
                        ValBinOp::Minus => {
                            let rhs = self.push_term(Term::Neg(rhs), src);
                            self.push_term(Term::Add([lhs, rhs]), src)
                        }
                        ValBinOp::Else => self.push_term(Term::Unify([lhs, rhs]), src),
                        ValBinOp::Then => self.push_term(Term::Then([lhs, rhs]), src),
                        ValBinOp::ParserApply if rhs_bt && needs_bt => {
                            let res = self.push_term(Term::OpaqueBin([lhs, rhs]), src);
                            self.push_term(Term::Backtracking(res), src)
                        }
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
                        | ValBinOp::Range
                        | ValBinOp::ParserApply => self.push_term(Term::OpaqueBin([lhs, rhs]), src),
                    },
                    ExprHead::Variadic(ValVarOp::PartialApply(_), args) => {
                        let (f, ..) = args[0];
                        let mut ret = *f;
                        for (arg, ..) in args[1..].iter() {
                            ret = self.push_term(Term::Apply([ret, *arg]), src);
                        }
                        ret
                    }
                };
                Ok((ret, bt, ty))
            })?
            .map(|(x, ..)| x)
            .collect();
        let root_loc = *locs.root_data();
        self.expr_locs.insert(expr_id, locs);
        Ok(root_loc)
    }

    fn create_pd(&mut self, pd: hir::ParserDefId) -> SResult<usize> {
        let parserdef = pd.lookup(self.db)?;
        let kind = if parserdef.from.is_some() {
            ScopeKind::PartialLenUse
        } else {
            ScopeKind::AllValUse
        };
        let params = arg_ranks(self.db, parserdef.id)?;
        let param_count = params.len() as u32;
        let idx = ScopeInfoIdx::new(self.terms.scope_info.len() as u32);
        self.terms.scope_info.push(ScopeInfo {
            captures: SmallBitVec::default(),
            kind,
            params,
        });
        self.arg_depth += param_count;
        self.push_term(Term::ScopeIntro(idx), Origin::Node(pd.0));
        for (arg_idx, arg) in parserdef
            .args
            .into_iter()
            .flat_map(IntoIterator::into_iter)
            .enumerate()
        {
            let loc = Origin::Node(arg.0);
            let term = self.push_term(Term::Arg(arg_idx.try_into().unwrap()), loc);
            let val_loc = SubValue::new_val(arg.0);
            self.vals.insert(val_loc, term);
        }
        let res = self.create_expr(parserdef.to)?;
        self.arg_depth -= param_count;
        Ok(self.push_term(Term::FunctionEnd(idx, res), Origin::Node(pd.0)))
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct PdLenTerm {
    pub expr: SizeExpr<hir::ParserDefId>,
    pub term_spans: Vec<Origin>,
    pub vals: Arc<FxHashMap<SubValue, usize>>,
    pub expr_locs: Arc<FxHashMap<hir::ExprId, ShapedData<Vec<usize>, Resolved>>>,
    pub root: usize,
    pub block_locs: FxHashMap<hir::BlockId, usize>,
    pub is_val_fun: bool,
}

pub fn len_term(db: &dyn Constraints, pd: hir::ParserDefId) -> SResult<Arc<PdLenTerm>> {
    let mut ctx = SizeTermBuilder::new(db);
    let root = ctx.create_pd(pd)?;
    let expr = ctx.terms;
    let term_spans = ctx.term_spans;
    let block_locs = ctx.block_locs;
    let expr_locs = Arc::new(ctx.expr_locs);
    let vals = Arc::new(ctx.vals);
    let parserdef = pd.lookup(db)?;
    let val_fun = parserdef.from.is_none();
    Ok(Arc::new(PdLenTerm {
        expr,
        root,
        vals,
        term_spans,
        block_locs,
        expr_locs,
        is_val_fun: val_fun,
    }))
}
