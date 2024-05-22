use std::sync::Arc;

use super::Constraints;
use fxhash::FxHashMap;
use hir::HirIdWrapper;
use yaboc_ast::expr::{FieldAccessMode, WiggleKind};
use yaboc_base::{
    error::{SResult, Silencable},
    interner::{DefId, FieldName},
};
use yaboc_dependents::{
    requirements::ExprDepData, BacktrackStatus, BlockSerialization, SubValue, SubValueKind,
};
use yaboc_expr::{ExprHead, ExprIdx, Expression, FetchExpr, TakeRef};
use yaboc_hir as hir;
use yaboc_hir_types::FullTypeId;
use yaboc_len::{depvec::SmallBitVec, BlockInfo, BlockInfoIdx, BlockKind, SizeExpr, Term};
use yaboc_req::NeededBy;
use yaboc_resolve::expr::{Resolved, ResolvedAtom};
use yaboc_resolve::expr::{ValBinOp, ValUnOp, ValVarOp};
use yaboc_types::Type;

pub struct SizeTermBuilder<'a> {
    db: &'a dyn Constraints,
    terms: SizeExpr<hir::ParserDefId>,
    call_arities: Vec<usize>,
    term_spans: Vec<Origin>,
    vals: FxHashMap<SubValue, usize>,
    block_locs: FxHashMap<hir::BlockId, usize>,
    arg_depth: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Origin {
    Expr(hir::ExprId, ExprIdx<Resolved>),
    Node(DefId),
}

impl<'a> SizeTermBuilder<'a> {
    pub fn new(db: &'a dyn Constraints, arg_depth: u32) -> Self {
        Self {
            db,
            terms: SizeExpr::new(),
            call_arities: Vec::new(),
            term_spans: Vec::new(),
            vals: FxHashMap::default(),
            block_locs: FxHashMap::default(),
            arg_depth,
        }
    }

    fn push_term(&mut self, term: Term<hir::ParserDefId>, span: Origin) -> usize {
        let index = self.terms.push(term, self.arg_depth);
        self.term_spans.push(span);
        self.call_arities.push(0);
        index
    }

    fn new_block(&mut self, bid: hir::BlockId) -> SResult<BlockInfoIdx> {
        let mut captures = SmallBitVec::default();
        for id in self.db.captures(bid).iter() {
            let val = self.vals[&SubValue::new_val(*id)];
            captures.set(val);
        }
        let kind = match bid.lookup(self.db)?.kind {
            hir::BlockKind::Inline => BlockKind::Inline,
            hir::BlockKind::Parser => BlockKind::Parser,
        };
        let block_info = BlockInfo { captures, kind };
        let idx = BlockInfoIdx::new(self.terms.block_info.len() as u32);
        self.terms.block_info.push(block_info);
        Ok(idx)
    }

    fn create_block(&mut self, bid: hir::BlockId) -> SResult<usize> {
        let ser: BlockSerialization = self.db.block_serialization(bid).silence()?;
        let mut whole_result = None;
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
                    let val = if b.returns {
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
        let idx = self.new_block(bid)?;
        let res = self.push_term(Term::BlockEnd(idx, res), loc);
        self.block_locs.insert(bid, res);
        Ok(res)
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
        mapped
            .try_fold(|(head, (src, ty, dep, bt))| {
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
                        ResolvedAtom::Block(bid, _) => {
                            self.arg_depth += 1;
                            let ret = self.create_block(bid);
                            self.arg_depth -= 1;
                            ret?
                        }
                    },
                    ExprHead::Monadic(m, (inner, inner_bt, inner_ty)) => match m {
                        ValUnOp::Neg => self.push_term(Term::Neg(inner), src),
                        ValUnOp::Wiggle(_, WiggleKind::If) if needs_bt => {
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
                    ExprHead::Dyadic(d, [(lhs, _, _), (rhs, rhs_bt, _)]) => match d {
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
                        | ValBinOp::ParserApply => self.push_term(Term::OpaqueBin([lhs, rhs]), src),
                    },
                    ExprHead::Variadic(ValVarOp::PartialApply(_), args) => {
                        let (f, ..) = args[0];
                        let mut ret = f;
                        for (arg, ..) in args[1..].iter() {
                            ret = self.push_term(Term::Apply([ret, *arg]), src);
                        }
                        *self.call_arities.last_mut().unwrap() = args.len() - 1;
                        ret
                    }
                };
                Ok((ret, bt, ty))
            })
            .map(|(x, ..)| x)
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
            let term = self.push_term(Term::Arg(arg_idx.try_into().unwrap()), loc);
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
    pub is_val_fun: bool,
}

pub fn len_term(db: &dyn Constraints, pd: hir::ParserDefId) -> SResult<Arc<PdLenTerm>> {
    let arg_count = db.argnum(pd)?.unwrap_or(0);
    let mut ctx = SizeTermBuilder::new(db, arg_count as u32);
    let root = ctx.create_pd(pd)?;
    let call_arities = ctx.call_arities;
    let expr = ctx.terms;
    let term_spans = ctx.term_spans;
    let block_locs = ctx.block_locs;
    let vals = Arc::new(ctx.vals);
    let parserdef = pd.lookup(db)?;
    let val_fun = parserdef.from.is_none();
    Ok(Arc::new(PdLenTerm {
        expr,
        root,
        call_arities,
        vals,
        term_spans,
        block_locs,
        is_val_fun: val_fun,
    }))
}
