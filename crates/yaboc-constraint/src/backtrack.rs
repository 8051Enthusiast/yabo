use std::sync::Arc;

use fxhash::FxHashMap;
use yaboc_ast::expr::{BtMarkKind, FieldAccessMode, WiggleKind};
use yaboc_backtrack::{
    Arena, EffectError, EvalCtx, EvalEffectKind, ExprNode, ExpressionBuilder, Instruction, Matrix,
    MatrixArena, Row, TransformInfo, TypeBtInfo, TypeLookup, TypeMatrixCtx, VarRow,
};
use yaboc_base::{
    error::{SResult, Silencable},
    interner::FieldName,
};
use yaboc_dependents::{BacktrackStatus, SubValueKind};
use yaboc_expr::{ExprHead, ExprIdx, Expression as _, FetchExpr, SmallVec, TakeRef};
use yaboc_hir::{
    BlockId, BlockKind, BlockReturnKind, ContextId, ExprId, HirIdWrapper, HirNode, LambdaId,
    ParserDefId,
};
use yaboc_hir_types::{FullTypeId, NominalId};
use yaboc_resolve::{
    expr::{Resolved, ResolvedAtom, ValBinOp, ValUnOp, ValVarOp},
    parserdef_ssc::FunctionSscId,
};
use yaboc_types::{DefId, Type, TypeId};

use crate::{Constraints, Origin};

impl TypeLookup for dyn Constraints + '_ {
    fn lookup(&self, ty: TypeId) -> Type {
        self.lookup_intern_type(ty)
    }

    fn bound_types(&self, id: yaboc_types::DefId) -> SResult<Arc<[TypeId]>> {
        self.bound_args(id)
    }

    fn subst_ty(&self, ty: TypeId, subst: Arc<Vec<TypeId>>) -> TypeId {
        self.substitute_typevar(ty, subst)
    }

    fn least_deref_type(&self, ty: TypeId) -> SResult<TypeId> {
        self.least_deref_type(ty)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BtTerm {
    pub pd: ParserDefId,
    pub return_idx: u32,
    pub lookup_idx: u32,
    pub expr: Vec<ExprNode>,
    pub origin: Vec<Origin>,
    pub field_idx: FxHashMap<DefId, u32>,
}

impl BtTerm {
    fn empty_vals(&self) -> Vec<Row> {
        let row_count = self.expr.last().map(|x| x.row_range().end).unwrap_or(0);
        let mut ret = Vec::with_capacity(row_count);
        for expr_node in &self.expr {
            let row = Row::Vars(VarRow::empty(expr_node.bound, expr_node.bound));
            for _ in expr_node.row_range() {
                ret.push(row.clone());
            }
        }
        ret
    }

    fn return_node(&self) -> &ExprNode {
        &self.expr[self.return_idx as usize]
    }

    fn lookup_node(&self) -> &ExprNode {
        &self.expr[self.lookup_idx as usize]
    }
}

struct ExpressionBuildCtx<'a> {
    builder: ExpressionBuilder<'a, dyn Constraints + 'a>,
    origin: Vec<Origin>,
    db: &'a dyn Constraints,
    pd: ParserDefId,
    defs: FxHashMap<DefId, u32>,
    fields: FxHashMap<DefId, u32>,
    current_arg_idx: u32,
}

impl<'a> ExpressionBuildCtx<'a> {
    fn new(db: &'a dyn Constraints, pd: ParserDefId) -> Self {
        let builder = ExpressionBuilder::new(db);
        Self {
            db,
            pd,
            builder,
            origin: Vec::new(),
            defs: FxHashMap::default(),
            fields: FxHashMap::default(),
            current_arg_idx: 0,
        }
    }

    fn push(&mut self, instr: Instruction, ty: TypeId, origin: Origin) -> SResult<u32> {
        let res = self.builder.push(instr, ty);
        if res.is_ok() {
            self.origin.push(origin);
        }
        res
    }

    fn is_silent_context_impl(
        &self,
        context: ContextId,
        cache: &mut FxHashMap<ContextId, bool>,
    ) -> SResult<bool> {
        let ctx = context.lookup(self.db)?;
        let Some(choice) = ctx.parent_choice else {
            return Ok(false);
        };
        let choice = choice.lookup(self.db)?;
        if choice.subcontexts.last() != Some(&context) {
            return Ok(true);
        }
        let parent_context = choice.parent_context;
        self.is_silent_context(parent_context, cache)
    }

    fn is_silent_context(
        &self,
        context: ContextId,
        cache: &mut FxHashMap<ContextId, bool>,
    ) -> SResult<bool> {
        if let Some(is) = cache.get(&context) {
            return Ok(*is);
        }
        let is = self.is_silent_context_impl(context, cache)?;
        cache.insert(context, is);
        Ok(is)
    }

    fn build_block(&mut self, block_id: BlockId, kind: BlockKind, ty: TypeId) -> SResult<u32> {
        let ser = self.db.block_serialization(block_id).silence()?;
        let block_origin = Origin::Node(block_id.0);
        self.push(Instruction::EnterScope, ty, block_origin)?;
        let (arg, ret_ty) = if kind == BlockKind::Parser {
            let arg_idx = self.current_arg_idx;
            self.current_arg_idx += 1;
            let Type::ParserArg { arg, result } = self.db.lookup(ty) else {
                panic!("expected parser arg type");
            };
            (
                Some(self.push(Instruction::Arg(arg_idx), arg, block_origin)?),
                result,
            )
        } else {
            let Type::FunctionArg(res, _) = self.db.lookup(ty) else {
                panic!("expected function arg type");
            };
            (None, res)
        };
        let mut context_cache = FxHashMap::default();
        for val in ser.eval_order.iter() {
            if val.val.kind != SubValueKind::Val {
                continue;
            }
            let origin = Origin::Node(val.val.id);
            match self.db.hir_node(val.val.id)? {
                HirNode::Let(l) => {
                    let silent = self.is_silent_context(l.context, &mut context_cache)?;
                    let (_, expr) = self.build_expr(l.expr, silent)?;
                    let ty = self.db.parser_type_at(l.id.0)?;
                    let res = self.push(Instruction::Copy(expr), ty, origin)?;
                    self.defs.insert(l.id.0, res);
                }
                HirNode::Expr(_) => continue,
                HirNode::ChoiceIndirection(ind) => {
                    let inner = ind.choices.iter().map(|(_, c)| self.defs[c]);
                    let ty = self.db.parser_type_at(ind.id.0)?;
                    let res =
                        self.push(Instruction::Unify(SmallVec::from_iter(inner)), ty, origin)?;
                    self.defs.insert(ind.id.0, res);
                }
                HirNode::Parse(p) => {
                    let silent = self.is_silent_context(p.parent_context, &mut context_cache)?;
                    let (effectful, expr) = self.build_expr(p.expr, silent)?;
                    let ty = self.db.parser_type_at(p.id.0)?;
                    let effectful = EvalEffectKind::from_flags(effectful, silent);
                    let parse_res = self.push(
                        Instruction::Parse {
                            effectful,
                            fun: expr,
                            arg: arg.unwrap(),
                        },
                        ty,
                        origin,
                    )?;
                    self.defs.insert(p.id.0, parse_res);
                }
                HirNode::Block(_) | HirNode::Context(_) | HirNode::Choice(_) => continue,
                HirNode::Module(_)
                | HirNode::ParserDef(_)
                | HirNode::Import(_)
                | HirNode::ArgDef(_)
                | HirNode::Lambda(_)
                | HirNode::TExpr(_) => unreachable!(),
            }
        }
        let block = block_id.lookup(self.db)?;
        let ctx = block.root_context.lookup(self.db)?;
        let ret = match block.returns {
            BlockReturnKind::Returns => {
                let ret_id = ctx
                    .vars
                    .get(FieldName::Return)
                    .expect("no return field")
                    .inner();
                self.defs[ret_id]
            }
            BlockReturnKind::Fields => {
                for (_, var) in ctx.vars.iter() {
                    let id = var.inner();
                    self.fields.insert(*id, self.defs[id]);
                }
                self.push(Instruction::Identity, ret_ty, block_origin)?
            }
            BlockReturnKind::Nothing => self.push(Instruction::None, ret_ty, block_origin)?,
        };
        if kind == BlockKind::Parser {
            self.current_arg_idx -= 1;
        }
        self.push(Instruction::LeaveScope(ret), ty, block_origin)
    }

    fn build_lambda(&mut self, lambda_id: LambdaId, ty: TypeId) -> SResult<u32> {
        let lambda_origin = Origin::Node(lambda_id.0);
        let lambda = lambda_id.lookup(self.db)?;
        self.push(Instruction::EnterScope, ty, lambda_origin)?;
        for arg in lambda.args.iter() {
            let arg_ty = self.db.parser_type_at(arg.0)?;
            let arg_idx = self.push(
                Instruction::Arg(self.current_arg_idx),
                arg_ty,
                lambda_origin,
            )?;
            self.current_arg_idx += 1;
            self.defs.insert(arg.0, arg_idx);
        }
        let (_, expr) = self.build_expr(lambda.expr, false)?;
        self.push(Instruction::LeaveScope(expr), ty, lambda_origin)
    }

    fn build_expr(&mut self, expr_id: ExprId, silent: bool) -> SResult<(bool, u32)> {
        let expr = Resolved::expr_with_data::<(ExprIdx<Resolved>, (FullTypeId, BacktrackStatus))>(
            self.db, expr_id,
        )?;
        let mut reachable = vec![false; expr.expr.len()];
        reachable[expr.expr.root().as_usize()] = true;
        for (i, node) in expr.expr.take_ref().iter_parts().enumerate().rev() {
            let current_reachable = reachable[i];
            if let ExprHead::Dyadic(ValBinOp::Else, [_, rhs]) = node {
                reachable[rhs.as_usize()] = current_reachable;
            } else {
                for inner in node.inner_refs() {
                    reachable[inner.as_usize()] = current_reachable;
                }
            }
        }
        let expr = expr.take_ref().map(|(idx, (ty, bt))| {
            (
                Origin::Expr(expr_id, idx),
                ty,
                bt.can_backtrack(),
                silent || !reachable[idx.as_usize()],
            )
        });
        expr.try_fold(|(expr, (orig, ty, bt, silent))| -> SResult<(bool, u32)> {
            match expr {
                ExprHead::Niladic(atom) => match atom {
                    ResolvedAtom::Val(id) | ResolvedAtom::Captured(id) => {
                        self.push(Instruction::Copy(self.defs[&id]), *ty, orig)
                    }
                    ResolvedAtom::ParserDef(pd) | ResolvedAtom::Global(pd) => {
                        self.push(Instruction::ParserDef(pd.0), *ty, orig)
                    }
                    ResolvedAtom::Regex(_) => self.push(Instruction::True, *ty, orig),
                    ResolvedAtom::Single => self.push(Instruction::Single, *ty, orig),
                    ResolvedAtom::Array => self.push(Instruction::Array, *ty, orig),
                    ResolvedAtom::ArrayFill => self.push(Instruction::Array, *ty, orig),
                    ResolvedAtom::Block(id, kind) => self.build_block(id, kind, *ty),
                    ResolvedAtom::Lambda(id) => self.build_lambda(id, *ty),
                    ResolvedAtom::Number(_)
                    | ResolvedAtom::Char(_)
                    | ResolvedAtom::Bool(_)
                    | ResolvedAtom::Span(_, _) => self.push(Instruction::None, *ty, orig),
                },
                ExprHead::Monadic(op, (inner_bt, inner)) => match op {
                    ValUnOp::Wiggle(_, WiggleKind::Is) => {
                        if matches!(self.db.lookup_intern_type(*ty), Type::ParserArg { .. }) {
                            self.push(Instruction::Activate(inner), *ty, orig)
                        } else {
                            if !silent {
                                self.push(Instruction::Fail, *ty, orig)?;
                            }
                            self.push(Instruction::Copy(inner), *ty, orig)
                        }
                    }
                    ValUnOp::Dot(name, mode) => {
                        if matches!(mode, FieldAccessMode::Backtrack) && !silent {
                            self.push(Instruction::Fail, *ty, orig)?;
                        }
                        let block_ty = self.builder.ty(inner);
                        let block_ty = self.db.least_deref_type(block_ty)?;
                        let Type::Nominal(nom) = self.db.lookup(block_ty) else {
                            panic!("expected block type");
                        };
                        let NominalId::Block(block_id) = NominalId::from_nominal_head(&nom) else {
                            panic!("expected block type");
                        };
                        let ctx = block_id.lookup(self.db)?.root_context.lookup(self.db)?;
                        let field = *ctx.vars.get(name).expect("did not find field").inner();
                        self.push(Instruction::GetField(inner, field), *ty, orig)
                    }
                    ValUnOp::Wiggle(_, WiggleKind::Try) | ValUnOp::BtMark(BtMarkKind::KeepBt) => {
                        self.push(Instruction::Copy(inner), *ty, orig)
                    }
                    ValUnOp::BtMark(BtMarkKind::RemoveBt) => {
                        self.push(Instruction::Deactivate(inner), *ty, orig)
                    }
                    ValUnOp::EvalFun => self.push(
                        Instruction::Eval(EvalEffectKind::from_flags(inner_bt, silent), inner),
                        *ty,
                        orig,
                    ),
                    ValUnOp::Not | ValUnOp::Neg | ValUnOp::Size | ValUnOp::GetAddr => {
                        self.push(Instruction::None, *ty, orig)
                    }
                },
                ExprHead::Dyadic(op, [(_, lhs), (rbt, rhs)]) => match op {
                    ValBinOp::ParserApply => self.push(
                        Instruction::Parse {
                            effectful: EvalEffectKind::from_flags(rbt, silent),
                            fun: rhs,
                            arg: lhs,
                        },
                        *ty,
                        orig,
                    ),
                    ValBinOp::Else => self.push(
                        Instruction::Unify(SmallVec::from_slice(&[lhs, rhs])),
                        *ty,
                        orig,
                    ),
                    ValBinOp::Then => self.push(Instruction::Copy(rhs), *ty, orig),
                    ValBinOp::Range
                    | ValBinOp::And
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
                    | ValBinOp::Minus
                    | ValBinOp::Plus
                    | ValBinOp::Div
                    | ValBinOp::Modulo
                    | ValBinOp::Mul => self.push(Instruction::None, *ty, orig),
                },
                ExprHead::Variadic(ValVarOp::PartialApply(_), args) => {
                    let ((_, fun), args) = args.split_first().unwrap();
                    self.push(
                        Instruction::Apply(*fun, SmallVec::from_iter(args.iter().map(|x| x.1))),
                        *ty,
                        orig,
                    )
                }
            }
            .map(|v| (bt, v))
        })
    }

    fn build(mut self) -> SResult<BtTerm> {
        let sig = self.db.parser_args(self.pd)?;
        let origin = Origin::Node(self.pd.0);
        let thunk_ty = self.db.intern_type(Type::Nominal(sig.thunk));
        let parserdef = self.pd.lookup(self.db)?;
        let mut ty = thunk_ty;
        let parse_ty = if let Some(arg) = sig.from {
            let parse_ty = self.db.intern_type(Type::ParserArg { result: ty, arg });
            ty = parse_ty;
            Some(parse_ty)
        } else {
            None
        };
        let fun_ty = if let Some(args) = sig.args {
            let fun_ty = self.db.intern_type(Type::FunctionArg(ty, args));
            Some(fun_ty)
        } else {
            None
        };
        if let Some(fun_ty) = fun_ty {
            self.push(Instruction::EnterScope, fun_ty, origin)?;
            for arg in parserdef.args.into_iter().flatten() {
                let arg_ty = self.db.parser_type_at(arg.0)?;
                let arg_idx = self.push(Instruction::Arg(self.current_arg_idx), arg_ty, origin)?;
                self.current_arg_idx += 1;
                self.defs.insert(arg.0, arg_idx);
            }
        }
        let parse_arg = if let Some(parse_ty) = parse_ty {
            self.push(Instruction::EnterScope, parse_ty, origin)?;
            let arg = self.push(
                Instruction::Arg(self.current_arg_idx),
                sig.from.unwrap(),
                origin,
            )?;
            self.current_arg_idx += 1;
            Some(arg)
        } else {
            None
        };
        let return_ty = self.db.parser_returns(self.pd)?.deref;
        let (effectful, mut return_idx) = self.build_expr(parserdef.to, false)?;
        if let Some(arg) = parse_arg {
            let effectful = EvalEffectKind::from_flags(effectful, false);
            return_idx = self.push(
                Instruction::Parse {
                    effectful,
                    fun: return_idx,
                    arg,
                },
                return_ty,
                origin,
            )?;
        }
        let mut lookup_idx = self.push(Instruction::Identity, thunk_ty, origin)?;
        if let Some(parse_ty) = parse_ty {
            lookup_idx = self.push(Instruction::LeaveScope(lookup_idx), parse_ty, origin)?;
        }
        if let Some(fun_ty) = fun_ty {
            lookup_idx = self.push(Instruction::LeaveScope(lookup_idx), fun_ty, origin)?;
        }
        let expr = self.builder.build();
        Ok(BtTerm {
            pd: self.pd,
            expr,
            lookup_idx,
            return_idx,
            field_idx: self.fields,
            origin: self.origin,
        })
    }
}

pub fn bt_term(db: &dyn Constraints, pd: ParserDefId) -> SResult<Arc<BtTerm>> {
    let ctx = ExpressionBuildCtx::new(db, pd);
    ctx.build().map(Arc::new)
}

pub struct BtInferContext<'a> {
    db: &'a dyn Constraints,
    arena: MatrixArena<'a>,
    terms: FxHashMap<DefId, Arc<BtTerm>>,
    vals: FxHashMap<DefId, Vec<Row>>,
    pds: Vec<ParserDefId>,
}

impl<'a> BtInferContext<'a> {
    pub fn new(db: &'a dyn Constraints, arena: &'a Arena, ssc: FunctionSscId) -> SResult<Self> {
        let pds = db.lookup_intern_recursion_scc(ssc);
        let mut terms = FxHashMap::default();
        let mut vals = FxHashMap::default();
        for pd in pds.iter() {
            let term = db.bt_term(*pd)?;
            vals.insert(pd.0, term.empty_vals());
            terms.insert(pd.0, term);
        }
        let arena = MatrixArena::new(arena);
        Ok(Self {
            db,
            terms,
            vals,
            arena,
            pds,
        })
    }

    fn get_transform_info(
        &self,
        pd: ParserDefId,
        get_node: impl FnOnce(&BtTerm) -> &ExprNode,
    ) -> SResult<TransformInfo> {
        let term = if let Some(term) = self.terms.get(&pd.0) {
            term.clone()
        } else {
            self.db.bt_term(pd)?
        };
        let vals = if let Some(vals) = self.vals.get(&pd.0) {
            vals
        } else {
            &self.db.bt_vals(pd).vals
        };
        let node = get_node(&term);
        let range = node.row_range();
        let matrix = self.arena.new_mutable_rows(vals[range].iter().cloned());
        for row in matrix.iter_mut() {
            row.set_bound_bits(0);
        }
        let matrix = Matrix::from_rows(matrix);
        Ok(TransformInfo {
            matrix,
            to_ty: node.ty,
        })
    }

    fn infer(mut self) -> SResult<BtResult> {
        let arena = self.arena;
        let errors = loop {
            let mut matrix_ctx = TypeMatrixCtx::new(&self, arena);
            let mut current_errors = Vec::new();
            let mut new_vals_map = FxHashMap::default();
            for def in self.pds.iter() {
                let term = self.terms[&def.0].clone();
                let mut new_vals = term.empty_vals();
                let eval = EvalCtx::new(&term.expr, &mut new_vals, matrix_ctx);
                let errors;
                (matrix_ctx, errors) = eval.infer();
                current_errors.extend(errors?.into_iter().map(|err| (def.0, err)));
                new_vals_map.insert(def.0, new_vals);
            }
            if new_vals_map == self.vals {
                break current_errors;
            }
            self.vals = new_vals_map;
        };
        let mut ret_vals = Vec::new();
        for def in self.pds.iter() {
            ret_vals.push(BtVals {
                vals: self.vals.remove(&def.0).unwrap(),
            });
        }
        Ok(BtResult {
            vals: ret_vals,
            errors,
        })
    }
}

impl<'a> TypeBtInfo<'a> for BtInferContext<'a> {
    fn deref_matrix(&self, def: DefId) -> SResult<Option<TransformInfo>> {
        let pd = self.db.hir_parent_parserdef(def)?;
        if pd.0 != def {
            return Ok(None);
        }
        self.get_transform_info(pd, |term| term.return_node())
            .map(Some)
    }

    fn field(&self, def: DefId) -> SResult<TransformInfo> {
        let pd = self.db.hir_parent_parserdef(def)?;
        self.get_transform_info(pd, |term| {
            let field_idx = term.field_idx[&def];
            &term.expr[field_idx as usize]
        })
    }

    fn parserdef(&self, def: DefId) -> SResult<TransformInfo> {
        let pd = self.db.hir_parent_parserdef(def)?;
        assert!(pd.0 == def);
        self.get_transform_info(pd, |term| term.lookup_node())
    }

    fn partial_apply_ty(&self, ty: TypeId, applied_arg_count: usize) -> TypeId {
        let ty = self.db.lookup(ty);
        let Type::FunctionArg(res, args) = ty else {
            panic!("expected function arg type, got {:?}", ty);
        };
        let leftover_args = args[applied_arg_count..].to_vec();
        self.db
            .intern_type(Type::FunctionArg(res, Arc::new(leftover_args)))
    }

    type Lookup = dyn Constraints + 'a;

    fn types(&self) -> &Self::Lookup {
        self.db
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BtVals {
    pub vals: Vec<Row>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BtResult {
    pub vals: Vec<BtVals>,
    pub errors: Vec<(DefId, EffectError)>,
}

pub fn ssc_bt_vals(db: &dyn Constraints, ssc: FunctionSscId) -> SResult<Arc<BtResult>> {
    let arena = Arena::new();
    let ctx = BtInferContext::new(db, &arena, ssc)?;
    ctx.infer().map(Arc::new)
}
