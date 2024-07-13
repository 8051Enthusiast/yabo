use std::sync::Arc;

use fxhash::FxHashMap;
use yaboc_ast::expr::{BtMarkKind, FieldAccessMode, WiggleKind};
use yaboc_backtrack::{ExprNode, ExpressionBuilder, Instruction, TypeLookup};
use yaboc_base::{
    error::{SResult, Silencable},
    interner::FieldName,
};
use yaboc_dependents::{BacktrackStatus, SubValueKind};
use yaboc_expr::{ExprHead, Expression as _, FetchExpr, SmallVec, TakeRef};
use yaboc_hir::{BlockId, BlockKind, ExprId, HirIdWrapper, HirNode, ParserDefId};
use yaboc_hir_types::{FullTypeId, NominalId};
use yaboc_resolve::expr::{Resolved, ResolvedAtom, ValBinOp, ValUnOp, ValVarOp};
use yaboc_types::{DefId, Type, TypeId};

use crate::Constraints;

impl<'a> TypeLookup for dyn Constraints + 'a {
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
    pub field_idx: FxHashMap<DefId, u32>,
}

struct ExpressionBuildCtx<'a> {
    builder: ExpressionBuilder<'a, dyn Constraints + 'a>,
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
            defs: FxHashMap::default(),
            fields: FxHashMap::default(),
            current_arg_idx: 0,
        }
    }

    fn build_block(&mut self, block_id: BlockId, kind: BlockKind, ty: TypeId) -> SResult<u32> {
        let ser = self.db.block_serialization(block_id).silence()?;
        self.builder.push(Instruction::EnterScope, ty)?;
        let (arg, ret_ty) = if kind == BlockKind::Parser {
            let arg_idx = self.current_arg_idx;
            self.current_arg_idx += 1;
            let Type::ParserArg { arg, result } = self.db.lookup(ty) else {
                panic!("expected parser arg type");
            };
            (
                Some(self.builder.push(Instruction::Arg(arg_idx), arg)?),
                result,
            )
        } else {
            let Type::FunctionArg(res, _) = self.db.lookup(ty) else {
                panic!("expected function arg type");
            };
            (None, res)
        };
        for val in ser.eval_order.iter() {
            if val.val.kind != SubValueKind::Val {
                continue;
            }
            match self.db.hir_node(val.val.id)? {
                HirNode::Let(l) => {
                    let expr = self.defs[&l.expr.0];
                    let ty = self.db.parser_type_at(l.id.0)?;
                    let res = self.builder.push(Instruction::Copy(expr), ty)?;
                    self.defs.insert(l.id.0, res);
                }
                HirNode::Expr(e) => {
                    let res = self.build_expr(e.id)?;
                    self.defs.insert(e.id.0, res);
                }
                HirNode::ChoiceIndirection(ind) => {
                    let inner = ind.choices.iter().map(|(_, c)| self.defs[c]);
                    let ty = self.db.parser_type_at(ind.id.0)?;
                    let res = self
                        .builder
                        .push(Instruction::Unify(SmallVec::from_iter(inner)), ty)?;
                    self.defs.insert(ind.id.0, res);
                }
                HirNode::Parse(p) => {
                    let expr = self.defs[&p.expr.0];
                    let ty = self.db.parser_type_at(p.id.0)?;
                    let effectful = self.db.can_backtrack(p.expr.0)?;
                    let parse_res = self.builder.push(
                        Instruction::Parse {
                            effectful,
                            fun: expr,
                            arg: arg.unwrap(),
                        },
                        ty,
                    )?;
                    self.defs.insert(p.id.0, parse_res);
                }
                HirNode::Block(_) | HirNode::Context(_) | HirNode::Choice(_) => continue,
                HirNode::Module(_)
                | HirNode::ParserDef(_)
                | HirNode::Import(_)
                | HirNode::ArgDef(_)
                | HirNode::TExpr(_) => unreachable!(),
            }
        }
        let block = block_id.lookup(self.db)?;
        let ctx = block.root_context.lookup(self.db)?;
        let ret = if block.returns {
            let ret_id = ctx
                .vars
                .get(FieldName::Return)
                .expect("no return field")
                .inner();
            self.defs[ret_id]
        } else {
            for (_, var) in ctx.vars.iter() {
                let id = var.inner();
                self.fields.insert(*id, self.defs[id]);
            }
            self.builder.push(Instruction::Identity, ret_ty)?
        };
        if kind == BlockKind::Parser {
            self.current_arg_idx -= 1;
        }
        self.builder.push(Instruction::LeaveScope(ret), ty)
    }

    fn build_expr(&mut self, expr_id: ExprId) -> SResult<u32> {
        let expr = Resolved::expr_with_data::<(FullTypeId, BacktrackStatus)>(self.db, expr_id)?;
        expr.take_ref().try_fold(|(expr, (ty, bt))| match expr {
            ExprHead::Niladic(atom) => match atom {
                ResolvedAtom::Val(id) | ResolvedAtom::Captured(id) => {
                    self.builder.push(Instruction::Copy(self.defs[&id]), *ty)
                }
                ResolvedAtom::ParserDef(pd) | ResolvedAtom::Global(pd) => {
                    self.builder.push(Instruction::ParserDef(pd.0), *ty)
                }
                ResolvedAtom::Regex(_) => self.builder.push(Instruction::True, *ty),
                ResolvedAtom::Single => self.builder.push(Instruction::Single, *ty),
                ResolvedAtom::Nil => self.builder.push(Instruction::False, *ty),
                ResolvedAtom::Array => self.builder.push(Instruction::Array, *ty),
                ResolvedAtom::ArrayFill => self.builder.push(Instruction::Array, *ty),
                ResolvedAtom::Block(id, kind) => self.build_block(id, kind, *ty),
                ResolvedAtom::Number(_)
                | ResolvedAtom::Char(_)
                | ResolvedAtom::Bool(_)
                | ResolvedAtom::Span(_, _) => self.builder.push(Instruction::None, *ty),
            },
            ExprHead::Monadic(op, inner) => match op {
                ValUnOp::Wiggle(_, WiggleKind::If) => {
                    if matches!(self.db.lookup_intern_type(*ty), Type::ParserArg { .. }) {
                        self.builder.push(Instruction::Activate(inner), *ty)
                    } else {
                        self.builder.push(Instruction::Copy(inner), *ty)
                    }
                }
                ValUnOp::Dot(name, mode) => {
                    if matches!(mode, FieldAccessMode::Backtrack) {
                        self.builder.push(Instruction::Fail, *ty)?;
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
                    self.builder.push(Instruction::GetField(inner, field), *ty)
                }
                ValUnOp::Wiggle(_, WiggleKind::Try) | ValUnOp::BtMark(BtMarkKind::KeepBt) => {
                    self.builder.push(Instruction::Copy(inner), *ty)
                }
                ValUnOp::BtMark(BtMarkKind::RemoveBt) => {
                    self.builder.push(Instruction::Deactivate(inner), *ty)
                }
                ValUnOp::EvalFun => self
                    .builder
                    .push(Instruction::Eval(bt.can_backtrack(), inner), *ty),
                ValUnOp::Not | ValUnOp::Neg | ValUnOp::Size | ValUnOp::GetAddr => {
                    self.builder.push(Instruction::None, *ty)
                }
            },
            ExprHead::Dyadic(op, [lhs, rhs]) => match op {
                ValBinOp::ParserApply => self.builder.push(
                    Instruction::Parse {
                        effectful: bt.can_backtrack(),
                        fun: rhs,
                        arg: lhs,
                    },
                    *ty,
                ),
                ValBinOp::Else => self
                    .builder
                    .push(Instruction::Unify(SmallVec::from_slice(&[lhs, rhs])), *ty),
                ValBinOp::Then => self.builder.push(Instruction::Copy(rhs), *ty),
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
                | ValBinOp::Mul => self.builder.push(Instruction::None, *ty),
            },
            ExprHead::Variadic(ValVarOp::PartialApply(_), args) => {
                let (fun, args) = args.split_first().unwrap();
                self.builder
                    .push(Instruction::Apply(*fun, SmallVec::from(args)), *ty)
            }
        })
    }

    fn build(mut self) -> SResult<BtTerm> {
        let sig = self.db.parser_args(self.pd)?;
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
            self.builder.push(Instruction::EnterScope, fun_ty)?;
            for arg in parserdef.args.into_iter().flatten() {
                let arg_ty = self.db.parser_type_at(arg.0)?;
                let arg_idx = self
                    .builder
                    .push(Instruction::Arg(self.current_arg_idx), arg_ty)?;
                self.current_arg_idx += 1;
                self.defs.insert(arg.0, arg_idx);
            }
        }
        let parse_arg = if let Some(parse_ty) = parse_ty {
            self.builder.push(Instruction::EnterScope, parse_ty)?;
            let arg = self
                .builder
                .push(Instruction::Arg(self.current_arg_idx), sig.from.unwrap())?;
            self.current_arg_idx += 1;
            Some(arg)
        } else {
            None
        };
        let return_ty = self.db.parser_returns(self.pd)?.deref;
        let mut return_idx = self.build_expr(parserdef.to)?;
        if let Some(arg) = parse_arg {
            let effectful = self.db.can_backtrack(parserdef.to.0)?;
            return_idx = self.builder.push(
                Instruction::Parse {
                    effectful,
                    fun: return_idx,
                    arg,
                },
                return_ty,
            )?;
        }
        let mut lookup_idx = self.builder.push(Instruction::Identity, thunk_ty)?;
        if let Some(parse_ty) = parse_ty {
            lookup_idx = self
                .builder
                .push(Instruction::LeaveScope(lookup_idx), parse_ty)?;
        }
        if let Some(fun_ty) = fun_ty {
            lookup_idx = self
                .builder
                .push(Instruction::LeaveScope(lookup_idx), fun_ty)?;
        }
        let expr = self.builder.build();
        Ok(BtTerm {
            pd: self.pd,
            expr,
            lookup_idx,
            return_idx,
            field_idx: self.fields,
        })
    }
}

pub fn bt_term(db: &dyn Constraints, pd: ParserDefId) -> SResult<Arc<BtTerm>> {
    let ctx = ExpressionBuildCtx::new(db, pd);
    ctx.build().map(Arc::new)
}
