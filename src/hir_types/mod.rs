mod full;
mod public;
pub mod represent;
mod returns;
mod signature;

use std::{collections::BTreeMap, fmt::Debug, hash::Hash, marker::PhantomData, sync::Arc};

use fxhash::FxHashMap;

use crate::{
    expr::{
        Atom, BasicValBinOp, ExprIter, Expression, ExpressionComponent, ExpressionKind, TypeBinOp,
        TypeUnOp, ValBinOp, ValUnOp,
    },
    hir::{HirIdWrapper, IndexSpanned, ParseStatement, ParserAtom, ParserDefRef},
    interner::{FieldName, HirId, TypeVar, TypeVarName},
    types::{
        EitherType, InfTypeId, InferenceContext, InferenceType, NominalInfHead, NominalKind,
        NominalTypeHead, Signature, Type, TypeError, TypeId, TypeResolver,
    },
};

use crate::hir::{self, Hirs};

use full::{parser_full_types, parser_type_at, ParserFullTypes};
use public::{ambient_type, public_expr_type, public_type};
use returns::{parser_returns, parser_returns_ssc, ParserDefType};
use signature::{get_signature, get_thunk, parser_args};

#[salsa::query_group(HirTypesDatabase)]
pub trait TyHirs: Hirs + crate::types::TypeInterner {
    fn parser_args(&self, id: hir::ParserDefId) -> Result<Signature, TypeError>;
    fn parser_returns(&self, id: hir::ParserDefId) -> Result<ParserDefType, TypeError>;
    fn parser_returns_ssc(
        &self,
        id: hir::recursion::FunctionSscId,
    ) -> Result<Vec<ParserDefType>, TypeError>;
    fn public_type(&self, loc: HirId) -> Result<TypeId, ()>;
    fn parser_type_at(&self, loc: HirId) -> Result<TypeId, TypeError>;
    fn public_expr_type(
        &self,
        loc: hir::ExprId,
    ) -> Result<(Expression<TypedHirVal<TypeId>>, TypeId), ()>;
    fn ambient_type(&self, id: hir::ParseId) -> Result<TypeId, ()>;
    fn parser_full_types(&self, id: hir::ParserDefId) -> Result<Arc<ParserFullTypes>, TypeError>;
}

type TypedExpression = Expression<TypedHirVal<TypeId>>;
type InfTypedExpression = Expression<TypedHirVal<InfTypeId>>;

pub struct ExpressionTypeConstraints {
    pub root_type: Option<InfTypeId>,
    pub from_type: Option<InfTypeId>,
}

impl<'a, TR: TypeResolver> TypingContext<'a, TR> {
    fn new(db: &'a dyn TyHirs, tr: TR) -> Self {
        TypingContext {
            db,
            infctx: InferenceContext::new(tr),
        }
    }

    pub fn resolve_type_expr(
        &mut self,
        context: &mut TypingLocation,
        expr: &Expression<hir::HirType>,
    ) -> Result<InfTypeId, TypeError> {
        let ret = match expr {
            Expression::BinaryOp(op) => match op.as_ref() {
                TypeBinOp::Wiggle(e, _, _) => self.resolve_type_expr(context, e)?,
                TypeBinOp::Ref(_, _, _) => {
                    unimplemented!()
                }
                TypeBinOp::ParseArg(from_expr, inner_expr, _) => {
                    let from = self.resolve_type_expr(context, from_expr)?;
                    let inner = match inner_expr {
                        Expression::Atom(IndexSpanned {
                            atom: hir::TypeAtom::ParserDef(pd),
                            ..
                        }) => self.resolve_type_expr_parserdef_ref(context, pd, Some(from))?,
                        _ => self.resolve_type_expr(context, inner_expr)?,
                    };
                    self.db.intern_inference_type(InferenceType::ParserArg {
                        result: inner,
                        arg: from,
                    })
                }
            },
            Expression::UnaryOp(op) => match op.as_ref() {
                TypeUnOp::Ref(e, _) => self.resolve_type_expr(context, e)?,
            },
            Expression::Atom(a) => match &a.atom {
                hir::TypeAtom::Primitive(hir::TypePrimitive::Int) => self.infctx.int(),
                hir::TypeAtom::Primitive(hir::TypePrimitive::Bit) => self.infctx.bit(),
                hir::TypeAtom::Primitive(hir::TypePrimitive::Char) => self.infctx.char(),
                hir::TypeAtom::Primitive(hir::TypePrimitive::Mem) => todo!(),
                hir::TypeAtom::ParserDef(pd) => {
                    return self.resolve_type_expr_parserdef_ref(context, pd, None)
                }
                hir::TypeAtom::Array(a) => {
                    let inner = self.resolve_type_expr(context, &a.expr)?;
                    self.db
                        .intern_inference_type(InferenceType::Loop(a.direction, inner))
                }
                hir::TypeAtom::TypeVar(v) => {
                    let var_idx = context.vars.get_var(*v).ok_or(TypeError)?;
                    self.db.intern_inference_type(InferenceType::TypeVarRef(
                        context.pd.0,
                        0,
                        var_idx,
                    ))
                }
            },
        };
        Ok(ret)
    }
    fn resolve_type_expr_parserdef_ref(
        &mut self,
        context: &mut TypingLocation,
        pd: &ParserDefRef,
        parserarg_from: Option<InfTypeId>,
    ) -> Result<InfTypeId, TypeError> {
        let mut parse_arg = pd
            .from
            .as_ref()
            .map(|x| self.resolve_type_expr(context, x))
            .transpose()?
            .or(parserarg_from);
        let mut fun_args = Box::new(
            pd.args
                .iter()
                .map(|x| self.resolve_type_expr(context, x))
                .collect::<Result<Vec<_>, _>>()?,
        );
        let def = hir::refs::parserdef_ref(self.db, context.loc, FieldName::Ident(pd.name.atom))
            .map_err(|_| TypeError)?;
        let definition = self.db.parser_args(def)?;
        match (parse_arg, definition.from) {
            (None, Some(_)) => parse_arg = Some(self.infctx.var()),
            (Some(_), None) => return Err(TypeError),
            _ => {}
        }
        if fun_args.len() > definition.args.len() {
            return Err(TypeError);
        }
        while fun_args.len() < definition.args.len() {
            fun_args.push(self.infctx.var());
        }
        let ty_args = (0..definition.ty_args.len())
            .map(|_| self.infctx.var())
            .collect::<Vec<InfTypeId>>();
        let def_type = self.db.intern_type(Type::Nominal(NominalTypeHead {
            kind: NominalKind::Def,
            def: def.0,
            parse_arg: definition.from,
            fun_args: definition.args,
            ty_args: Arc::new(n_type_vars(self.db, def, definition.ty_args.len() as u32)),
        }));
        let inferred_def = self.infctx.from_type_with_args(def_type, &ty_args);
        let ret = self
            .db
            .intern_inference_type(InferenceType::Nominal(NominalInfHead {
                kind: NominalKind::Def,
                def: def.0,
                parse_arg,
                fun_args,
                ty_args: Box::new(ty_args),
                internal: false,
            }));
        self.infctx.equal(ret, inferred_def)?;
        Ok(ret)
    }
    pub fn val_expression_type(
        &mut self,
        context: &mut TypingLocation,
        expr: &Expression<hir::HirVal>,
    ) -> Result<InfTypedExpression, TypeError> {
        use BasicValBinOp::*;
        let mut ty_and_subexpr = |expr: &Expression<hir::HirVal>| {
            self.val_expression_type(context, expr)
                .map(|e| (e.root_type(), e))
        };
        Ok(match expr {
            Expression::BinaryOp(op) => Expression::BinaryOp(Box::new(match op.as_ref() {
                ValBinOp::Basic(
                    l,
                    op @ (And | Or | Xor | Minus | Plus | Mul | Div | Modulo | Lesser | LesserEq
                    | Greater | GreaterEq | ShiftR | ShiftL),
                    r,
                    s,
                ) => {
                    let (lhs_ty, lhs) = ty_and_subexpr(l)?;
                    let (rhs_ty, rhs) = ty_and_subexpr(r)?;
                    let int = self.infctx.int();
                    self.infctx.constrain(lhs_ty, int)?;
                    self.infctx.constrain(rhs_ty, int)?;
                    ValBinOp::Basic(lhs, *op, rhs, (int, *s))
                }
                ValBinOp::Basic(_, Equals | Uneq, _, _) => todo!(),
                ValBinOp::Basic(l, Compose, r, s) => {
                    let (lhs_ty, lhs) = ty_and_subexpr(l)?;
                    let (rhs_ty, rhs) = ty_and_subexpr(r)?;
                    let ret = self.infctx.parser_compose(lhs_ty, rhs_ty)?;
                    ValBinOp::Basic(lhs, Compose, rhs, (ret, *s))
                }
                ValBinOp::Basic(l, ParserApply, r, s) => {
                    let (lhs_ty, lhs) = ty_and_subexpr(l)?;
                    let (rhs_ty, rhs) = ty_and_subexpr(r)?;
                    let ret = self.infctx.parser_apply(rhs_ty, lhs_ty)?;
                    ValBinOp::Basic(lhs, ParserApply, rhs, (ret, *s))
                }
                ValBinOp::Wiggle(inner, r, s) => {
                    let (inner_ty, inner) = ty_and_subexpr(inner)?;
                    ValBinOp::Wiggle(inner, r.clone(), (inner_ty, *s))
                }
                ValBinOp::Else(l, r, s) => {
                    let (lhs_ty, lhs) = ty_and_subexpr(l)?;
                    let (rhs_ty, rhs) = ty_and_subexpr(r)?;
                    let ret = self.infctx.one_of(&[lhs_ty, rhs_ty])?;
                    ValBinOp::Else(lhs, rhs, (ret, *s))
                }
                ValBinOp::Dot(from, name, s) => match name {
                    Atom::Field(f) => {
                        let (from_ty, from) = ty_and_subexpr(from)?;
                        let ret = self.infctx.access_field(from_ty, *f)?;
                        ValBinOp::Dot(from, name.clone(), (ret, *s))
                    }
                    Atom::Number(_) | Atom::Char(_) | Atom::String(_) => return Err(TypeError),
                },
            })),
            Expression::UnaryOp(op) => Expression::UnaryOp(Box::new(match op.as_ref() {
                v @ (ValUnOp::Not(i, s) | ValUnOp::Neg(i, s) | ValUnOp::Pos(i, s)) => {
                    let (inner_ty, inner) = ty_and_subexpr(i)?;
                    self.infctx.constrain(inner_ty, self.infctx.int())?;
                    match v {
                        ValUnOp::Not(_, _) => ValUnOp::Not(inner, (inner_ty, *s)),
                        ValUnOp::Neg(_, _) => ValUnOp::Neg(inner, (inner_ty, *s)),
                        ValUnOp::Pos(_, _) => ValUnOp::Pos(inner, (inner_ty, *s)),
                        ValUnOp::If(_, _) => unreachable!(),
                    }
                }
                ValUnOp::If(i, s) => {
                    let (inner_ty, inner) = ty_and_subexpr(i)?;
                    ValUnOp::If(inner, (inner_ty, *s))
                }
            })),
            Expression::Atom(a) => {
                let inftype = match &a.atom {
                    ParserAtom::Atom(Atom::Char(_)) => self.infctx.char(),
                    ParserAtom::Atom(Atom::Number(_)) => self.infctx.int(),
                    ParserAtom::Atom(Atom::String(_)) => todo!(),
                    ParserAtom::Atom(Atom::Field(f)) => self.infctx.lookup(context.loc, *f)?,
                    ParserAtom::Single => self.infctx.single(),
                    ParserAtom::Array(a) => {
                        let array = a.lookup(self.db).map_err(|_| TypeError)?;
                        let inner = array.expr.lookup(self.db).map_err(|_| TypeError)?.expr;
                        let inner_ty = self.val_expression_type(context, &inner)?.root_type();
                        self.infctx.array_call(array.direction, inner_ty)?
                    }
                    ParserAtom::Block(b) => {
                        let pd = self.db.hir_parent_parserdef(b.0).map_err(|_| TypeError)?;
                        let ty_vars = (0..context.vars.defs.len() as u32)
                            .map(|i| {
                                self.db
                                    .intern_inference_type(InferenceType::TypeVarRef(pd.0, 0, i))
                            })
                            .collect::<Vec<_>>();
                        self.infctx.block_call(b.0, &ty_vars)?
                    }
                };
                Expression::Atom(TypedAtom {
                    atom: a.atom.clone(),
                    span_index: a.span,
                    ty: inftype,
                })
            }
        })
    }
    fn inftype_to_concrete_type(&mut self, infty: InfTypeId) -> Result<TypeId, TypeError> {
        self.infctx.to_type(infty)
    }
    fn expr_to_concrete_type(
        &mut self,
        expr: &InfTypedExpression,
    ) -> Result<TypedExpression, TypeError> {
        Ok(match expr {
            Expression::BinaryOp(b) => Expression::BinaryOp(match b.as_ref() {
                ValBinOp::Basic(lhs, op, rhs, data) => {
                    let lhs = self.expr_to_concrete_type(lhs)?;
                    let rhs = self.expr_to_concrete_type(rhs)?;
                    let data = (self.inftype_to_concrete_type(data.0)?, data.1);
                    Box::new(ValBinOp::Basic(lhs, op.clone(), rhs, data))
                }
                ValBinOp::Wiggle(lhs, rhs, data) => {
                    let lhs = self.expr_to_concrete_type(lhs)?;
                    let data = (self.inftype_to_concrete_type(data.0)?, data.1);
                    Box::new(ValBinOp::Wiggle(lhs, rhs.clone(), data))
                }
                ValBinOp::Else(lhs, rhs, data) => {
                    let lhs = self.expr_to_concrete_type(lhs)?;
                    let rhs = self.expr_to_concrete_type(rhs)?;
                    let data = (self.inftype_to_concrete_type(data.0)?, data.1);
                    Box::new(ValBinOp::Else(lhs, rhs, data))
                }
                ValBinOp::Dot(lhs, name, data) => {
                    let lhs = self.expr_to_concrete_type(lhs)?;
                    let data = (self.inftype_to_concrete_type(data.0)?, data.1);
                    Box::new(ValBinOp::Dot(lhs, name.clone(), data))
                }
            }),
            Expression::UnaryOp(u) => Expression::UnaryOp(match u.as_ref() {
                ValUnOp::Not(inner, data) => {
                    let inner = self.expr_to_concrete_type(inner)?;
                    let data = (self.inftype_to_concrete_type(data.0)?, data.1);
                    Box::new(ValUnOp::Not(inner, data))
                }
                ValUnOp::Neg(inner, data) => {
                    let inner = self.expr_to_concrete_type(inner)?;
                    let data = (self.inftype_to_concrete_type(data.0)?, data.1);
                    Box::new(ValUnOp::Neg(inner, data))
                }
                ValUnOp::Pos(inner, data) => {
                    let inner = self.expr_to_concrete_type(inner)?;
                    let data = (self.inftype_to_concrete_type(data.0)?, data.1);
                    Box::new(ValUnOp::Pos(inner, data))
                }
                ValUnOp::If(inner, data) => {
                    let inner = self.expr_to_concrete_type(inner)?;
                    let data = (self.inftype_to_concrete_type(data.0)?, data.1);
                    Box::new(ValUnOp::If(inner, data))
                }
            }),
            Expression::Atom(a) => Expression::Atom(TypedAtom {
                atom: a.atom.clone(),
                span_index: a.span_index.clone(),
                ty: self.inftype_to_concrete_type(a.ty)?,
            }),
        })
    }
}

pub struct TypingContext<'a, TR: TypeResolver> {
    db: &'a dyn TyHirs,
    infctx: InferenceContext<TR>,
}

#[derive(Clone, Debug)]
pub struct TypeVarCollection {
    pub defs: Vec<TypeVar>,
    names: FxHashMap<TypeVar, u32>,
    frozen: bool,
}

impl TypeVarCollection {
    pub fn new_empty() -> Self {
        TypeVarCollection {
            defs: vec![],
            names: FxHashMap::default(),
            frozen: false,
        }
    }
    pub fn get_var(&mut self, var_name: TypeVar) -> Option<u32> {
        if let Some(idx) = self.names.get(&var_name) {
            return Some(*idx);
        }
        if self.frozen {
            return None;
        }
        let new_index = self.defs.len() as u32;
        self.defs.push(var_name);
        self.names.insert(var_name, new_index);
        Some(new_index)
    }
    pub fn freeze(&mut self) {
        self.frozen = true
    }
    pub fn at_id(db: &(impl TyHirs + ?Sized), id: hir::ParserDefId) -> Result<Self, TypeError> {
        let tys = db.parser_args(id)?.ty_args;
        let mut ret = Self::new_empty();
        for ty in tys.iter() {
            ret.get_var(*ty);
        }
        ret.freeze();
        Ok(ret)
    }
    pub fn var_types(&self, db: &(impl TyHirs + ?Sized), id: hir::ParserDefId) -> Vec<TypeId> {
        n_type_vars(db, id, self.defs.len() as u32)
    }
    pub fn fill_anon_vars(&mut self, db: &(impl TyHirs + ?Sized), target_count: u32) {
        for i in 1..=(target_count - self.defs.len() as u32) {
            let nth_name = format!("'{}", i);
            let tvar = db.intern_type_var(TypeVarName::new(nth_name));
            self.defs.push(tvar);
        }
    }
}

fn n_type_vars(db: &(impl TyHirs + ?Sized), id: hir::ParserDefId, n: u32) -> Vec<TypeId> {
    (0..n)
        .map(|i| db.intern_type(Type::TypeVarRef(id.0, 0, i)))
        .collect()
}

#[derive(Clone)]
pub struct TypingLocation {
    vars: TypeVarCollection,
    loc: HirId,
    pd: hir::ParserDefId,
}

impl TypingLocation {
    pub fn at_id(db: &(impl TyHirs + ?Sized), loc: HirId) -> Result<Self, ()> {
        let pd = db.hir_parent_parserdef(loc)?;
        let vars = TypeVarCollection::at_id(db, pd).map_err(|_| ())?;
        Ok(TypingLocation { vars, loc, pd })
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct TypedHirVal<T>(PhantomData<T>);

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct TypedAtom<Id> {
    ty: Id,
    span_index: hir::SpanIndex,
    atom: ParserAtom,
}

impl<T: Debug + Hash + Copy + Ord> ExpressionKind for TypedHirVal<T> {
    type BinaryOp = ValBinOp<Self, hir::HirConstraint, (T, hir::SpanIndex)>;
    type UnaryOp = ValUnOp<Self, (T, hir::SpanIndex)>;
    type Atom = TypedAtom<T>;
}

impl<T: Debug + Hash + Copy + Ord> Expression<TypedHirVal<T>> {
    pub fn root_type(&self) -> T {
        match self {
            Expression::BinaryOp(b) => match b.as_ref() {
                ValBinOp::Basic(_, _, _, t)
                | ValBinOp::Wiggle(_, _, t)
                | ValBinOp::Else(_, _, t)
                | ValBinOp::Dot(_, _, t) => t.0,
            },
            Expression::UnaryOp(u) => match u.as_ref() {
                ValUnOp::Not(_, t)
                | ValUnOp::Neg(_, t)
                | ValUnOp::Pos(_, t)
                | ValUnOp::If(_, t) => t.0,
            },
            Expression::Atom(a) => a.ty,
        }
    }
}

impl<T: Debug + std::hash::Hash + Copy + Ord> ExpressionComponent<TypedHirVal<T>> for TypedAtom<T> {
    fn children(&self) -> Vec<&Expression<TypedHirVal<T>>> {
        Vec::new()
    }
}

enum NominalId {
    Def(hir::ParserDefId),
    Block(hir::BlockId),
}

impl NominalId {
    pub fn from_nominal_inf_head(head: &NominalInfHead) -> Self {
        match head.kind {
            NominalKind::Def => NominalId::Def(hir::ParserDefId(head.def)),
            NominalKind::Block => NominalId::Block(hir::BlockId(head.def)),
        }
    }
}
