use std::collections::BTreeMap;

use fxhash::FxHashMap;

use crate::{
    expr::{
        Atom, BasicValBinOp, Expression, ExpressionComponent, ExpressionKind, TypeBinOp, TypeUnOp,
        ValBinOp, ValUnOp,
    },
    hir::{HirIdWrapper, ParserAtom},
    interner::{FieldName, HirId, Identifier, PathComponent},
    types::{
        InfTypeId, InferenceContext, InferenceType, NominalInfHead, PrimitiveType, TypeError,
        TypeId, TypeInterner, TypeInternerDatabase, TypeResolver, VarDef,
    },
};

use super::{
    recursion::FunctionSscId,
    refs::{resolve_var_ref, VarType},
    HirConstraint, HirType, HirVal, Hirs, ParserDefId, SpanIndex, TypeAtom, TypeExpression,
    TypePrimitive, ValExpression,
};

pub fn type_signature(db: &dyn Hirs, id: HirId) -> Result<ParserDefType, TypeError> {
    todo!()
}

fn type_signature_ssc(
    db: &dyn Hirs,
    id: FunctionSscId,
) -> Result<Vec<ParserDefTypeInf>, TypeError> {
    let funs = db.lookup_intern_recursion_scc(id);
    let mut defs = funs
        .iter()
        .map(|fun: &ParserDefId| fun.lookup(db))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_| TypeError)?;

    todo!()
}

fn parse_arg_type(
    db: &dyn Hirs,
    expr: &Expression<HirType>,
    ty_vars: &mut Vec<VarDef>,
    name_idx: &mut FxHashMap<Identifier, usize>,
) -> Result<TypeId, TypeError> {
    match expr {
        Expression::BinaryOp(b) => {
            match b.as_ref() {
                TypeBinOp::Ref(_, _, _) => todo!(),
                TypeBinOp::ParseArg(_, _, _) => todo!(),
                TypeBinOp::Wiggle(i, _, _) => {
                    parse_arg_type(db, i, ty_vars, name_idx)
                }
            }
        }
        Expression::UnaryOp(_) => todo!(),
        Expression::Atom(a) => {
            db.intern_type(match &a.atom {
                TypeAtom::Primitive(TypePrimitive::Bit) => todo!(),
                TypeAtom::Primitive(_) => todo!(),
                TypeAtom::Id(_) => todo!(),
                TypeAtom::Array(_) => todo!(),
            });
            todo!()
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefType {
    pub ty: TypeId,
    pub deref: TypeId,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefTypeInf {
    pub id: ParserDefId,
    pub ty: InfTypeId,
    pub deref: InfTypeId,
}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
struct BlockType {
    fields: BTreeMap<FieldName, TypeId>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
enum Abstraction {
    Block(BlockType),
    ParserDef(ParserDefType),
}

pub fn public_type(db: &dyn Hirs, id: HirId) -> Option<TypeId> {
    //    TypingContext::new(db);
    todo!()
}

pub struct TypingContext<'a> {
    db: &'a dyn Hirs,
    infctx: InferenceContext<'a, (dyn Hirs + 'a)>,
}

impl<'a> TypingContext<'a> {
    fn new(db: &'a dyn Hirs, tr: &'a dyn TypeResolver) -> Self {
        TypingContext {
            db,
            infctx: InferenceContext::new(db, tr),
        }
    }

    fn is_public_ref(&self, id: HirId) -> bool {
        match self.db.lookup_intern_hir_path(id).path() {
            [PathComponent::File(_), PathComponent::Named(_)] => true,
            _ => false,
        }
    }
    fn resolve_type_ref(
        &mut self,
        context: HirId,
        name: FieldName,
    ) -> Result<InfTypeId, TypeError> {
        let (id, kind) = resolve_var_ref(self.db, context, name)
            .map_err(|_| TypeError)?
            .ok_or(TypeError)?;
        if let VarType::Value = kind {
            return Err(TypeError);
        }
        if !self.is_public_ref(id) {
            return Err(TypeError);
        }
        let ntype = type_signature(self.db, id)?.ty;
        let ty = self
            .infctx
            .from_type(&self.db.lookup_intern_type(ntype))
            .map_err(|_| TypeError)?;
        Ok(ty)
    }
    pub fn resolve_type_expr(
        &mut self,
        context: HirId,
        expr: &Expression<HirType>,
    ) -> Result<InfTypeId, TypeError> {
        let ret = match expr {
            Expression::BinaryOp(op) => match op.as_ref() {
                TypeBinOp::Wiggle(e, _, _) => self.resolve_type_expr(context, e)?,
                TypeBinOp::Ref(from, inner, _) => {
                    let from = self.resolve_type_expr(context, from)?;
                    let inner = self.resolve_type_expr(context, inner)?;
                    self.infctx.force_ref(from, inner)?;
                    inner
                }
                TypeBinOp::ParseArg(from_expr, inner, s) => {
                    let from = self.resolve_type_expr(context, from_expr)?;
                    let inner = self.resolve_type_expr(context, inner)?;
                    self.infctx.force_ref(from, inner)?;
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
                TypeAtom::Primitive(TypePrimitive::Int) => self.infctx.int(),
                TypeAtom::Primitive(TypePrimitive::Bit) => self.infctx.bit(),
                TypeAtom::Primitive(TypePrimitive::Char) => self.infctx.char(),
                TypeAtom::Id(ident) => self.resolve_type_ref(context, FieldName::Ident(*ident))?,
                TypeAtom::Array(a) => {
                    let inner = self.resolve_type_expr(context, &a.expr)?;
                    self.db.intern_inference_type(InferenceType::Loop(
                        a.direction,
                        inner,
                        self.infctx.var(),
                    ))
                }
                TypeAtom::Primitive(TypePrimitive::Mem) => todo!(),
            },
        };
        Ok(ret)
    }
    pub fn val_expression_type(
        &mut self,
        context: HirId,
        expr: &Expression<HirVal>,
    ) -> Result<Expression<InfTypedHirVal>, TypeError> {
        use BasicValBinOp::*;
        let mut ty_and_subexpr = |expr: &Expression<HirVal>| {
            self.val_expression_type(context, expr)
                .map(|e| (e.root_inftype(), e))
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
                    ParserAtom::Atom(Atom::Field(f)) => {
                        self.infctx.lookup(context, *f).ok_or(TypeError)?
                    }
                    ParserAtom::Array(a) => {
                        let array = a.lookup(self.db).map_err(|_| TypeError)?;
                        let inner = array.expr.lookup(self.db).map_err(|_| TypeError)?.expr;
                        let inner_ty = self.val_expression_type(context, &inner)?.root_inftype();
                        self.infctx.array_call(array.direction, inner_ty)?
                    }
                    ParserAtom::Block(b) => self.infctx.block_call(b.0)?,
                };
                Expression::Atom(InfTypedAtom {
                    index: a.span,
                    inftype,
                })
            }
        })
    }
}

pub struct PublicResolver<'a> {
    tcx: TypingContext<'a>,
}

impl<'a> TypeResolver for PublicResolver<'a> {
    fn field_type(&self, ty: &crate::types::NominalInfHead, name: FieldName) -> Option<InfTypeId> {
        todo!()
    }

    fn deref(&self, ty: &crate::types::NominalInfHead) -> Option<InferenceType> {
        todo!()
    }

    fn lookup(&self, _context: HirId, _name: FieldName) -> Option<InfTypeId> {
        todo!()
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct InfTypedHirVal;

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct InfTypedAtom {
    index: SpanIndex,
    inftype: InfTypeId,
}

impl ExpressionKind for InfTypedHirVal {
    type BinaryOp = ValBinOp<Self, HirConstraint, (InfTypeId, SpanIndex)>;
    type UnaryOp = ValUnOp<Self, (InfTypeId, SpanIndex)>;
    type Atom = InfTypedAtom;
}

impl Expression<InfTypedHirVal> {
    pub fn root_inftype(&self) -> InfTypeId {
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
            Expression::Atom(a) => a.inftype,
        }
    }
}

impl ExpressionComponent<InfTypedHirVal> for InfTypedAtom {
    fn children(&self) -> Vec<&Expression<InfTypedHirVal>> {
        Vec::new()
    }
}
