mod full;
mod public;
pub mod represent;
mod returns;
mod signature;

use std::{collections::BTreeMap, fmt::Debug, hash::Hash, sync::Arc};

use fxhash::FxHashMap;
use sha2::Digest;

use crate::{
    error::{SResult, Silencable},
    expr::{
        self, Atom, Dyadic, ExprIter, Expression, ExpressionHead, Monadic, OpWithData, TypeBinOp,
        TypeUnOp, ValBinOp, ValUnOp,
    },
    hash::StableHash,
    hir::{HirIdWrapper, ParseStatement, ParserAtom, ParserDefRef},
    interner::{DefId, FieldName, TypeVar, TypeVarName},
    resolve::{self, refs::parserdef_ref},
    source::SpanIndex,
    types::{
        inference::{InfTypeId, InferenceContext, InferenceType, NominalInfHead, TypeResolver},
        EitherType, NominalKind, NominalTypeHead, PrimitiveType, Signature, Type, TypeError,
        TypeId,
    },
};

use crate::hir::{self, Hirs};

use full::{parser_expr_at, parser_full_types, parser_type_at, ParserFullTypes};
use public::{ambient_type, public_expr_type, public_type};
pub use returns::IndirectionLevel;
use returns::{deref_type, least_deref_type, parser_returns, parser_returns_ssc, ParserDefType};
use signature::{get_signature, get_thunk, parser_args};

#[salsa::query_group(HirTypesDatabase)]
pub trait TyHirs: Hirs + crate::types::TypeInterner + resolve::Resolves {
    fn parser_args(&self, id: hir::ParserDefId) -> SResult<Signature>;
    fn parser_returns(&self, id: hir::ParserDefId) -> SResult<ParserDefType>;
    fn parser_returns_ssc(&self, id: resolve::parserdef_ssc::FunctionSscId) -> Vec<ParserDefType>;
    fn deref_type(&self, ty: TypeId) -> SResult<Option<TypeId>>;
    fn least_deref_type(&self, ty: TypeId) -> SResult<TypeId>;
    fn public_type(&self, loc: DefId) -> SResult<TypeId>;
    fn parser_type_at(&self, loc: DefId) -> SResult<TypeId>;
    fn parser_expr_at(&self, loc: hir::ExprId) -> SResult<TypedExpression>;
    fn public_expr_type(&self, loc: hir::ExprId) -> SResult<(TypedExpression, TypeId)>;
    fn ambient_type(&self, id: hir::ParseId) -> SResult<TypeId>;
    fn parser_full_types(&self, id: hir::ParserDefId) -> Result<Arc<ParserFullTypes>, TypeError>;
    fn type_hash(&self, ty: TypeId) -> [u8; 32];
    fn head_discriminant(&self, ty: TypeId) -> i64;
}

#[derive(Clone, Copy)]
pub enum HeadDiscriminant {
    Int = 0x100,
    Bit = 0x200,
    Char = 0x300,
    Loop = 0x400,
    Parser = 0x500,
    FunctionArgs = 0x600,
    Block = 0x700,
}

pub const DISCRIMINANT_MASK: i64 = !0x3;

pub fn head_discriminant(db: &dyn TyHirs, ty: TypeId) -> i64 {
    match db.lookup_intern_type(ty) {
        Type::Primitive(PrimitiveType::Int) => HeadDiscriminant::Int as i64,
        Type::Primitive(PrimitiveType::Bit) => HeadDiscriminant::Bit as i64,
        Type::Primitive(PrimitiveType::Char) => HeadDiscriminant::Char as i64,
        Type::Loop(_, _) => HeadDiscriminant::Loop as i64,
        Type::ParserArg { .. } => HeadDiscriminant::Parser as i64,
        Type::FunctionArg(_, _) => HeadDiscriminant::FunctionArgs as i64,
        Type::ForAll(inner, _) => db.head_discriminant(inner),
        Type::Nominal(NominalTypeHead {
            kind: NominalKind::Block,
            ..
        }) => HeadDiscriminant::Block as i64,
        Type::Nominal(NominalTypeHead {
            kind: NominalKind::Def,
            def,
            ..
        }) => {
            let def_hash: [u8; 8] = db.def_hash(def)[0..8].try_into().unwrap();
            // highest bit is set for nominal types (so that it is negative)
            // and the rest is derived from the first 8 bytes of the hash
            //
            // the lowest two bits are for flags so they get zeroed out
            i64::from_le_bytes(def_hash) & DISCRIMINANT_MASK | i64::MIN
        }
        Type::TypeVarRef(_, _, _) | Type::Any | Type::Bot | Type::Unknown => 0,
    }
}
type TypedExpression = Expression<TypedHirVal<(TypeId, SpanIndex)>>;
type InfTypedExpression = Expression<TypedHirVal<(InfTypeId, SpanIndex)>>;

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
        expr: &Expression<hir::HirTypeSpanned>,
    ) -> Result<InfTypeId, TypeError> {
        let ret = match &expr.0 {
            ExpressionHead::Dyadic(Dyadic {
                op,
                inner: [left, right],
            }) => match &op.inner {
                TypeBinOp::Ref => {
                    unimplemented!()
                }
                TypeBinOp::ParseArg => {
                    let from = self.resolve_type_expr(context, left)?;
                    let inner = match &right.0 {
                        ExpressionHead::Niladic(OpWithData {
                            inner: hir::TypeAtom::ParserDef(pd),
                            ..
                        }) => self.resolve_type_expr_parserdef_ref(context, pd, Some(from))?,
                        _ => self.resolve_type_expr(context, right)?,
                    };
                    self.db.intern_inference_type(InferenceType::ParserArg {
                        result: inner,
                        arg: from,
                    })
                }
            },
            ExpressionHead::Monadic(Monadic { op, inner }) => match &op.inner {
                TypeUnOp::Ref | TypeUnOp::Wiggle(_) => self.resolve_type_expr(context, inner)?,
            },
            ExpressionHead::Niladic(op) => match &op.inner {
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
        let def = parserdef_ref(self.db, context.loc, FieldName::Ident(pd.name.atom))?
            .ok_or(TypeError)?;
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
        expr: &Expression<hir::HirValSpanned>,
    ) -> Result<InfTypedExpression, TypeError> {
        use ValBinOp::*;
        expr.try_scan(&mut |expr| {
            Ok(match expr {
                ExpressionHead::Dyadic(Dyadic {
                    op,
                    inner: [&(left, _), &(right, _)],
                }) => (
                    match op.inner {
                        And | Xor | Or | ShiftR | ShiftL | Minus | Plus | Div | Modulo | Mul => {
                            let int = self.infctx.int();
                            self.infctx.constrain(left, int)?;
                            self.infctx.constrain(right, int)?;
                            int
                        }
                        LesserEq | Lesser | GreaterEq | Greater => {
                            let bit = self.infctx.bit();
                            self.infctx.constrain(left, bit)?;
                            self.infctx.constrain(right, bit)?;
                            bit
                        }
                        Else => self.infctx.one_of(&[left, right])?,
                        Compose => self.infctx.parser_compose(left, right)?,
                        ParserApply => self.infctx.parser_apply(right, left)?,
                        Uneq | Equals => todo!(),
                    },
                    op.data,
                ),
                ExpressionHead::Monadic(Monadic {
                    op,
                    inner: &(inner, _),
                }) => (
                    match &op.inner {
                        ValUnOp::Neg | ValUnOp::Pos | ValUnOp::Not => {
                            let int = self.infctx.int();
                            self.infctx.constrain(inner, int)?;
                            int
                        }
                        ValUnOp::Wiggle(_, _) => inner,
                        ValUnOp::Dot(name) => self.infctx.access_field(inner, *name)?,
                    },
                    op.data,
                ),
                ExpressionHead::Niladic(a) => (
                    match &a.inner {
                        ParserAtom::Atom(Atom::Char(_)) => self.infctx.char(),
                        ParserAtom::Atom(Atom::Number(_)) => self.infctx.int(),
                        ParserAtom::Atom(Atom::Field(f)) => self.infctx.lookup(context.loc, *f)?,
                        ParserAtom::Single => self.infctx.single(),
                        ParserAtom::Block(b) => {
                            let pd = self.db.hir_parent_parserdef(b.0)?;
                            let ty_vars = (0..context.vars.defs.len() as u32)
                                .map(|i| {
                                    self.db.intern_inference_type(InferenceType::TypeVarRef(
                                        pd.0, 0, i,
                                    ))
                                })
                                .collect::<Vec<_>>();
                            self.infctx.block_call(b.0, &ty_vars)?
                        }
                    },
                    a.data,
                ),
            })
        })
    }
    fn inftype_to_concrete_type(&mut self, infty: InfTypeId) -> Result<TypeId, TypeError> {
        self.infctx.to_type(infty)
    }
    fn expr_to_concrete_type(
        &mut self,
        expr: &InfTypedExpression,
    ) -> Result<TypedExpression, TypeError> {
        expr.try_map(&mut |(ty, span)| Ok((self.inftype_to_concrete_type(*ty)?, *span)))
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
    pub fn at_id(db: &(impl TyHirs + ?Sized), id: hir::ParserDefId) -> SResult<Self> {
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

fn type_hash(db: &dyn TyHirs, id: TypeId) -> [u8; 32] {
    let mut hasher = sha2::Sha256::new();
    id.update_hash(&mut hasher, db);
    hasher.finalize().try_into().unwrap()
}

#[derive(Clone)]
pub struct TypingLocation {
    vars: TypeVarCollection,
    loc: DefId,
    pd: hir::ParserDefId,
}

impl TypingLocation {
    pub fn at_id(db: &(impl TyHirs + ?Sized), loc: DefId) -> SResult<Self> {
        let pd = db.hir_parent_parserdef(loc)?;
        let vars = TypeVarCollection::at_id(db, pd)?;
        Ok(TypingLocation { vars, loc, pd })
    }
}

type TypedHirVal<T> = expr::KindWithData<hir::HirVal, T>;

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct TypeInfo<Id> {
    ty: Id,
    span_index: SpanIndex,
}

pub enum NominalId {
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
    pub fn from_nominal_head(head: &NominalTypeHead) -> Self {
        match head.kind {
            NominalKind::Def => NominalId::Def(hir::ParserDefId(head.def)),
            NominalKind::Block => NominalId::Block(hir::BlockId(head.def)),
        }
    }
}
