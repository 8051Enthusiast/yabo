use std::collections::BTreeMap;

use fxhash::FxHashMap;

use crate::{
    expr::{
        Atom, BasicValBinOp, Expression, ExpressionComponent, ExpressionKind, TypeBinOp, TypeUnOp,
        ValBinOp, ValUnOp,
    },
    hir::{HirIdWrapper, ParserAtom},
    interner::{FieldName, HirId, TypeVar},
    types::{
        InfTypeId, InferenceContext, InferenceType, LazyInfType, NominalInfHead, NominalKind,
        Polarity, Signature, TypeError, TypeId, TypeResolver, VarDef,
    },
};

use super::{
    recursion::FunctionSscId, refs::parserdef_ref, BlockId, HirConstraint, HirType, HirVal, Hirs,
    ParserDefId, SpanIndex, TypeAtom, TypePrimitive,
};

pub fn parser_returns(db: &dyn Hirs, id: ParserDefId) -> Result<ParserDefType, TypeError> {
    let rets = db.parser_returns_ssc(db.parser_ssc(id).map_err(|_| TypeError)?)?;
    for def in rets {
        if def.id.0 == id.0 {
            return Ok(def);
        }
    }
    panic!("Not included in ssc even though it should")
}

pub fn parser_returns_ssc(
    db: &dyn Hirs,
    id: FunctionSscId,
) -> Result<Vec<ParserDefType>, TypeError> {
    let funs = db.lookup_intern_recursion_scc(id);
    let _defs = funs
        .iter()
        .map(|fun: &ParserDefId| fun.lookup(db))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_| TypeError)?;

    todo!()
}

pub fn parser_args(db: &dyn Hirs, id: ParserDefId) -> Result<Signature, TypeError> {
    let pd = id.lookup(db).map_err(|_| TypeError)?;
    let mut context = TypingLocation {
        vars: TypeVarCollection::new_empty(),
        loc: db.hir_parent_module(id.0).map_err(|_| TypeError)?.0,
    };
    let arg_resolver = ArgResolver(db);
    let mut tcx = TypingContext::new(db, arg_resolver);
    let from_expr = pd.from.lookup(db).map_err(|_| TypeError)?;
    let from_infty = tcx.resolve_type_expr(&mut context, &from_expr.expr)?;
    let from_ty = tcx.to_concrete_type(from_infty)?;
    Ok(Signature {
        ty_args: context.vars.defs,
        from: Some(from_ty),
        args: vec![],
    })
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefType {
    pub id: ParserDefId,
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

pub fn public_type(_db: &dyn Hirs, _id: HirId) -> Option<TypeId> {
    //    TypingContext::new(db);
    todo!()
}

pub struct TypingContext<'a, TR: TypeResolver> {
    db: &'a dyn Hirs,
    infctx: InferenceContext<TR>,
}

pub struct TypeVarCollection {
    defs: Vec<VarDef>,
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
    pub fn get_var(&mut self, var_name: Option<TypeVar>) -> Option<u32> {
        if let Some(name) = var_name {
            if let Some(idx) = self.names.get(&name) {
                return Some(*idx);
            }
        }
        if self.frozen {
            return None;
        }
        let var_def = VarDef::new(var_name);
        let new_index = self.defs.len() as u32;
        self.defs.push(var_def);
        if let Some(name) = var_name {
            self.names.insert(name, new_index);
        }
        Some(new_index)
    }
    pub fn freeze(&mut self) {
        self.frozen = true
    }
}

pub struct TypingLocation {
    vars: TypeVarCollection,
    loc: HirId,
}

impl<'a, TR: TypeResolver> TypingContext<'a, TR> {
    fn new(db: &'a dyn Hirs, tr: TR) -> Self {
        TypingContext {
            db,
            infctx: InferenceContext::new(tr),
        }
    }

    /*
    fn resolve_type_ref(
        &mut self,
        context: &mut TypingLocation,
        name: FieldName,
    ) -> Result<InfTypeId, TypeError> {
        let id = self.parserdef_ref(context, name)?;
        let ntype = parser_returns(self.db, id)?.ty;
        let ty = self
            .infctx
            .from_type(&self.db.lookup_intern_type(ntype))
            .map_err(|_| TypeError)?;
        Ok(ty)
    }
    */
    pub fn resolve_type_expr(
        &self,
        context: &mut TypingLocation,
        expr: &Expression<HirType>,
    ) -> Result<InfTypeId, TypeError> {
        let ret = match expr {
            Expression::BinaryOp(op) => match op.as_ref() {
                TypeBinOp::Wiggle(e, _, _) => self.resolve_type_expr(context, e)?,
                TypeBinOp::Ref(_, _, _) => {
                    unimplemented!()
                }
                TypeBinOp::ParseArg(from_expr, inner, _) => {
                    let from = self.resolve_type_expr(context, from_expr)?;
                    let inner = self.resolve_type_expr(context, inner)?;
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
                TypeAtom::Primitive(TypePrimitive::Mem) => todo!(),
                TypeAtom::ParserDef(pd) => {
                    let parse_arg = pd
                        .from
                        .as_ref()
                        .map(|x| self.resolve_type_expr(context, x))
                        .transpose()?;
                    let fun_args = Box::new(
                        pd.args
                            .iter()
                            .map(|x| self.resolve_type_expr(context, x))
                            .collect::<Result<Vec<_>, _>>()?,
                    );
                    let def = parserdef_ref(self.db, context.loc, FieldName::Ident(pd.name.atom))
                        .map_err(|_| TypeError)?
                        .0;
                    self.db
                        .intern_inference_type(InferenceType::Nominal(NominalInfHead {
                            kind: NominalKind::Def,
                            def,
                            parse_arg,
                            fun_args,
                        }))
                }
                TypeAtom::Array(a) => {
                    let inner = self.resolve_type_expr(context, &a.expr)?;
                    self.db
                        .intern_inference_type(InferenceType::Loop(a.direction, inner))
                }
                TypeAtom::TypeVar(v) => {
                    let var_idx = context.vars.get_var(Some(*v)).ok_or(TypeError)?;
                    self.db
                        .intern_inference_type(InferenceType::TypeVarRef(0, var_idx))
                }
            },
        };
        Ok(ret)
    }
    pub fn val_expression_type(
        &mut self,
        context: &mut TypingLocation,
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
                        self.infctx.lookup(context.loc, *f).ok_or(TypeError)?
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
    fn to_concrete_type(&mut self, infty: InfTypeId) -> Result<TypeId, TypeError> {
        self.infctx.to_type(infty, Polarity::Positive)
    }
}

pub struct PublicResolver<'a> {
    db: &'a dyn Hirs,
}

impl<'a> TypeResolver for PublicResolver<'a> {
    fn field_type(&self, _ty: &NominalInfHead, _name: FieldName) -> Option<LazyInfType> {
        None
    }

    fn deref(&self, ty: &NominalInfHead) -> Option<LazyInfType> {
        let id = match NominalId::from_nominal_head(ty) {
            NominalId::Def(d) => d,
            NominalId::Block(_) => return None,
        };
        Some(LazyInfType::Type(self.db.parser_returns(id).ok()?.deref))
    }

    fn signature(&self, ty: &NominalInfHead) -> Option<Signature> {
        let id = match NominalId::from_nominal_head(ty) {
            NominalId::Def(d) => d,
            NominalId::Block(_) => panic!("Attempting to extract signature directly from block"),
        };
        parser_args(self.db, id).ok()
    }

    fn lookup(&self, context: HirId, name: FieldName) -> Option<LazyInfType> {
        let pd = parserdef_ref(self.db, context, name).ok()?;
        Some(LazyInfType::Type(self.db.parser_returns(pd).ok()?.ty))
    }

    type DB = dyn Hirs + 'a;

    fn db(&self) -> &Self::DB {
        self.db
    }
}

pub struct ArgResolver<'a>(&'a dyn Hirs);

impl<'a> ArgResolver<'a> {
    pub fn new(db: &'a dyn Hirs) -> Self {
        ArgResolver(db)
    }
}

impl<'a> TypeResolver for ArgResolver<'a> {
    fn field_type(&self, _ty: &NominalInfHead, _name: FieldName) -> Option<LazyInfType> {
        None
    }

    fn deref(&self, _ty: &NominalInfHead) -> Option<LazyInfType> {
        None
    }

    fn signature(&self, ty: &NominalInfHead) -> Option<Signature> {
        let id = match NominalId::from_nominal_head(ty) {
            NominalId::Def(d) => d,
            NominalId::Block(_) => panic!("attempted to extract signature directly from block"),
        };
        parser_args(self.0, id).ok()
    }

    fn lookup(&self, _context: HirId, _name: FieldName) -> Option<LazyInfType> {
        None
    }

    type DB = dyn Hirs + 'a;

    fn db(&self) -> &Self::DB {
        self.0
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

enum NominalId {
    Def(ParserDefId),
    Block(BlockId),
}

impl NominalId {
    fn from_nominal_head(head: &NominalInfHead) -> Self {
        match head.kind {
            NominalKind::Def => NominalId::Def(ParserDefId(head.def)),
            NominalKind::Block => NominalId::Block(BlockId(head.def)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::represent::HirToString;
    use crate::context::Context;

    use super::*;
    #[test]
    fn arg_types() {
        let ctx = Context::mock(
            r#"
def for[int] *> expr1 = {}
def for[for[int] &> expr1] *> expr2 = {}
def for['x] *> expr3 = {}
        "#,
        );
        let arg_type = |name| {
            let p = ctx.parser(name);
            ctx.db
                .parser_args(p)
                .unwrap()
                .from
                .unwrap()
                .hir_to_string(&ctx.db)
        };
        assert_eq!("for[int]", arg_type("expr1"));
        assert_eq!("for[for[int] &> file[anonymous].expr1]", arg_type("expr2"));
        assert_eq!("for[<Var Ref (0, 0)>]", arg_type("expr3"));
    }
}
