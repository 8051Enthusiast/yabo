use std::{collections::BTreeMap, fmt::Debug, hash::Hash, marker::PhantomData, sync::Arc};

use fxhash::FxHashMap;

use crate::{
    expr::{
        Atom, BasicValBinOp, ExprIter, Expression, ExpressionComponent, ExpressionKind, TypeBinOp,
        TypeUnOp, ValBinOp, ValUnOp,
    },
    hir::{HirIdWrapper, ParseStatement, ParserAtom},
    interner::{FieldName, HirId, TypeVar},
    types::{
        InfTypeId, InferenceContext, InferenceType, NominalInfHead, NominalKind, NominalTypeHead,
        Polarity, Signature, Type, TypeError, TypeId, TypeResolver, VarDef,
    },
};

use super::{
    recursion::FunctionSscId, refs::parserdef_ref, BlockId, ExprId, HirConstraint, HirNode,
    HirType, HirVal, Hirs, LetStatement, ParserDef, ParserDefId, SpanIndex, TypeAtom,
    TypePrimitive,
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
    let def_ids = db.lookup_intern_recursion_scc(id);
    let defs = def_ids
        .iter()
        .map(|fun: &ParserDefId| fun.lookup(db))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_| TypeError)?;

    let resolv = ReturnResolver::new(db);
    let mut ctx = TypingContext::new(db, resolv);

    for def in defs.iter() {
        // in this loop we do not directly the inference variables we are creating yet
        let deref = ctx.infctx.var();
        let retinf = ParserDefTypeInf { id: def.id, deref };
        ctx.infctx.tr.return_infs.insert(retinf.id.0, retinf);
    }
    for def in defs.iter() {
        // in this separate loop we do the actual type inference
        let expr = def.to.lookup(db).map_err(|_| TypeError)?.expr;
        let mut context = TypingLocation {
            vars: TypeVarCollection::at_id(db, def.id)?,
            loc: db.hir_parent_module(def.id.0).map_err(|_| TypeError)?.0,
            pd: def.id,
        };
        let ty = ctx.val_expression_type(&mut context, &expr)?.root_type();
        let sig = db.parser_args(def.id)?;
        if let Some(from) = sig.from {
            let inffrom = ctx.infctx.from_type(from);
            let deref = ctx.infctx.tr.return_infs[&def.id.0].deref;
            let parser_ty = ctx.infctx.parser(deref, inffrom);
            ctx.infctx.constrain(ty, parser_ty)?;
        };
    }
    let mut ret = Vec::new();
    for def in defs.iter() {
        // here we are finished with inference so we can convert to actual types
        let deref = ctx.inftype_to_concrete_type(ctx.infctx.tr.return_infs[&def.id.0].deref)?;
        ret.push(ParserDefType { id: def.id, deref })
    }
    Ok(ret)
}

pub fn parser_args(db: &dyn Hirs, id: ParserDefId) -> Result<Signature, TypeError> {
    let pd = id.lookup(db).map_err(|_| TypeError)?;
    let mut context = TypingLocation {
        vars: TypeVarCollection::new_empty(),
        loc: db.hir_parent_module(id.0).map_err(|_| TypeError)?.0,
        pd: id,
    };
    let arg_resolver = ArgResolver(db);
    let mut tcx = TypingContext::new(db, arg_resolver);
    let from_expr = pd.from.lookup(db).map_err(|_| TypeError)?;
    let from_infty = tcx.resolve_type_expr(&mut context, &from_expr.expr)?;
    let from_ty = tcx.inftype_to_concrete_type(from_infty)?;
    let args = Arc::new(vec![]);
    let thunk = db.intern_type(Type::Nominal(NominalTypeHead {
        kind: NominalKind::Def,
        def: id.0,
        parse_arg: Some(from_ty),
        fun_args: args.clone(),
        ty_args: Arc::new(vec![]),
    }));
    Ok(Signature {
        ty_args: context.vars.defs,
        from: Some(from_ty),
        args,
        thunk,
    })
}

pub fn public_expr_type(
    db: &dyn Hirs,
    loc: ExprId,
) -> Result<(Expression<TypedHirVal<TypeId>>, TypeId), ()> {
    let mut ctx = PublicResolver::new_typing_context_and_loc(db, loc.0)?;
    let mut typeloc = ctx.infctx.tr.tloc.clone();
    let parent = loc.0.parent(db);
    let surrounding_types = match db.hir_node(parent)? {
        HirNode::ParserDef(pd) => ctx.parserdef_types(&pd).map_err(|_| ())?,
        HirNode::Let(l) => ctx.let_statement_types(&l).map_err(|_| ())?,
        HirNode::Parse(p) => ctx.parse_statement_types(&p)?,
        _ => panic!("expected parse statement, let statement or parser def"),
    };
    let expr = loc.lookup(db)?;
    let expr = ctx
        .val_expression_type(&mut typeloc, &expr.expr)
        .map_err(|_| ())?;
    let root = expr.root_type();
    let into_ret = if let Some(ty) = surrounding_types.from_type {
        ctx.infctx.parser_apply(root, ty).map_err(|_| ())?
    } else {
        root
    };
    let ret = if let Some(real_ret) = surrounding_types.root_type {
        ctx.infctx.constrain(real_ret, into_ret).map_err(|_| ())?;
        real_ret
    } else {
        into_ret
    };
    let ret = ctx.inftype_to_concrete_type(ret).map_err(|_| ())?;
    Ok((ctx.expr_to_concrete_type(&expr).map_err(|_| ())?, ret))
}

pub fn public_type(db: &dyn Hirs, loc: HirId) -> Result<TypeId, ()> {
    let node = db.hir_node(loc)?;
    let ret = match node {
        HirNode::Let(l) => public_expr_type(db, l.expr)?.1,
        HirNode::Parse(p) => {
            db.public_expr_type(p.expr).map_err(|_| ())?.1
        }
        HirNode::ChoiceIndirection(ind) => {
            let mut ctx = PublicResolver::new_typing_context_and_loc(db, ind.parent_context.0)?;
            let ret = ctx.infctx.var();
            for (_, choice_id) in ind.choices.iter() {
                let choice_ty = db.public_type(*choice_id)?;
                let intfy = ctx.infctx.from_type(choice_ty);
                ctx.infctx.constrain(ret, intfy).map_err(|_| ())?;
            }
            ctx.inftype_to_concrete_type(ret).map_err(|_| ())?
        }
        _ => panic!("unexpected node type"),
    };
    Ok(ret)
}

/// finds the public ambient type (ie the type that gets applied as the from argument
/// to the parser) for the parser at loc, which is either inside a block, inside an array
/// or at the toplevel on a parserdef.
pub fn ambient_type(db: &dyn Hirs, loc: HirId) -> Result<TypeId, ()> {
    let parent_id = loc.parent(db);
    let expr = match db.hir_node(parent_id)? {
        HirNode::ParserDef(pd) => {
            let sig = db.parser_args(pd.id).map_err(|_| ())?;
            return Ok(sig.from.expect("parserdef has no from type"));
        }
        HirNode::Block(block) => match db.hir_node(block.id.0.parent(db))? {
            HirNode::Expr(e) => e,
            _ => panic!("expected parent of block to be an expr"),
        },
        HirNode::Array(array) => match db.hir_node(array.id.0.parent(db))? {
            HirNode::Expr(e) => e,
            _ => panic!("expected parent of array to be an expr"),
        },
        _ => panic!("unexpected node type"),
    };
    let ty_expr = db.public_expr_type(expr.id)?.0;

    let mut ty = None;
    for part in ExprIter::new(&ty_expr) {
        ty = Some(match part {
            Expression::Atom(a) => match &a.atom {
                ParserAtom::Block(block) if block.0 == loc => a.ty,
                ParserAtom::Array(array) if array.0 == loc => a.ty,
                _ => continue,
            },
            _ => continue,
        });
    }
    let ty = db.lookup_intern_type(ty.ok_or(())?);
    Ok(match ty {
        Type::ParserArg { arg, .. } => arg,
        _ => panic!("unexpected type"),
    })
}

pub struct ExpressionTypeConstraints {
    pub root_type: Option<InfTypeId>,
    pub from_type: Option<InfTypeId>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefType {
    pub id: ParserDefId,
    pub deref: TypeId,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefTypeInf {
    pub id: ParserDefId,
    pub deref: InfTypeId,
}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
struct BlockType {
    fields: BTreeMap<FieldName, TypeId>,
}

pub struct TypingContext<'a, TR: TypeResolver> {
    db: &'a dyn Hirs,
    infctx: InferenceContext<TR>,
}

#[derive(Clone)]
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
    pub fn at_id(db: &dyn Hirs, id: ParserDefId) -> Result<Self, TypeError> {
        let tys = db.parser_args(id)?.ty_args;
        let mut ret = Self::new_empty();
        for ty in tys.iter() {
            ret.get_var(ty.name);
        }
        ret.freeze();
        Ok(ret)
    }
}

#[derive(Clone)]
pub struct TypingLocation {
    vars: TypeVarCollection,
    loc: HirId,
    pd: ParserDefId,
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
        &mut self,
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
                            ty_args: Box::default(),
                        }))
                }
                TypeAtom::Array(a) => {
                    let inner = self.resolve_type_expr(context, &a.expr)?;
                    self.db
                        .intern_inference_type(InferenceType::Loop(a.direction, inner))
                }
                TypeAtom::TypeVar(v) => {
                    let var_idx = context.vars.get_var(Some(*v)).ok_or(TypeError)?;
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
    pub fn val_expression_type(
        &mut self,
        context: &mut TypingLocation,
        expr: &Expression<HirVal>,
    ) -> Result<Expression<TypedHirVal<InfTypeId>>, TypeError> {
        use BasicValBinOp::*;
        let mut ty_and_subexpr = |expr: &Expression<HirVal>| {
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
                    ParserAtom::Array(a) => {
                        let array = a.lookup(self.db).map_err(|_| TypeError)?;
                        let inner = array.expr.lookup(self.db).map_err(|_| TypeError)?.expr;
                        let inner_ty = self.val_expression_type(context, &inner)?.root_type();
                        self.infctx.array_call(array.direction, inner_ty)?
                    }
                    ParserAtom::Block(b) => self.infctx.block_call(b.0)?,
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
        self.infctx.to_type(infty, Polarity::Positive)
    }
    fn expr_to_concrete_type(
        &mut self,
        expr: &Expression<TypedHirVal<InfTypeId>>,
    ) -> Result<Expression<TypedHirVal<TypeId>>, TypeError> {
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

impl<'a> TypingContext<'a, PublicResolver<'a>> {
    pub fn parse_statement_types(
        &mut self,
        parse: &ParseStatement,
    ) -> Result<ExpressionTypeConstraints, ()> {
        let from = self.db.ambient_type(parse.id.0)?;
        let infty = self.infctx.from_type(from);
        Ok(ExpressionTypeConstraints {
            root_type: Some(infty),
            from_type: None,
        })
    }
    pub fn let_statement_types(
        &mut self,
        let_statement: &LetStatement,
    ) -> Result<ExpressionTypeConstraints, TypeError> {
        let ty = let_statement.ty;
        let ty_expr = ty.lookup(self.db).map_err(|_| TypeError)?.expr;
        let mut typeloc = self.infctx.tr.tloc.clone();
        let infty = self.resolve_type_expr(&mut typeloc, &ty_expr)?;
        Ok(ExpressionTypeConstraints {
            root_type: Some(infty),
            from_type: None,
        })
    }
    pub fn parserdef_types(
        &mut self,
        parserdef: &ParserDef,
    ) -> Result<ExpressionTypeConstraints, TypeError> {
        let ty_expr = parserdef.from.lookup(self.db).map_err(|_| TypeError)?.expr;
        let mut typeloc = self.infctx.tr.tloc.clone();
        let from = self.resolve_type_expr(&mut typeloc, &ty_expr)?;
        Ok(ExpressionTypeConstraints {
            root_type: None,
            from_type: Some(from),
        })
    }
}

pub struct ReturnResolver<'a> {
    db: &'a dyn Hirs,
    return_infs: FxHashMap<HirId, ParserDefTypeInf>,
}

impl<'a> ReturnResolver<'a> {
    #[must_use]
    pub fn new(db: &'a dyn Hirs) -> Self {
        Self {
            db,
            return_infs: Default::default(),
        }
    }
}

impl<'a> TypeResolver for ReturnResolver<'a> {
    type DB = dyn Hirs + 'a;

    fn db(&self) -> &Self::DB {
        self.db
    }

    fn field_type(&self, _: &NominalInfHead, _: FieldName) -> Result<TypeId, ()> {
        Ok(self.db.intern_type(Type::Unknown))
    }

    fn deref(&self, ty: &NominalInfHead) -> Result<Option<TypeId>, ()> {
        if self.return_infs.get(&ty.def).is_some() {
            Ok(Some(self.db.intern_type(Type::Unknown)))
        } else {
            let id = match NominalId::from_nominal_head(ty) {
                NominalId::Def(d) => d,
                NominalId::Block(_) => return Ok(None),
            };
            Ok(Some(self.db.parser_returns(id).map_err(|_| ())?.deref))
        }
    }

    fn signature(&self, ty: &NominalInfHead) -> Result<Signature, ()> {
        get_signature(self.db, ty)
    }

    fn lookup(&self, context: HirId, name: FieldName) -> Result<TypeId, ()> {
        get_thunk(self.db, context, name)
    }
}

pub struct PublicResolver<'a> {
    db: &'a dyn Hirs,
    tloc: TypingLocation,
}

impl<'a> PublicResolver<'a> {
    pub fn new(db: &'a dyn Hirs, tloc: TypingLocation) -> Self {
        Self { db, tloc }
    }
    pub fn new_typing_context_and_loc(
        db: &'a dyn Hirs,
        loc: HirId,
    ) -> Result<TypingContext<Self>, ()> {
        let typeloc = TypingLocation {
            vars: TypeVarCollection::new_empty(),
            loc: db.hir_parent_module(loc)?.0,
            pd: db.hir_parent_parserdef(loc)?,
        };
        let public_resolver = Self::new(db, typeloc);
        Ok(TypingContext::new(db, public_resolver))
    }
}

impl<'a> TypeResolver for PublicResolver<'a> {
    fn field_type(&self, _ty: &NominalInfHead, _name: FieldName) -> Result<TypeId, ()> {
        Ok(self.db.intern_type(Type::Unknown))
    }

    fn deref(&self, ty: &NominalInfHead) -> Result<Option<TypeId>, ()> {
        let id = match NominalId::from_nominal_head(ty) {
            NominalId::Def(d) => d,
            NominalId::Block(_) => return Ok(None),
        };
        Ok(Some(self.db.parser_returns(id).map_err(|_| ())?.deref))
    }

    fn signature(&self, ty: &NominalInfHead) -> Result<Signature, ()> {
        get_signature(self.db, ty)
    }

    fn lookup(&self, context: HirId, name: FieldName) -> Result<TypeId, ()> {
        get_thunk(self.db, context, name)
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
    fn field_type(&self, _ty: &NominalInfHead, _name: FieldName) -> Result<TypeId, ()> {
        Ok(self.0.intern_type(Type::Unknown))
    }

    fn deref(&self, _ty: &NominalInfHead) -> Result<Option<TypeId>, ()> {
        Ok(Some(self.0.intern_type(Type::Unknown)))
    }

    fn signature(&self, ty: &NominalInfHead) -> Result<Signature, ()> {
        get_signature(self.0, ty)
    }

    fn lookup(&self, _context: HirId, _name: FieldName) -> Result<TypeId, ()> {
        Ok(self.0.intern_type(Type::Unknown))
    }

    type DB = dyn Hirs + 'a;

    fn db(&self) -> &Self::DB {
        self.0
    }
}

fn get_thunk(db: &dyn Hirs, context: HirId, name: FieldName) -> Result<TypeId, ()> {
    let pd = parserdef_ref(db, context, name).map_err(|_| ())?;
    let args = db.parser_args(pd).map_err(|_| ())?;
    let mut ret = args.thunk;
    if let Some(from) = args.from {
        ret = db.intern_type(Type::ParserArg {
            result: ret,
            arg: from,
        })
    }
    if !args.args.is_empty() {
        ret = db.intern_type(Type::FunctionArg(ret, args.args.clone()))
    }
    Ok(ret)
}

fn get_signature(db: &dyn Hirs, ty: &NominalInfHead) -> Result<Signature, ()> {
    let id = match NominalId::from_nominal_head(ty) {
        NominalId::Def(d) => d,
        NominalId::Block(_) => panic!("attempted to extract signature directly from block"),
    };
    db.parser_args(id).map_err(|_| ())
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct TypedHirVal<T>(PhantomData<T>);

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct TypedAtom<Id> {
    ty: Id,
    span_index: SpanIndex,
    atom: ParserAtom,
}

impl<T: Debug + Hash + Copy + Ord> ExpressionKind for TypedHirVal<T> {
    type BinaryOp = ValBinOp<Self, HirConstraint, (T, SpanIndex)>;
    type UnaryOp = ValUnOp<Self, (T, SpanIndex)>;
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
def each[for[int] *> expr2] *> expr4 = {}
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
        assert_eq!(
            "for[<Var Ref (file[anonymous].expr3, 0, 0)>]",
            arg_type("expr3")
        );
        assert_eq!("each[for[int] *> file[anonymous].expr2]", arg_type("expr4"));
    }
    #[test]
    fn return_types() {
        let ctx = Context::mock(
            r#"
def for['t] *> nil = {}
def nil *> expr1 = {}
            "#,
        );
        let return_type = |name| {
            let p = ctx.parser(name);
            ctx.db
                .parser_returns(p)
                .unwrap()
                .deref
                .hir_to_string(&ctx.db)
        };
        eprintln!("{}", return_type("nil"));
        eprintln!("{}", return_type("expr1"));
    }
}
