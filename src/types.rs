use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    sync::Arc,
};

use salsa::InternId;

use crate::{
    ast::ArrayKind,
    interner::{HirId, Identifier},
};

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TypeId(InternId);
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct InfTypeId(InternId);

impl salsa::InternKey for TypeId {
    fn from_intern_id(v: InternId) -> Self {
        TypeId(v)
    }

    fn as_intern_id(&self) -> InternId {
        self.0
    }
}

impl salsa::InternKey for InfTypeId {
    fn from_intern_id(v: InternId) -> Self {
        InfTypeId(v)
    }

    fn as_intern_id(&self) -> InternId {
        self.0
    }
}

#[salsa::query_group(TypeInterner)]
pub trait TyInterner: crate::source::Files {
    #[salsa::interned]
    fn intern_type(&self, ty: Type) -> TypeId;
    #[salsa::interned]
    fn intern_inference_type(&self, ty: InferenceType) -> InfTypeId;
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Any,
    Bot,
    Error,
    Primitive(PrimitiveType),
    TypeVarRef(u32, u32),
    ForAll(TypeId, Arc<Vec<VarDef>>),
    Nominal {
        kind: NominalKind,
        def: HirId,
        type_args: Arc<Vec<TypeId>>,
    },
    Loop(ArrayKind, TypeId, TypeId),
    ParserArg(TypeId, TypeId),
    FunctionArg(TypeId, Arc<Vec<TypeId>>),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarId(usize);

impl VarId {
    fn usize(&self) -> usize {
        self.0
    }
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum InferenceType {
    Any,
    Bot,
    Primitive(PrimitiveType),
    Var(VarId),
    Unknown(VarId),
    Nominal {
        kind: NominalKind,
        def: HirId,
        type_args: Box<Vec<InfTypeId>>,
    },
    Loop(ArrayKind, InfTypeId, InfTypeId),
    ParserArg {
        result: InfTypeId,
        arg: InfTypeId,
    },
    FunctionArgs {
        result: InfTypeId,
        args: Box<Vec<InfTypeId>>,
    },
    InferField(Identifier, InfTypeId),
}

impl InferenceType {
    fn children(&mut self, pol: Polarity) -> Vec<(&mut InfTypeId, Polarity)> {
        match self {
            InferenceType::Loop(_, from, inner) => {
                vec![(from, pol), (inner, pol)]
            }
            InferenceType::ParserArg { result, arg } => {
                vec![(result, pol), (arg, pol.reverse())]
            }
            InferenceType::FunctionArgs { result, args } => {
                let mut ret = args
                    .iter_mut()
                    .map(|x| (x, pol.reverse()))
                    .collect::<Vec<_>>();
                ret.push((result, pol));
                ret
            }
            InferenceType::InferField(_, inner) => {
                vec![(inner, pol)]
            }
            InferenceType::Var(..) | InferenceType::Unknown(..) => {
                panic!("Internal Compiler Error: taking children of variable");
            }
            _ => vec![],
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeHead {
    Any,
    Bot,
    Primitive(PrimitiveType),
    Nominal(HirId),
    Loop(ArrayKind),
    ParserArg,
    FunctionArgs,
}

impl TryFrom<&InferenceType> for TypeHead {
    type Error = TypeError;

    fn try_from(value: &InferenceType) -> Result<Self, Self::Error> {
        Ok(match value {
            InferenceType::Any => TypeHead::Any,
            InferenceType::Bot => TypeHead::Bot,
            InferenceType::Primitive(p) => TypeHead::Primitive(*p),
            InferenceType::Nominal { def, .. } => TypeHead::Nominal(*def),
            InferenceType::Loop(kind, _, _) => TypeHead::Loop(*kind),
            InferenceType::ParserArg { .. } => TypeHead::ParserArg,
            InferenceType::FunctionArgs { .. } => TypeHead::FunctionArgs,
            InferenceType::Var(_) | InferenceType::Unknown(_) | InferenceType::InferField(..) => {
                return Err(TypeError)
            }
        })
    }
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct InferenceVar {
    id: usize,
    upper: Vec<InfTypeId>,
    lower: Vec<InfTypeId>,
}

impl InferenceVar {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            upper: Vec::new(),
            lower: Vec::new(),
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum PrimitiveType {
    Bit,
    Int,
    Char,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum NominalKind {
    Def,
    Block,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VarDef {
    name: Option<Identifier>,
    defloc: Option<HirId>,
}

#[derive(Clone, Default)]
pub struct VarStore {
    vars: Vec<InferenceVar>,
}

impl VarStore {
    pub fn new() -> Self {
        Self::default()
    }
    fn add_var(&mut self) -> VarId {
        let id = self.vars.len();
        self.vars.push(InferenceVar::new(id));
        VarId(id)
    }
    fn get(&self, id: VarId) -> &InferenceVar {
        &self.vars[id.usize()]
    }
    fn get_mut(&mut self, id: VarId) -> &mut InferenceVar {
        &mut self.vars[id.usize()]
    }
}

pub struct InferenceContext<'db> {
    var_store: VarStore,
    db: &'db dyn TyInterner,
    cache: HashSet<(InfTypeId, InfTypeId)>,
}

pub trait TypingContext {
    type DB: TyInterner + ?Sized;

    fn infctx_mut(&mut self) -> &mut InferenceContext;
    fn infctx(&self) -> &InferenceContext;
    fn field_type(&self, ty: &InferenceType, name: Identifier) -> InfTypeId;
    fn deref(&self, ty: &InferenceType) -> Option<InferenceType>;
    fn constrain(&mut self, lower: InfTypeId, upper: InfTypeId) -> Result<(), TypeError> {
        use InferenceType::*;
        if !self.infctx_mut().cache.insert((lower, upper)) {
            return Ok(());
        }
        let [lower_ty, upper_ty] =
            [lower, upper].map(|x| self.infctx().db.lookup_intern_inference_type(x));
        match (lower_ty, upper_ty) {
            (_, Any) => Ok(()),

            (Bot, _) => Ok(()),

            (Primitive(l), Primitive(u)) if l == u => Ok(()),
            (
                FunctionArgs {
                    result: res1,
                    args: args1,
                },
                FunctionArgs {
                    result: res2,
                    args: args2,
                },
            ) => {
                self.constrain(res1, res2)?;
                if args1.len() != args2.len() {
                    return Err(TypeError);
                }
                for (&arg1, &arg2) in args1.iter().zip(args2.iter()) {
                    self.constrain(arg2, arg1)?;
                }
                Ok(())
            }

            (
                ParserArg {
                    result: res1,
                    arg: arg1,
                },
                ParserArg {
                    result: res2,
                    arg: arg2,
                },
            ) => {
                self.constrain(res1, res2)?;
                self.constrain(arg2, arg1)
            }

            (Var(var_id) | Unknown(var_id), _) => {
                self.infctx_mut()
                    .var_store
                    .get_mut(var_id)
                    .upper
                    .push(upper);
                // idx is needed because var.lower may change during the loop
                let mut idx = 0;
                while let Some(&lower_bound) =
                    self.infctx_mut().var_store.get(var_id).lower.get(idx)
                {
                    idx += 1;
                    self.constrain(lower_bound, upper)?;
                }
                Ok(())
            }

            (_, Var(var_id) | Unknown(var_id)) => {
                self.infctx_mut()
                    .var_store
                    .get_mut(var_id)
                    .lower
                    .push(lower);
                // idx is needed because var.lower may change during the loop
                let mut idx = 0;
                while let Some(&upper_bound) =
                    self.infctx_mut().var_store.get(var_id).upper.get(idx)
                {
                    idx += 1;
                    self.constrain(upper_bound, lower)?;
                }
                Ok(())
            }

            (Loop(kind1, from1, inner1), Loop(kind2, from2, inner2)) => {
                if matches!((kind1, kind2), (ArrayKind::For, ArrayKind::Each)) {
                    return Err(TypeError);
                }
                self.constrain(from1, from2)?;
                self.constrain(inner1, inner2)
            }

            (InferField(name1, inner1), InferField(name2, inner2)) => {
                if name1 != name2 {
                    return Ok(());
                }
                self.constrain(inner1, inner2)
            }

            (ref nom @ Nominal { .. }, InferField(name, fieldtype)) => {
                let field_ty = self.field_type(&nom, name);
                self.constrain(field_ty, fieldtype)
            }

            (
                Nominal {
                    def: def1,
                    type_args: type_args1,
                    ..
                },
                Nominal {
                    def: def2,
                    type_args: type_args2,
                    ..
                },
            ) if def1 == def2 => {
                if type_args1.len() != type_args2.len() {
                    panic!("Internal Compiler Error: compared same nominal types do not have same number of type args");
                }
                type_args1
                    .iter()
                    .zip(type_args2.iter())
                    .map(|(&arg1, &arg2)| self.constrain(arg1, arg2))
                    .collect()
            }

            (l @ Nominal { .. }, _) => {
                // if they are not the same head, try upcasting/dereferencing/evaluating
                if let Some(deref) = self
                    .deref(&l)
                    .map(|x| self.infctx().db.intern_inference_type(x))
                {
                    self.constrain(deref, upper)
                } else {
                    return Err(TypeError);
                }
            }

            _ => Err(TypeError),
        }
    }
    fn var(&mut self) -> InfTypeId {
        let inftype = InferenceType::Var(self.infctx_mut().var_store.add_var());
        self.infctx().db.intern_inference_type(inftype)
    }
    fn unknown(&mut self) -> InfTypeId {
        let inftype = InferenceType::Unknown(self.infctx_mut().var_store.add_var());
        self.infctx().db.intern_inference_type(inftype)
    }
    fn parser_apply(&mut self, parser: InfTypeId, arg: InfTypeId) -> Result<InfTypeId, TypeError> {
        let var = self.var();
        let new_parser = self
            .infctx()
            .db
            .intern_inference_type(InferenceType::ParserArg { arg, result: var });
        self.constrain(parser, new_parser)?;
        Ok(var)
    }
    fn function_apply(
        &mut self,
        function: InfTypeId,
        args: &[InfTypeId],
    ) -> Result<InfTypeId, TypeError> {
        let var = self.var();
        let new_function = InferenceType::FunctionArgs {
            args: Box::new(Vec::from(args)),
            result: var,
        };
        let new_function = self.infctx().db.intern_inference_type(new_function);
        self.constrain(function, new_function)?;
        Ok(var)
    }
    fn access_field(
        &mut self,
        accessed: InfTypeId,
        name: Identifier,
    ) -> Result<InfTypeId, TypeError> {
        let var = self.var();
        let infer_access = self
            .infctx()
            .db
            .intern_inference_type(InferenceType::InferField(name, var));
        self.constrain(accessed, infer_access)?;
        Ok(var)
    }
    fn from_type(&mut self, ty: &Type) -> Result<InfTypeId, TypeError> {
        from_type_internal(self, ty, None)
    }
    fn from_type_with_args<'a>(
        &mut self,
        ty: &Type,
        args: &'a [InfTypeId],
    ) -> Result<InfTypeId, TypeError> {
        match ty {
            // we ignore the args here and replace with our own args
            Type::ForAll(inner, _args) => {
                if args.len() != _args.len() {
                    panic!("Internal Compiler Error: forall args has different length from substituted args!");
                }
                let vars = VarStack {
                    cur: args,
                    next: None,
                };
                let inner = self.infctx().db.lookup_intern_type(*inner);
                from_type_internal(self, &inner, Some(&vars))
            }
            _ => from_type_internal(self, ty, None),
        }
    }
    fn to_type(&self, infty: InfTypeId, pol: Polarity) -> Result<TypeId, TypeError> {
        to_type_internal(self, infty, pol, &mut TypeConvertMemo::new(self.infctx()))
    }
}

pub struct TypeError;

impl<'a> InferenceContext<'a> {
    pub fn new(db: &'a dyn TyInterner) -> Self {
        Self {
            var_store: VarStore::new(),
            db,
            cache: HashSet::new(),
        }
    }
}

struct VarStack<'a> {
    cur: &'a [InfTypeId],
    next: Option<&'a VarStack<'a>>,
}

impl<'a> VarStack<'a> {
    fn resolve(&'a self, level: u32, index: u32) -> Option<InfTypeId> {
        match level {
            0 => self.cur.get(index as usize).copied(),
            1.. => self.next?.resolve(level - 1, index),
        }
    }
}

#[derive(Debug)]
pub struct MemoRecursor<From: Copy + Eq + Hash, To: Copy> {
    process: HashMap<From, usize>,
    memo: HashMap<From, To>,
}

impl<From: Copy + Eq + Hash, To: Copy> Default for MemoRecursor<From, To> {
    fn default() -> Self {
        Self {
            process: Default::default(),
            memo: Default::default(),
        }
    }
}

impl<From: Copy + Eq + Hash, To: Copy> MemoRecursor<From, To> {
    fn enter_fun(&mut self, from: From) -> Option<Result<To, TypeError>> {
        if let Some(t) = self.memo.get(&from) {
            return Some(Ok(t.clone()));
        }
        let depth = self.process.len();
        if let Some(_) = self.process.insert(from, depth) {
            return Some(Err(TypeError));
        }
        None
    }
    fn leave_fun(&mut self, from: From, to: To) -> Result<To, TypeError> {
        self.memo.insert(from, to);
        self.process.remove(&from);
        Ok(to)
    }
}

pub struct TypeConvertMemo<'a> {
    convert: MemoRecursor<(InfTypeId, Polarity), TypeId>,
    normalize: MemoRecursor<(InfTypeId, Polarity), InfTypeId>,
    meet: MemoRecursor<(InfTypeId, InfTypeId), InfTypeId>,
    join: MemoRecursor<(InfTypeId, InfTypeId), InfTypeId>,
    // immutable reference to InferenceContext to make sure the memoized values remain valid
    _immutable_ref: &'a InferenceContext<'a>,
}

impl<'a> TypeConvertMemo<'a> {
    pub fn new(ctx: &'a InferenceContext) -> Self {
        fn default<T: Default>() -> T {
            T::default()
        }
        TypeConvertMemo {
            convert: default(),
            normalize: default(),
            meet: default(),
            join: default(),
            _immutable_ref: ctx,
        }
    }
}

fn meet_inftype<'a, TCX: TypingContext + ?Sized>(
    ctx: &TCX,
    lhs: InfTypeId,
    rhs: InfTypeId,
    memo: &'a mut TypeConvertMemo,
) -> Result<InfTypeId, TypeError> {
    use InferenceType::*;
    if let Some(x) = memo.join.enter_fun((lhs, rhs)) {
        return x;
    }
    let [lhs_ty, rhs_ty] = [lhs, rhs].map(|x| ctx.infctx().db.lookup_intern_inference_type(x));
    match (lhs_ty, rhs_ty) {
        (Bot, _) | (_, Bot) => Bot,
        (InferField(..), InferField(..)) => Any,
        (Any | InferField(..), other) | (other, Any | InferField(..)) => other.clone(),
        (Primitive(p), Primitive(q)) if p == q => Primitive(p),
        (Loop(kind1, from1, inner1), Loop(kind2, from2, inner2)) => {
            let kind = kind1.min(kind2);
            let from = meet_inftype(ctx, from1, from2, memo)?;
            let inner = meet_inftype(ctx, inner1, inner2, memo)?;
            Loop(kind, from, inner)
        }
        (ParserArg { result: r1, arg: a }, ParserArg { result: r2, arg: b }) => {
            let result = meet_inftype(ctx, r1, r2, memo)?;
            let arg = join_inftype(ctx, a, b, memo)?;
            ParserArg { result, arg }
        }
        (
            FunctionArgs {
                result: result1,
                args: args1,
            },
            FunctionArgs {
                result: result2,
                args: args2,
            },
        ) => {
            let result = meet_inftype(ctx, result1, result2, memo)?;
            if args1.len() != args2.len() {
                return Err(TypeError);
            }
            let args = Box::new(
                args1
                    .iter()
                    .zip(args2.iter())
                    .map(|(arg1, arg2)| join_inftype(ctx, *arg1, *arg2, memo))
                    .collect::<Result<_, _>>()?,
            );
            FunctionArgs { result, args }
        }
        (
            Nominal {
                kind,
                def: def1,
                type_args: type_args1,
            },
            Nominal {
                def: def2,
                type_args: type_args2,
                ..
            },
        ) if def1 == def2 => {
            let type_args = Box::new(
                type_args1
                    .iter()
                    .zip(type_args2.iter())
                    .map(|(arg1, arg2)| meet_inftype(ctx, *arg1, *arg2, memo))
                    .collect::<Result<_, _>>()?,
            );
            Nominal {
                kind: kind,
                def: def1,
                type_args,
            }
        }
        (nom @ Nominal { .. }, other) | (other, nom @ Nominal { .. }) => {
            let nomhead = TypeHead::try_from(&nom)?;
            let otherhead = TypeHead::try_from(&other)?;
            let next = |ty: &_| ctx.deref(ty);
            let mut ret = |a, b| {
                let [a_id, b_id] = [a, b].map(|x| ctx.infctx().db.intern_inference_type(x));
                meet_inftype(ctx, a_id, b_id, memo).and_then(|x| memo.meet.leave_fun((lhs, rhs), x))
            };
            for nom_ty in std::iter::successors(Some(nom.clone()), next) {
                if TypeHead::try_from(&nom_ty)? == otherhead {
                    return ret(nom_ty, other);
                }
            }
            for other_ty in std::iter::successors(Some(other), next) {
                if TypeHead::try_from(&other_ty)? == nomhead {
                    return ret(other_ty, nom);
                }
            }
            return Err(TypeError);
        }
        _ => return Err(TypeError),
    };
    todo!()
}

fn join_inftype<'a, TCX: TypingContext + ?Sized>(
    ctx: &TCX,
    lhs: InfTypeId,
    rhs: InfTypeId,
    memo: &'a mut TypeConvertMemo,
) -> Result<InfTypeId, TypeError> {
    use InferenceType::*;
    if let Some(x) = memo.join.enter_fun((lhs, rhs)) {
        return x;
    }
    let [other, nom] = [lhs, rhs].map(|x| ctx.infctx().db.lookup_intern_inference_type(x));
    let res = match (other, nom) {
        (Any, _) | (_, Any) => Any,
        (InferField(..), InferField(..)) => Bot,
        (Bot | InferField(..), other) | (other, Bot | InferField(..)) => other.clone(),
        (Primitive(p), Primitive(q)) if p == q => Primitive(p),
        (Loop(kind1, from1, inner1), Loop(kind2, from2, inner2)) => {
            let kind = kind1.max(kind2);
            let from = join_inftype(ctx, from1, from2, memo)?;
            let inner = join_inftype(ctx, inner1, inner2, memo)?;
            Loop(kind, from, inner)
        }
        (ParserArg { result: r1, arg: a }, ParserArg { result: r2, arg: b }) => {
            let result = join_inftype(ctx, r1, r2, memo)?;
            let arg = meet_inftype(ctx, a, b, memo)?;
            ParserArg { result, arg }
        }
        (
            FunctionArgs {
                result: result1,
                args: args1,
            },
            FunctionArgs {
                result: result2,
                args: args2,
            },
        ) => {
            let result = join_inftype(ctx, result1, result2, memo)?;
            if args1.len() != args2.len() {
                return Err(TypeError);
            }
            let args = Box::new(
                args1
                    .iter()
                    .zip(args2.iter())
                    .map(|(arg1, arg2)| meet_inftype(ctx, *arg1, *arg2, memo))
                    .collect::<Result<_, _>>()?,
            );
            FunctionArgs { result, args }
        }
        (
            Nominal {
                kind,
                def: def1,
                type_args: type_args1,
            },
            Nominal {
                def: def2,
                type_args: type_args2,
                ..
            },
        ) if def1 == def2 => {
            let type_args = Box::new(
                type_args1
                    .iter()
                    .zip(type_args2.iter())
                    .map(|(arg1, arg2)| join_inftype(ctx, *arg1, *arg2, memo))
                    .collect::<Result<_, _>>()?,
            );
            Nominal {
                kind: kind,
                def: def1,
                type_args,
            }
        }
        (nom @ Nominal { .. }, other) | (other, nom @ Nominal { .. }) => {
            let mut other_upset = HashMap::new();
            let next = |ty: &_| ctx.deref(ty);
            for other_ty in std::iter::successors(Some(other), next) {
                other_upset.insert(TypeHead::try_from(&other_ty)?, other_ty);
            }
            let mut res = None;
            for nom_ty in std::iter::successors(Some(nom), next) {
                if let Some(other_ty) = other_upset.remove(&TypeHead::try_from(&nom_ty)?) {
                    let other = ctx.infctx().db.intern_inference_type(other_ty);
                    let nom = ctx.infctx().db.intern_inference_type(nom_ty);
                    res = Some(join_inftype(ctx, other, nom, memo)?);
                    break;
                } else {
                    res = None
                }
            }
            if let Some(res) = res {
                ctx.infctx().db.lookup_intern_inference_type(res)
            } else {
                return Err(TypeError);
            }
        }
        _ => return Err(TypeError),
    };
    let ret = ctx.infctx().db.intern_inference_type(res);
    memo.join.leave_fun((lhs, rhs), ret)
}

fn normalize_children<TCX: TypingContext + ?Sized>(
    ctx: &TCX,
    infty: InfTypeId,
    pol: Polarity,
    memo: &mut TypeConvertMemo,
) -> Result<InfTypeId, TypeError> {
    let mut ty = ctx.infctx().db.lookup_intern_inference_type(infty);
    if let InferenceType::Var(_) | InferenceType::Unknown(_) = ty {
        ty = match pol {
            Polarity::Positive => InferenceType::Bot,
            Polarity::Negative => InferenceType::Any,
        };
    } else {
        for (child, pol) in ty.children(pol) {
            *child = normalize_inftype(ctx, *child, pol, memo)?;
        }
    }
    Ok(ctx.infctx().db.intern_inference_type(ty))
}

fn normalize_inftype<'a, TCX: TypingContext + ?Sized>(
    ctx: &TCX,
    infty: InfTypeId,
    pol: Polarity,
    memo: &'a mut TypeConvertMemo,
) -> Result<InfTypeId, TypeError> {
    if let Some(x) = memo.normalize.enter_fun((infty, pol)) {
        return x;
    }
    let inf = ctx.infctx().db.lookup_intern_inference_type(infty);
    let res = match (inf, pol) {
        (InferenceType::Var(v) | InferenceType::Unknown(v), Polarity::Positive) => {
            let mut result = ctx.infctx().db.intern_inference_type(InferenceType::Bot);
            for ty in ctx.infctx().var_store.get(v).lower.iter() {
                let normalized = normalize_children(ctx, *ty, pol, memo)?;
                result = join_inftype(ctx, result, normalized, memo)?;
            }
            result
        }
        (InferenceType::Var(v) | InferenceType::Unknown(v), Polarity::Negative) => {
            let mut result = ctx.infctx().db.intern_inference_type(InferenceType::Any);
            for ty in ctx.infctx().var_store.get(v).upper.iter() {
                let normalized = normalize_children(ctx, *ty, pol, memo)?;
                result = meet_inftype(ctx, result, normalized, memo)?;
            }
            // the result we got upon here is the most general, however in the upper bounds there can be
            // types like InferField which are more like type classes, so we join with the lower bounds,
            // because the join operation still prioritizes concrete types over type classes
            for ty in ctx.infctx().var_store.get(v).lower.iter() {
                let normalized = normalize_children(ctx, *ty, pol, memo)?;
                result = join_inftype(ctx, result, normalized, memo)?;
            }
            result
        }
        (_, _) => normalize_children(ctx, infty, pol, memo)?,
    };
    memo.normalize.leave_fun((infty, pol), res)
}

fn to_type_internal<'a, TCX: TypingContext + ?Sized>(
    ctx: &TCX,
    infty: InfTypeId,
    pol: Polarity,
    memo: &'a mut TypeConvertMemo,
) -> Result<TypeId, TypeError> {
    if let Some(x) = memo.convert.enter_fun((infty, pol)) {
        return x;
    }
    let infty = normalize_inftype(ctx, infty, pol, memo)?;
    let mut recurse = |ty, pol| to_type_internal(ctx, ty, pol, memo);
    let inf = ctx.infctx().db.lookup_intern_inference_type(infty);
    let res = match inf {
        InferenceType::Any => Type::Any,
        InferenceType::Bot => Type::Bot,
        InferenceType::Primitive(p) => Type::Primitive(p),
        InferenceType::Var(..) => {
            panic!("Internal Compiler Error: normalized inference type contains variable");
        }
        InferenceType::Unknown(_) => Type::Error,
        InferenceType::Nominal {
            kind,
            def,
            type_args,
        } => {
            let type_args = type_args
                .iter()
                .copied()
                .map(|x| recurse(x, pol))
                .collect::<Result<_, _>>()?;
            Type::Nominal {
                kind,
                def,
                type_args: Arc::new(type_args),
            }
        }
        InferenceType::Loop(kind, from, inner) => {
            Type::Loop(kind, recurse(from, pol)?, recurse(inner, pol)?)
        }
        InferenceType::ParserArg { result, arg } => {
            Type::ParserArg(recurse(result, pol)?, recurse(arg, pol.reverse())?)
        }
        InferenceType::FunctionArgs { result, args } => {
            let args = args
                .iter()
                .map(|&x| recurse(x, pol.reverse()))
                .collect::<Result<_, _>>()?;
            Type::FunctionArg(recurse(result, pol)?, Arc::new(args))
        }
        InferenceType::InferField(_, _) => {
            panic!("Internal Compiler Error: InferField in normalized inference type")
        }
    };
    memo.convert
        .leave_fun((infty, pol), ctx.infctx().db.intern_type(res))
}

fn from_type_internal<'a, TCX: TypingContext + ?Sized>(
    ctx: &mut TCX,
    ty: &Type,
    var_stack: Option<&'a VarStack<'a>>,
) -> Result<InfTypeId, TypeError> {
    let mut recurse =
        |x| from_type_internal(ctx, &ctx.infctx().db.lookup_intern_type(x), var_stack);
    let ret =
        match ty {
            Type::Any => InferenceType::Any,
            Type::Bot => InferenceType::Bot,
            Type::Error => return Ok(ctx.unknown()),
            Type::Primitive(p) => InferenceType::Primitive(*p),
            Type::TypeVarRef(level, index) => {
                return var_stack
                    .and_then(|x| x.resolve(*level, *index))
                    .ok_or(TypeError)
            }
            Type::ForAll(inner, defvars) => {
                let current = defvars.iter().map(|_| ctx.var()).collect::<Vec<_>>();
                let new_stack = VarStack {
                    cur: &current,
                    next: var_stack,
                };
                let inner = ctx.infctx().db.lookup_intern_type(*inner);
                return from_type_internal(ctx, &inner, Some(&new_stack));
            }
            Type::Nominal {
                kind,
                def,
                type_args,
            } => {
                let type_args = Box::new(type_args.iter().copied().map(recurse).collect::<Result<
                    Vec<_>,
                    _,
                >>(
                )?);
                InferenceType::Nominal {
                    kind: *kind,
                    def: *def,
                    type_args,
                }
            }
            Type::Loop(kind, from, inner) => {
                InferenceType::Loop(*kind, recurse(*from)?, recurse(*inner)?)
            }
            Type::ParserArg(parser, arg) => {
                let result = recurse(*parser)?;
                let arg = recurse(*arg)?;
                InferenceType::ParserArg { result, arg }
            }
            Type::FunctionArg(function, args) => {
                let result = recurse(*function)?;
                let args = Box::new(
                    args.iter()
                        .copied()
                        .map(recurse)
                        .collect::<Result<Vec<_>, _>>()?,
                );
                InferenceType::FunctionArgs { result, args }
            }
        };
    Ok(ctx.infctx().db.intern_inference_type(ret))
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Polarity {
    Positive,
    Negative,
}

impl Polarity {
    fn reverse(self) -> Self {
        match self {
            Polarity::Positive => Polarity::Negative,
            Polarity::Negative => Polarity::Positive,
        }
    }
}
