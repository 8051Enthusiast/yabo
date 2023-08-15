use std::{collections::HashMap, hash::Hash, sync::Arc};

use crate::{inference::NominalInfHead, PrimitiveType, TypeHead};
use yaboc_base::interner::DefId;

use super::{
    inference::{InfTypeId, InfTypeInterner, InferenceType},
    inference::{InferenceContext, TypeResolver},
    NominalTypeHead, Type, TypeError, TypeId, TypeInterner,
};

pub struct TyVars<'a, 'intern> {
    pub(crate) cur: &'a [InfTypeId<'intern>],
}

impl<'a, 'intern> TyVars<'a, 'intern> {
    pub fn resolve(&'a self, index: u32) -> Option<InfTypeId<'intern>> {
        self.cur.get(index as usize).copied()
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
            return Some(Ok(*t));
        }
        let depth = self.process.len();
        if self.process.insert(from, depth).is_some() {
            return Some(Err(TypeError::RecursiveType));
        }
        None
    }
    fn leave_fun(&mut self, from: From, to: To) -> Result<To, TypeError> {
        self.memo.insert(from, to);
        self.process.remove(&from);
        Ok(to)
    }
}

struct NegativePolarity;
struct PositivePolarity;

trait Polarity {
    type Opposite: Polarity<Opposite = Self>;
    fn combine<C: Ord>(lhs: C, rhs: C) -> C;
    fn combine_nom<'a, 'intern, TR: TypeResolver<'intern>>(
        ctx: &mut TypeConvertMemo<'a, 'intern, TR>,
        nom: InfTypeId<'intern>,
        other: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError>;
    const IS_POSITIVE: bool;
    const SAT_TYPE: InferenceType<'static>;
    const ID_TYPE: InferenceType<'static>;
}

impl Polarity for PositivePolarity {
    type Opposite = NegativePolarity;
    fn combine<C: Ord>(lhs: C, rhs: C) -> C {
        lhs.max(rhs)
    }

    const IS_POSITIVE: bool = true;
    const SAT_TYPE: InferenceType<'static> = InferenceType::Any;
    const ID_TYPE: InferenceType<'static> = InferenceType::Bot;

    fn combine_nom<'a, 'intern, TR: TypeResolver<'intern>>(
        ctx: &mut TypeConvertMemo<'a, 'intern, TR>,
        lhs: InfTypeId<'intern>,
        rhs: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let nom = lhs.value();
        let other = rhs.value();
        let mut other_upset = HashMap::new();
        let mut next = |ty: &_| -> Result<_, TypeError> {
            match ty {
                InferenceType::Nominal(nom) => Ok(ctx.ctx.deref(nom)?.map(|x| x.value())),
                InferenceType::Primitive(PrimitiveType::U8) => Ok(Some(ctx.ctx.int().value())),
                _ => Ok(None),
            }
        };
        let mut other_ty = other;
        loop {
            if !other_ty.is_fun() {
                other_upset.insert(TypeHead::try_from(other_ty).unwrap(), other_ty.clone());
            }
            match next(other_ty)? {
                Some(n) => other_ty = n,
                None => break,
            }
        }
        let mut res = None;
        let mut nom_ty = nom;
        loop {
            if let Some(other_ty) = other_upset.remove(&TypeHead::try_from(nom_ty).unwrap()) {
                let other = ctx.ctx.intern_infty(other_ty);
                let nom = ctx.ctx.intern_infty(nom_ty.clone());
                res = Some(ctx.join_inftype(other, nom)?);
                break;
            }
            match next(nom_ty)? {
                Some(n) => nom_ty = n,
                None => break,
            }
        }
        if let Some(r) = res {
            Ok(r)
        } else {
            Err(TypeError::HeadIncompatible(lhs.into(), rhs.into()))
        }
    }
}

impl Polarity for NegativePolarity {
    type Opposite = PositivePolarity;
    fn combine<C: Ord>(lhs: C, rhs: C) -> C {
        lhs.min(rhs)
    }
    const IS_POSITIVE: bool = false;
    const SAT_TYPE: InferenceType<'static> = InferenceType::Bot;
    const ID_TYPE: InferenceType<'static> = InferenceType::Any;

    fn combine_nom<'a, 'intern, TR: TypeResolver<'intern>>(
        ctx: &mut TypeConvertMemo<'a, 'intern, TR>,
        lhs: InfTypeId<'intern>,
        rhs: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let nom = lhs.value();
        let other = rhs.value();
        let nomhead = TypeHead::try_from(nom).unwrap();
        let otherhead = TypeHead::try_from(other).unwrap();
        let mut next = |ty: &_| -> Result<Option<&InferenceType>, TypeError> {
            match ty {
                InferenceType::Nominal(nom) => Ok(ctx.ctx.deref(nom)?.map(|x| x.value())),
                InferenceType::Primitive(PrimitiveType::U8) => Ok(Some(ctx.ctx.int().value())),
                _ => Ok(None),
            }
        };
        let mut nom_ty = nom;
        loop {
            if TypeHead::try_from(nom_ty).unwrap() == otherhead {
                let [a_id, b_id] = [nom_ty, other].map(|x| ctx.ctx.intern_infty(x.clone()));
                return ctx.meet_inftype(a_id, b_id);
            }
            match next(nom_ty)? {
                Some(n) => nom_ty = n,
                None => break,
            }
        }
        let mut other_ty = other;
        loop {
            if TypeHead::try_from(other_ty).unwrap() == nomhead {
                let [a_id, b_id] = [other_ty, nom].map(|x| ctx.ctx.intern_infty(x.clone()));
                return ctx.meet_inftype(a_id, b_id);
            }
            match next(other_ty)? {
                Some(n) => other_ty = n,
                None => break,
            }
        }
        Err(TypeError::HeadIncompatible(lhs.into(), rhs.into()))
    }
}
pub struct TypeConvertMemo<'a, 'intern, TR: TypeResolver<'intern>> {
    convert: MemoRecursor<InfTypeId<'intern>, TypeId>,
    normalize: MemoRecursor<InfTypeId<'intern>, InfTypeId<'intern>>,
    meet: MemoRecursor<(InfTypeId<'intern>, InfTypeId<'intern>), InfTypeId<'intern>>,
    join: MemoRecursor<(InfTypeId<'intern>, InfTypeId<'intern>), InfTypeId<'intern>>,
    var_count: Option<(DefId, u32)>,
    ctx: &'a mut InferenceContext<'intern, TR>,
}

impl<'a, 'intern, TR: TypeResolver<'intern>> TypeConvertMemo<'a, 'intern, TR> {
    pub fn new(ctx: &'a mut InferenceContext<'intern, TR>) -> Self {
        TypeConvertMemo {
            convert: Default::default(),
            normalize: Default::default(),
            meet: Default::default(),
            join: Default::default(),
            var_count: None,
            ctx,
        }
    }
    fn enter_fun<P: Polarity>(
        &mut self,
        from: (InfTypeId<'intern>, InfTypeId<'intern>),
    ) -> Option<Result<InfTypeId<'intern>, TypeError>> {
        if P::IS_POSITIVE {
            self.join.enter_fun(from)
        } else {
            self.meet.enter_fun(from)
        }
    }
    fn leave_fun<P: Polarity>(
        &mut self,
        from: (InfTypeId<'intern>, InfTypeId<'intern>),
        to: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        if P::IS_POSITIVE {
            self.join.leave_fun(from, to)
        } else {
            self.meet.leave_fun(from, to)
        }
    }
    fn meet_inftype(
        &mut self,
        lhs: InfTypeId<'intern>,
        rhs: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        self.combine::<NegativePolarity>(lhs, rhs)
    }
    fn join_inftype(
        &mut self,
        lhs: InfTypeId<'intern>,
        rhs: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        self.combine::<PositivePolarity>(lhs, rhs)
    }
    fn combine<P: Polarity>(
        &mut self,
        lhs: InfTypeId<'intern>,
        rhs: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        use InferenceType::*;
        if let Some(x) = self.enter_fun::<P>((lhs, rhs)) {
            return x;
        }
        let res = match (lhs.value(), rhs.value()) {
            (Unknown, _) | (_, Unknown) => Unknown,
            (x, _) | (_, x) if x == &P::SAT_TYPE => P::SAT_TYPE,
            (InferField(..) | InferIfResult(..), InferField(..) | InferIfResult(..)) => Any,
            (InferField(..) | InferIfResult(..), other)
            | (other, InferField(..) | InferIfResult(..)) => other.clone(),
            (id, other) | (other, id) if id == &P::ID_TYPE => other.clone(),
            (Primitive(p), Primitive(q)) if p == q => Primitive(*p),
            (Loop(kind1, inner1), Loop(kind2, inner2)) => {
                let kind = P::combine(*kind1, *kind2);
                let inner = self.combine::<P>(*inner1, *inner2)?;
                Loop(kind, inner)
            }
            (ParserArg { result: r1, arg: a }, ParserArg { result: r2, arg: b }) => {
                let result = self.combine::<P>(*r1, *r2)?;
                let arg = self.combine::<P::Opposite>(*a, *b)?;
                ParserArg { result, arg }
            }
            (
                FunctionArgs {
                    result: result1,
                    args: args1,
                    partial: partial1,
                },
                FunctionArgs {
                    result: result2,
                    args: args2,
                    partial: partial2,
                },
            ) if args1.len() == args2.len() && *partial1 == *partial2 => {
                let result = self.combine::<P>(*result1, *result2)?;
                let args_vec: Vec<_> = args1
                    .iter()
                    .zip(args2.iter())
                    .map(|(arg1, arg2)| self.combine::<P::Opposite>(*arg1, *arg2))
                    .collect::<Result<_, _>>()?;
                let args = self.ctx.slice_interner.intern_slice(&args_vec);
                FunctionArgs {
                    result,
                    args,
                    partial: *partial1,
                }
            }
            (
                Nominal(NominalInfHead {
                    kind,
                    def: def1,
                    parse_arg: parse_arg1,
                    fun_args: fun_args1,
                    ty_args: ty_args1,
                    internal: internal1,
                }),
                Nominal(NominalInfHead {
                    def: def2,
                    parse_arg: parse_arg2,
                    fun_args: fun_args2,
                    ty_args: ty_args2,
                    internal: internal2,
                    ..
                }),
            ) if def1 == def2 => {
                let parse_arg = match (parse_arg1, parse_arg2) {
                    (None, None) => None,
                    (Some(arg1), Some(arg2)) => Some(self.combine::<P>(*arg1, *arg2)?),
                    // nominal types with same def should have same arity per construction
                    _ => unreachable!(),
                };
                let fun_args = fun_args1
                    .iter()
                    .zip(fun_args2.iter())
                    .map(|(arg1, arg2)| self.combine::<P>(*arg1, *arg2))
                    .collect::<Result<Vec<_>, _>>()?;
                let fun_args = self.ctx.slice_interner.intern_slice(&fun_args);
                let ty_args = ty_args1
                    .iter()
                    .zip(ty_args2.iter())
                    .map(|(arg1, arg2)| self.combine::<P>(*arg1, *arg2))
                    .collect::<Result<Vec<_>, _>>()?;
                let ty_args = self.ctx.slice_interner.intern_slice(&ty_args);
                Nominal(NominalInfHead {
                    kind: *kind,
                    def: *def1,
                    parse_arg,
                    fun_args,
                    ty_args,
                    internal: *internal1 && *internal2,
                })
            }
            (Nominal(NominalInfHead { .. }) | Primitive(PrimitiveType::U8), _) => {
                P::combine_nom(self, lhs, rhs)?.value().clone()
            }
            (_, Nominal(NominalInfHead { .. }) | Primitive(PrimitiveType::U8)) => {
                P::combine_nom(self, rhs, lhs)?.value().clone()
            }
            _ => return Err(TypeError::HeadIncompatible(lhs.into(), rhs.into())),
        };
        let ret = self.ctx.intern_infty(res);
        self.leave_fun::<P>((lhs, rhs), ret)
    }
    fn normalize_children(
        &mut self,
        infty: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        if !matches!(infty.value(), InferenceType::Var(_)) {
            return infty.try_map_children(self, |ctx, child, _| ctx.normalize_inftype(child));
        }
        Ok(infty)
    }
    fn normalize_inftype(
        &mut self,
        infty: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        if let Some(x) = self.normalize.enter_fun(infty) {
            return x;
        }
        let res = match infty.value() {
            InferenceType::Var(v) => {
                let mut result = self.ctx.intern_infty(InferenceType::Bot);
                let var_store = self.ctx.var_store.clone();
                let mut contains_non_var = false;
                for ty in var_store.get(*v).lower().iter() {
                    if !matches!(ty.value(), InferenceType::Var(_)) {
                        contains_non_var = true;
                    }
                    let normalized = self.normalize_children(*ty)?;
                    result = self.join_inftype(result, normalized)?;
                }
                if !contains_non_var {
                    if let Some((id, ref mut n)) = self.var_count {
                        result = self.ctx.intern_infty(InferenceType::TypeVarRef(id, *n));
                        self.ctx.equal(result, infty)?;
                        *n += 1;
                    }
                }
                result
            }
            _ => self.normalize_children(infty)?,
        };
        self.normalize.leave_fun(infty, res)
    }
    pub(crate) fn convert_to_type_internal(
        &mut self,
        infty: InfTypeId<'intern>,
    ) -> Result<TypeId, TypeError> {
        if let Some(x) = self.convert.enter_fun(infty) {
            return x;
        }
        let infty = self.normalize_inftype(infty)?;
        let res = match infty.value() {
            InferenceType::Any => Type::Any,
            InferenceType::Bot => Type::Bot,
            InferenceType::TypeVarRef(loc, index) => Type::TypeVarRef(*loc, *index),
            InferenceType::Primitive(p) => Type::Primitive(*p),
            InferenceType::Var(..) => {
                panic!("Internal Compiler Error: normalized inference type contains variable");
            }
            InferenceType::Unknown => Type::Unknown,
            InferenceType::Nominal(NominalInfHead {
                kind,
                def,
                parse_arg,
                fun_args,
                ty_args,
                internal: _,
            }) => {
                let parse_arg = parse_arg
                    .map(|x| self.convert_to_type_internal(x))
                    .transpose()?;
                let fun_args = fun_args
                    .iter()
                    .copied()
                    .map(|x| self.convert_to_type_internal(x))
                    .collect::<Result<_, _>>()?;
                let ty_args = ty_args
                    .iter()
                    .copied()
                    .map(|x| self.convert_to_type_internal(x))
                    .collect::<Result<_, _>>()?;
                Type::Nominal(NominalTypeHead {
                    kind: *kind,
                    def: *def,
                    parse_arg,
                    fun_args: Arc::new(fun_args),
                    ty_args: Arc::new(ty_args),
                })
            }
            InferenceType::Loop(kind, inner) => {
                Type::Loop(*kind, self.convert_to_type_internal(*inner)?)
            }
            InferenceType::ParserArg { result, arg } => Type::ParserArg {
                result: self.convert_to_type_internal(*result)?,
                arg: self.convert_to_type_internal(*arg)?,
            },
            InferenceType::FunctionArgs {
                result,
                args,
                partial: _,
            } => {
                let args = args
                    .iter()
                    .map(|&x| self.convert_to_type_internal(x))
                    .collect::<Result<_, _>>()?;
                Type::FunctionArg(self.convert_to_type_internal(*result)?, Arc::new(args))
            }
            InferenceType::InferField(_, _) | InferenceType::InferIfResult(..) => {
                panic!("Internal Compiler Error: Infer type in normalized inference type")
            }
        };
        self.convert
            .leave_fun(infty, self.ctx.tr.db().intern_type(res))
    }
    pub(crate) fn convert_to_type_internal_with_vars(
        &mut self,
        infty: InfTypeId<'intern>,
        n_vars: u32,
        at: DefId,
    ) -> Result<(TypeId, u32), TypeError> {
        self.var_count = Some((at, n_vars));
        let ret = self.convert_to_type_internal(infty);
        let new_var_count = self.var_count.take().unwrap().1;
        ret.map(|x| (x, new_var_count))
    }
}

impl<'a, 'intern, TR: TypeResolver<'intern>> InfTypeInterner<'intern>
    for TypeConvertMemo<'a, 'intern, TR>
{
    fn intern_infty(&mut self, infty: InferenceType<'intern>) -> InfTypeId<'intern> {
        InfTypeId(self.ctx.interner.intern(infty))
    }

    fn intern_infty_slice(
        &mut self,
        slice: &[InfTypeId<'intern>],
    ) -> super::inference::InfSlice<'intern> {
        self.ctx.slice_interner.intern_slice(slice)
    }
}
