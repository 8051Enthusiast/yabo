use std::{collections::HashMap, hash::Hash, sync::Arc};

use crate::{
    connections::ConnectionMap,
    deref_levels::DerefCache,
    inference::{InfSlice, InfTypeHead, NominalInfHead, TRACING_ENABLED},
    TypeVarRef,
};
use yaboc_base::{
    dbeprintln,
    error::{SResult, Silencable},
    interner::DefId,
};

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
    memo: HashMap<From, SResult<To>>,
}

impl<From: Copy + Eq + Hash, To: Copy> Default for MemoRecursor<From, To> {
    fn default() -> Self {
        Self {
            process: Default::default(),
            memo: Default::default(),
        }
    }
}

impl<From: Copy + Eq + Hash + std::fmt::Debug, To: Copy> MemoRecursor<From, To> {
    fn enter_fun(&mut self, from: From) -> Option<Result<To, TypeError>> {
        if let Some(t) = self.memo.get(&from) {
            return Some(t.clone().map_err(|e| e.clone().into()));
        }
        let depth = self.process.len();
        if self.process.insert(from, depth).is_some() {
            return Some(Err(TypeError::RecursiveType));
        }
        None
    }
    fn leave_fun(&mut self, from: From, to: To) -> Result<To, TypeError> {
        self.memo.insert(from, Ok(to));
        self.process.remove(&from);
        Ok(to)
    }
    fn leave_fun_err(&mut self, from: From, err: TypeError) -> Result<To, TypeError> {
        self.memo.insert(from, Err(err.clone().silence()));
        self.process.remove(&from);
        Err(err)
    }
}

pub fn collect_homogenous_children<'intern>(
    tys: &[InfTypeId<'intern>],
) -> Result<InferenceType<Vec<InfTypeId<'intern>>>, [InfTypeHead; 2]> {
    let [first, ..] = tys else {
        panic!("collect_children called with empty slice")
    };
    let mut arr = first.child_arrays();
    for ty in tys[1..].iter() {
        match (&mut arr, ty.value()) {
            (
                InferenceType::ParserArg { result, arg },
                InferenceType::ParserArg {
                    result: other_result,
                    arg: other_arg,
                },
            ) => {
                result.push(*other_result);
                arg.push(*other_arg);
            }
            (
                InferenceType::FunctionArgs { result, args, .. },
                InferenceType::FunctionArgs {
                    result: other_result,
                    args: other_args,
                    ..
                },
            ) => {
                result.push(*other_result);
                for (i, arg) in other_args.iter().enumerate() {
                    args[i].push(*arg);
                }
            }
            (InferenceType::Loop(_, body), InferenceType::Loop(_, other_body)) => {
                body.push(*other_body);
            }
            (
                InferenceType::Nominal(NominalInfHead {
                    parse_arg,
                    fun_args,
                    ty_args,
                    internal,
                    ..
                }),
                InferenceType::Nominal(NominalInfHead {
                    parse_arg: other_parse_arg,
                    fun_args: other_fun_args,
                    ty_args: other_ty_args,
                    internal: other_internal,
                    ..
                }),
            ) => {
                if let (Some(parse_arg), Some(other_parse_arg)) = (parse_arg, other_parse_arg) {
                    parse_arg.push(*other_parse_arg);
                }
                for (i, arg) in other_fun_args.iter().enumerate() {
                    fun_args[i].push(*arg);
                }
                for (i, arg) in other_ty_args.iter().enumerate() {
                    ty_args[i].push(*arg);
                }
                *internal &= other_internal;
            }
            (
                InferenceType::InferField(name, result),
                InferenceType::InferField(other_name, other_result),
            ) => {
                if name != other_name {
                    panic!("InferField names do not match")
                }
                result.push(*other_result);
            }
            (
                InferenceType::InferIfResult(a, b, c),
                InferenceType::InferIfResult(other_a, other_b, other_c),
            ) => {
                if let (Some(a), Some(other_a)) = (a, other_a) {
                    a.push(*other_a);
                }
                b.push(*other_b);
                c.push(*other_c);
            }
            (InferenceType::Primitive(_), InferenceType::Primitive(_))
            | (InferenceType::Var(_), InferenceType::Var(_))
            | (InferenceType::Unknown, InferenceType::Unknown)
            | (InferenceType::TypeVarRef(_), InferenceType::TypeVarRef(_))
            | (InferenceType::SizeOf, InferenceType::SizeOf) => {}
            (a, b) => return Err([(&*a).into(), b.into()]),
        }
    }
    Ok(arr)
}

struct NegativePolarity;
struct PositivePolarity;

trait Polarity {
    type Opposite: Polarity<Opposite = Self>;
    fn combine_impl<'a, 'intern, TR: TypeResolver<'intern>>(
        ctx: &mut TypeConvertMemo<'a, 'intern, TR>,
        nom: InfSlice<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError>;
    const IS_POSITIVE: bool;
    fn check_dominating_ty<'intern>(tys: &[InfTypeId<'intern>]) -> Option<InfTypeId<'intern>> {
        if let Some(unknown) = tys.iter().find(|x| x.value() == &InferenceType::Unknown) {
            return Some(*unknown);
        }
        None
    }
}

impl Polarity for PositivePolarity {
    type Opposite = NegativePolarity;

    const IS_POSITIVE: bool = true;
    fn combine_impl<'a, 'intern, TR: TypeResolver<'intern>>(
        ctx: &mut TypeConvertMemo<'a, 'intern, TR>,
        combinee: InfSlice<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let homogenous = ctx.deref_cache.homogenize(combinee, ctx.ctx)?;
        if let Some(dom) = Self::check_dominating_ty(&homogenous) {
            return Ok(dom);
        }
        let normalized_homogenous = homogenous
            .iter()
            .copied()
            .map(|x| ctx.normalize_typevar(x))
            .collect::<Result<Vec<_>, _>>()?;
        if normalized_homogenous.is_empty() {
            return Err(TypeError::NonInfer);
        }
        let homogenous_children = collect_homogenous_children(&normalized_homogenous)
            .map_err(|[lhs, rhs]| TypeError::HeadIncompatible(lhs, rhs))?;

        let recursed = homogenous_children.try_map(ctx, |ctx, inner, loc| {
            if loc.opposite_polarity() {
                ctx.meet_inftype(&inner)
            } else {
                ctx.join_inftype(&inner)
            }
        })?;

        Ok(ctx.ctx.intern_infty(recursed))
    }
}

impl Polarity for NegativePolarity {
    type Opposite = PositivePolarity;
    const IS_POSITIVE: bool = false;
    fn combine_impl<'a, 'intern, TR: TypeResolver<'intern>>(
        ctx: &mut TypeConvertMemo<'a, 'intern, TR>,
        combinee: InfSlice<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let levels = combinee
            .iter()
            .map(|ty| ctx.deref_cache.deref_level(*ty, ctx.ctx))
            .collect::<Result<Vec<_>, _>>()?;
        let Some(max_level) = levels.iter().max() else {
            return Err(TypeError::NonInfer);
        };
        let highest_level_subset = combinee
            .iter()
            .copied()
            .zip(levels.iter())
            .filter_map(|(ty, level)| (level == max_level).then_some(ty))
            .map(|ty| ctx.normalize_typevar(ty))
            .collect::<Result<Vec<_>, _>>()?;
        let homogenous = ctx.deref_cache.homogenize(&highest_level_subset, ctx.ctx)?;
        if homogenous.is_empty() {
            return Err(TypeError::NonInfer);
        }
        if let Some(dom) = Self::check_dominating_ty(&homogenous) {
            return Ok(dom);
        }
        let homogenous_children = collect_homogenous_children(&homogenous)
            .map_err(|[lhs, rhs]| TypeError::HeadIncompatible(lhs, rhs))?;
        let recursed = homogenous_children.try_map(ctx, |ctx, inner, loc| {
            if loc.opposite_polarity() {
                ctx.join_inftype(&inner)
            } else {
                ctx.meet_inftype(&inner)
            }
        })?;

        Ok(ctx.ctx.intern_infty(recursed))
    }
}
pub struct TypeConvertMemo<'a, 'intern, TR: TypeResolver<'intern>> {
    convert: MemoRecursor<InfTypeId<'intern>, TypeId>,
    normalize: MemoRecursor<InfTypeId<'intern>, InfTypeId<'intern>>,
    meet: MemoRecursor<InfSlice<'intern>, InfTypeId<'intern>>,
    join: MemoRecursor<InfSlice<'intern>, InfTypeId<'intern>>,
    id: DefId,
    var_count: Option<u32>,
    map: ConnectionMap,
    deref_cache: DerefCache,
    ctx: &'a mut InferenceContext<'intern, TR>,
}

impl<'a, 'intern, TR: TypeResolver<'intern>> TypeConvertMemo<'a, 'intern, TR> {
    pub(crate) fn new(
        ctx: &'a mut InferenceContext<'intern, TR>,
        id: DefId,
        map: ConnectionMap,
        deref_cache: DerefCache,
    ) -> Self {
        TypeConvertMemo {
            convert: Default::default(),
            normalize: Default::default(),
            meet: Default::default(),
            join: Default::default(),
            var_count: None,
            id,
            map,
            deref_cache,
            ctx,
        }
    }
    fn enter_fun<P: Polarity>(
        &mut self,
        from: InfSlice<'intern>,
    ) -> Option<Result<InfTypeId<'intern>, TypeError>> {
        if P::IS_POSITIVE {
            self.join.enter_fun(from)
        } else {
            self.meet.enter_fun(from)
        }
    }
    fn leave_fun<P: Polarity>(
        &mut self,
        from: InfSlice<'intern>,
        to: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        if P::IS_POSITIVE {
            self.join.leave_fun(from, to)
        } else {
            self.meet.leave_fun(from, to)
        }
    }
    fn leave_fun_err<P: Polarity>(
        &mut self,
        from: InfSlice<'intern>,
        err: TypeError,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        if P::IS_POSITIVE {
            self.join.leave_fun_err(from, err)
        } else {
            self.meet.leave_fun_err(from, err)
        }
    }
    fn meet_inftype(
        &mut self,
        combinee: &[InfTypeId<'intern>],
    ) -> Result<InfTypeId<'intern>, TypeError> {
        self.combine::<NegativePolarity>(combinee)
    }
    fn join_inftype(
        &mut self,
        combinee: &[InfTypeId<'intern>],
    ) -> Result<InfTypeId<'intern>, TypeError> {
        self.combine::<PositivePolarity>(combinee)
    }
    fn combine<P: Polarity>(
        &mut self,
        combinee: &[InfTypeId<'intern>],
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let mut combinee: Vec<_> = combinee.to_vec();
        for ty in combinee.iter_mut() {
            *ty = self.normalize_var(*ty)?;
        }
        combinee.sort_unstable();
        combinee.dedup();
        let combinee = self.ctx.slice_interner.intern_slice(&combinee);
        if let Some(x) = self.enter_fun::<P>(combinee) {
            return x;
        }
        match P::combine_impl(self, combinee) {
            Ok(x) => self.leave_fun::<P>(combinee, x),
            Err(e) => self.leave_fun_err::<P>(combinee, e),
        }
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
    fn normalize_typevar(
        &mut self,
        infty: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let InferenceType::TypeVarRef(var) = infty.value() else {
            return Ok(infty);
        };
        if var.0 == self.id {
            return Ok(infty);
        }
        let Some(local_typevar) = self.map.def_map(self.id, *var) else {
            return Err(TypeError::NonInferTypeVar(*var));
        };
        let interned = self
            .ctx
            .intern_infty(InferenceType::TypeVarRef(local_typevar));
        Ok(interned)
    }
    fn normalize_var(
        &mut self,
        infty: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        if let InferenceType::Var(v) = infty.value() {
            let var_tys = self.ctx.var_store.get(*v).lower().to_vec();
            let contains_only_var = var_tys
                .iter()
                .all(|ty| matches!(ty.value(), InferenceType::Var(_)));
            if contains_only_var {
                if let Some(ref mut n) = self.var_count {
                    let result = self
                        .ctx
                        .intern_infty(InferenceType::TypeVarRef(TypeVarRef(self.id, *n)));
                    self.ctx.equal(result, infty)?;
                    *n += 1;
                    return Ok(result);
                }
            }
        }
        Ok(infty)
    }
    fn normalize_inftype_impl(
        &mut self,
        infty: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let mut res = self.normalize_var(infty)?;
        res = self.normalize_typevar(res)?;
        if matches!(res.value(), InferenceType::Var(_)) {
            res = self.join_inftype(&[res])?;
        }
        self.normalize_children(res)?;
        Ok(res)
    }

    fn normalize_inftype(
        &mut self,
        infty: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        if let Some(x) = self.normalize.enter_fun(infty) {
            return x;
        }
        match self.normalize_inftype_impl(infty) {
            Ok(x) => self.normalize.leave_fun(infty, x),
            Err(e) => self.normalize.leave_fun_err(infty, e),
        }
    }
    pub fn set_id(&mut self, id: DefId) {
        self.id = id;
    }
    fn convert_to_type_impl(&mut self, infty: InfTypeId<'intern>) -> Result<TypeId, TypeError> {
        let infty = self.normalize_inftype(infty)?;
        let res = match infty.value() {
            InferenceType::TypeVarRef(varref) => Type::TypeVarRef(*varref),
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
                let parse_arg = parse_arg.map(|x| self.convert_to_type(x)).transpose()?;
                let fun_args = fun_args
                    .iter()
                    .copied()
                    .map(|x| self.convert_to_type(x))
                    .collect::<Result<_, _>>()?;
                let ty_args = ty_args
                    .iter()
                    .copied()
                    .map(|x| self.convert_to_type(x))
                    .collect::<Result<_, _>>()?;
                Type::Nominal(NominalTypeHead {
                    kind: *kind,
                    def: *def,
                    parse_arg,
                    fun_args: Arc::new(fun_args),
                    ty_args: Arc::new(ty_args),
                })
            }
            InferenceType::Loop(kind, inner) => Type::Loop(*kind, self.convert_to_type(*inner)?),
            InferenceType::ParserArg { result, arg } => Type::ParserArg {
                result: self.convert_to_type(*result)?,
                arg: self.convert_to_type(*arg)?,
            },
            InferenceType::FunctionArgs {
                result,
                args,
                partial: _,
            } => {
                let args = args
                    .iter()
                    .map(|&x| self.convert_to_type(x))
                    .collect::<Result<_, _>>()?;
                Type::FunctionArg(self.convert_to_type(*result)?, Arc::new(args))
            }
            InferenceType::InferField(_, _)
            | InferenceType::InferIfResult(..)
            | InferenceType::SizeOf => {
                panic!("Internal Compiler Error: virtual type in normalized inference type")
            }
        };
        Ok(self.ctx.tr.db().intern_type(res))
    }
    pub fn convert_to_type(&mut self, infty: InfTypeId<'intern>) -> Result<TypeId, TypeError> {
        if let Some(x) = self.convert.enter_fun(infty) {
            return x;
        }
        match self.convert_to_type_impl(infty) {
            Ok(x) => {
                if TRACING_ENABLED {
                    dbeprintln!(
                        self.ctx.tr.db(),
                        "[{}] Converted {} to {}",
                        &self.ctx.tr.name(),
                        &infty,
                        &x
                    );
                }
                self.convert.leave_fun(infty, x)
            }
            Err(e) => self.convert.leave_fun_err(infty, e),
        }
    }
    pub fn convert_to_type_with_vars(
        &mut self,
        infty: InfTypeId<'intern>,
        n_vars: u32,
    ) -> Result<(TypeId, u32), TypeError> {
        self.var_count = Some(n_vars);
        let ret = self.convert_to_type(infty);
        let new_var_count = self.var_count.take().unwrap();
        ret.map(|x| (x, new_var_count))
    }
}

impl<'intern, TR: TypeResolver<'intern>> InfTypeInterner<'intern>
    for TypeConvertMemo<'_, 'intern, TR>
{
    fn intern_infty(&mut self, infty: InferenceType<InfTypeId<'intern>>) -> InfTypeId<'intern> {
        InfTypeId(self.ctx.interner.intern(infty))
    }

    fn intern_infty_slice(
        &mut self,
        slice: &[InfTypeId<'intern>],
    ) -> super::inference::InfSlice<'intern> {
        self.ctx.slice_interner.intern_slice(slice)
    }
}
