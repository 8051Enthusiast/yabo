use std::{collections::HashMap, hash::Hash, sync::Arc};

use crate::{
    interner::HirId,
    types::{InferenceType, NominalInfHead, TypeHead},
};

use super::{
    InfTypeId, InferenceContext, NominalTypeHead, Type, TypeError, TypeId, TypeInterner,
    TypeResolver,
};

pub struct VarStack<'a> {
    pub(crate) cur: &'a [InfTypeId],
    pub(crate) next: Option<&'a VarStack<'a>>,
}

impl<'a> VarStack<'a> {
    pub fn resolve(&'a self, level: u32, index: u32) -> Option<InfTypeId> {
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
            return Some(Ok(*t));
        }
        let depth = self.process.len();
        if self.process.insert(from, depth).is_some() {
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

pub struct TypeConvertMemo<'a, TR: TypeResolver> {
    convert: MemoRecursor<InfTypeId, TypeId>,
    normalize: MemoRecursor<InfTypeId, InfTypeId>,
    meet: MemoRecursor<(InfTypeId, InfTypeId), InfTypeId>,
    join: MemoRecursor<(InfTypeId, InfTypeId), InfTypeId>,
    var_count: Option<(HirId, u32)>,
    ctx: &'a mut InferenceContext<TR>,
}

impl<'a, TR: TypeResolver> TypeConvertMemo<'a, TR> {
    pub fn new(ctx: &'a mut InferenceContext<TR>) -> Self {
        TypeConvertMemo {
            convert: Default::default(),
            normalize: Default::default(),
            meet: Default::default(),
            join: Default::default(),
            var_count: None,
            ctx,
        }
    }
    fn meet_inftype(&mut self, lhs: InfTypeId, rhs: InfTypeId) -> Result<InfTypeId, TypeError> {
        use super::InferenceType::*;
        if let Some(x) = self.join.enter_fun((lhs, rhs)) {
            return x;
        }
        let [lhs_ty, rhs_ty] = [lhs, rhs].map(|x| self.ctx.lookup_infty(x));
        let res = match (lhs_ty, rhs_ty) {
            (Unknown, _) | (_, Unknown) => Unknown,
            (Bot, _) | (_, Bot) => Bot,
            (InferField(..), InferField(..)) => Any,
            (TypeVarRef(a0, a1, a2), TypeVarRef(b0, b1, b2)) if (a0, a1, a2) == (b0, b1, b2) => {
                TypeVarRef(a0, a1, a2)
            }
            (Any | InferField(..), other) | (other, Any | InferField(..)) => other,
            (Primitive(p), Primitive(q)) if p == q => Primitive(p),
            (Loop(kind1, inner1), Loop(kind2, inner2)) => {
                let kind = kind1.min(kind2);
                let inner = self.meet_inftype(inner1, inner2)?;
                Loop(kind, inner)
            }
            (ParserArg { result: r1, arg: a }, ParserArg { result: r2, arg: b }) => {
                let result = self.meet_inftype(r1, r2)?;
                let arg = self.join_inftype(a, b)?;
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
                let result = self.meet_inftype(result1, result2)?;
                if args1.len() != args2.len() {
                    return Err(TypeError);
                }
                let args = Box::new(
                    args1
                        .iter()
                        .zip(args2.iter())
                        .map(|(arg1, arg2)| self.join_inftype(*arg1, *arg2))
                        .collect::<Result<_, _>>()?,
                );
                FunctionArgs { result, args }
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
                    (Some(arg1), Some(arg2)) => Some(self.meet_inftype(arg1, arg2)?),
                    (None, Some(_)) | (Some(_), None) => return Err(TypeError),
                };
                let fun_args = Box::new(
                    fun_args1
                        .iter()
                        .zip(fun_args2.iter())
                        .map(|(arg1, arg2)| self.meet_inftype(*arg1, *arg2))
                        .collect::<Result<_, _>>()?,
                );
                let ty_args = Box::new(
                    ty_args1
                        .iter()
                        .zip(ty_args2.iter())
                        .map(|(arg1, arg2)| self.join_inftype(*arg1, *arg2))
                        .collect::<Result<_, _>>()?,
                );
                Nominal(NominalInfHead {
                    kind,
                    def: def1,
                    parse_arg,
                    fun_args,
                    ty_args,
                    internal: internal1 || internal2,
                })
            }
            (nom @ Nominal(_), other) | (other, nom @ Nominal(_)) => {
                let nomhead = TypeHead::try_from(&nom)?;
                let otherhead = TypeHead::try_from(&other)?;
                let mut next = |ty: &_| -> Result<Option<InferenceType>, TypeError> {
                    if let InferenceType::Nominal(nom) = ty {
                        Ok(self.ctx.deref(nom)?.map(|x| self.ctx.lookup_infty(x)))
                    } else {
                        Ok(None)
                    }
                };
                let mut nom_ty = nom.clone();
                loop {
                    if TypeHead::try_from(&nom_ty)? == otherhead {
                        let [a_id, b_id] = [nom_ty, other].map(|x| self.ctx.intern_infty(x));
                        return self
                            .meet_inftype(a_id, b_id)
                            .and_then(|x| self.meet.leave_fun((lhs, rhs), x));
                    }
                    match next(&nom_ty)? {
                        Some(n) => nom_ty = n,
                        None => break,
                    }
                }
                let mut other_ty = other;
                loop {
                    if TypeHead::try_from(&other_ty)? == nomhead {
                        let [a_id, b_id] = [other_ty, nom].map(|x| self.ctx.intern_infty(x));
                        return self
                            .meet_inftype(a_id, b_id)
                            .and_then(|x| self.meet.leave_fun((lhs, rhs), x));
                    }
                    match next(&other_ty)? {
                        Some(n) => other_ty = n,
                        None => break,
                    }
                }
                return Err(TypeError);
            }
            _ => return Err(TypeError),
        };
        let ret = self.ctx.intern_infty(res);
        self.meet.leave_fun((lhs, rhs), ret)
    }
    fn join_inftype(&mut self, lhs: InfTypeId, rhs: InfTypeId) -> Result<InfTypeId, TypeError> {
        use InferenceType::*;
        if let Some(x) = self.join.enter_fun((lhs, rhs)) {
            return x;
        }
        let [other, nom] = [lhs, rhs].map(|x| self.ctx.lookup_infty(x));
        let res = match (other, nom) {
            (Unknown, _) | (_, Unknown) => Unknown,
            (Any, _) | (_, Any) => Any,
            (InferField(..), InferField(..)) => Bot,
            (TypeVarRef(a0, a1, a2), TypeVarRef(b0, b1, b2)) if (a0, a1, a2) == (b0, b1, b2) => {
                TypeVarRef(a0, a1, a2)
            }
            (Bot | InferField(..), other) | (other, Bot | InferField(..)) => other,
            (Primitive(p), Primitive(q)) if p == q => Primitive(p),
            (Loop(kind1, inner1), Loop(kind2, inner2)) => {
                let kind = kind1.max(kind2);
                let inner = self.join_inftype(inner1, inner2)?;
                Loop(kind, inner)
            }
            (ParserArg { result: r1, arg: a }, ParserArg { result: r2, arg: b }) => {
                let result = self.join_inftype(r1, r2)?;
                let arg = self.meet_inftype(a, b)?;
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
                let result = self.join_inftype(result1, result2)?;
                if args1.len() != args2.len() {
                    return Err(TypeError);
                }
                let args = Box::new(
                    args1
                        .iter()
                        .zip(args2.iter())
                        .map(|(arg1, arg2)| self.meet_inftype(*arg1, *arg2))
                        .collect::<Result<_, _>>()?,
                );
                FunctionArgs { result, args }
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
                    (Some(arg1), Some(arg2)) => Some(self.join_inftype(arg1, arg2)?),
                    (None, Some(_)) | (Some(_), None) => return Err(TypeError),
                };
                let fun_args = Box::new(
                    fun_args1
                        .iter()
                        .zip(fun_args2.iter())
                        .map(|(arg1, arg2)| self.join_inftype(*arg1, *arg2))
                        .collect::<Result<_, _>>()?,
                );
                let ty_args = Box::new(
                    ty_args1
                        .iter()
                        .zip(ty_args2.iter())
                        .map(|(arg1, arg2)| self.join_inftype(*arg1, *arg2))
                        .collect::<Result<_, _>>()?,
                );
                Nominal(NominalInfHead {
                    kind,
                    def: def1,
                    parse_arg,
                    fun_args,
                    ty_args,
                    internal: internal1 && internal2,
                })
            }
            (nom @ Nominal(NominalInfHead { .. }), other)
            | (other, nom @ Nominal(NominalInfHead { .. })) => {
                let mut other_upset = HashMap::new();
                let mut next = |ty: &_| -> Result<_, TypeError> {
                    if let InferenceType::Nominal(nom) = ty {
                        Ok(self.ctx.deref(nom)?.map(|x| self.ctx.lookup_infty(x)))
                    } else {
                        Ok(None)
                    }
                };
                let mut other_ty = other;
                loop {
                    other_upset.insert(TypeHead::try_from(&other_ty)?, other_ty.clone());
                    match next(&other_ty)? {
                        Some(n) => other_ty = n,
                        None => break,
                    }
                }
                let mut res = None;
                let mut nom_ty = nom;
                loop {
                    if let Some(other_ty) = other_upset.remove(&TypeHead::try_from(&nom_ty)?) {
                        let other = self.ctx.intern_infty(other_ty);
                        let nom = self.ctx.intern_infty(nom_ty);
                        res = Some(self.join_inftype(other, nom)?);
                        break;
                    }
                    match next(&nom_ty)? {
                        Some(n) => nom_ty = n,
                        None => break,
                    }
                }
                if let Some(r) = res {
                    self.ctx.lookup_infty(r)
                } else {
                    return Err(TypeError);
                }
            }
            _ => return Err(TypeError),
        };
        let ret = self.ctx.intern_infty(res);
        self.join.leave_fun((lhs, rhs), ret)
    }
    fn normalize_children(&mut self, infty: &mut InferenceType) -> Result<(), TypeError> {
        if !matches!(infty, InferenceType::Var(_)) {
            for child in infty.children() {
                *child = self.normalize_inftype(*child)?;
            }
        }
        Ok(())
    }
    fn normalize_inftype(&mut self, infty: InfTypeId) -> Result<InfTypeId, TypeError> {
        if let Some(x) = self.normalize.enter_fun(infty) {
            return x;
        }
        let inf = self.ctx.lookup_infty(infty);
        let res = match inf {
            InferenceType::Var(v) => {
                let mut result = self.ctx.intern_infty(InferenceType::Bot);
                let var_store = self.ctx.var_store.clone();
                let mut contains_non_var = false;
                for ty in var_store.get(v).lower.iter() {
                    let mut inference_type = self.ctx.lookup_infty(*ty);
                    if !matches!(inference_type, InferenceType::Var(_)) {
                        contains_non_var = true;
                        self.normalize_children(&mut inference_type)?;
                    }
                    let normalized = self.ctx.intern_infty(inference_type);
                    result = self.join_inftype(result, normalized)?;
                }
                if !contains_non_var {
                    if let Some((id, ref mut n)) = self.var_count {
                        result = self.ctx.intern_infty(InferenceType::TypeVarRef(id, 0, *n));
                        self.ctx.equal(result, infty)?;
                        *n += 1;
                    }
                }
                result
            }
            _ => {
                let mut inference_type = self.ctx.lookup_infty(infty);
                self.normalize_children(&mut inference_type)?;
                self.ctx.intern_infty(inference_type)
            }
        };
        self.normalize.leave_fun(infty, res)
    }
    pub(crate) fn to_type_internal(&mut self, infty: InfTypeId) -> Result<TypeId, TypeError> {
        if let Some(x) = self.convert.enter_fun(infty) {
            return x;
        }
        let infty = self.normalize_inftype(infty)?;
        let inf = self.ctx.lookup_infty(infty);
        let res = match inf {
            InferenceType::Any => Type::Any,
            InferenceType::Bot => Type::Bot,
            InferenceType::TypeVarRef(loc, level, index) => Type::TypeVarRef(loc, level, index),
            InferenceType::Primitive(p) => Type::Primitive(p),
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
                let parse_arg = parse_arg.map(|x| self.to_type_internal(x)).transpose()?;
                let fun_args = fun_args
                    .iter()
                    .copied()
                    .map(|x| self.to_type_internal(x))
                    .collect::<Result<_, _>>()?;
                let ty_args = ty_args
                    .iter()
                    .copied()
                    .map(|x| self.to_type_internal(x))
                    .collect::<Result<_, _>>()?;
                Type::Nominal(NominalTypeHead {
                    kind,
                    def,
                    parse_arg,
                    fun_args: Arc::new(fun_args),
                    ty_args: Arc::new(ty_args),
                })
            }
            InferenceType::Loop(kind, inner) => Type::Loop(kind, self.to_type_internal(inner)?),
            InferenceType::ParserArg { result, arg } => Type::ParserArg {
                result: self.to_type_internal(result)?,
                arg: self.to_type_internal(arg)?,
            },
            InferenceType::FunctionArgs { result, args } => {
                let args = args
                    .iter()
                    .map(|&x| self.to_type_internal(x))
                    .collect::<Result<_, _>>()?;
                Type::FunctionArg(self.to_type_internal(result)?, Arc::new(args))
            }
            InferenceType::InferField(_, _) => {
                panic!("Internal Compiler Error: InferField in normalized inference type")
            }
        };
        self.convert
            .leave_fun(infty, self.ctx.tr.db().intern_type(res))
    }
    pub(crate) fn to_type_internal_with_vars(
        &mut self,
        infty: InfTypeId,
        n_vars: u32,
        at: HirId,
    ) -> Result<(TypeId, u32), TypeError> {
        self.var_count = Some((at, n_vars));
        let ret = self.to_type_internal(infty);
        let new_var_count = self.var_count.take().unwrap().1;
        ret.map(|x| (x, new_var_count))
    }
}
