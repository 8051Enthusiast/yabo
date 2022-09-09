use std::{collections::HashMap, hash::Hash, sync::Arc};

use crate::{
    interner::DefId,
    types::{inference::NominalInfHead, InferenceType, TypeHead},
};

use super::{
    inference::{InfTypeId, InfTypeInterner},
    inference::{InferenceContext, TypeResolver},
    NominalTypeHead, Type, TypeError, TypeId, TypeInterner,
};

pub struct VarStack<'a, 'intern> {
    pub(crate) cur: &'a [InfTypeId<'intern>],
    pub(crate) next: Option<&'a VarStack<'a, 'intern>>,
}

impl<'a, 'intern> VarStack<'a, 'intern> {
    pub fn resolve(&'a self, level: u32, index: u32) -> Option<InfTypeId<'intern>> {
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
    fn meet_inftype(
        &mut self,
        lhs: InfTypeId<'intern>,
        rhs: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        use super::InferenceType::*;
        if let Some(x) = self.join.enter_fun((lhs, rhs)) {
            return x;
        }
        let res = match (lhs.value(), rhs.value()) {
            (Unknown, _) | (_, Unknown) => Unknown,
            (Bot, _) | (_, Bot) => Bot,
            (InferField(..), InferField(..)) => Any,
            (TypeVarRef(a0, a1, a2), TypeVarRef(b0, b1, b2)) if (a0, a1, a2) == (b0, b1, b2) => {
                TypeVarRef(*a0, *a1, *a2)
            }
            (Any | InferField(..), other) | (other, Any | InferField(..)) => other.clone(),
            (Primitive(p), Primitive(q)) if p == q => Primitive(*p),
            (Loop(kind1, inner1), Loop(kind2, inner2)) => {
                let kind = kind1.min(kind2);
                let inner = self.meet_inftype(*inner1, *inner2)?;
                Loop(*kind, inner)
            }
            (ParserArg { result: r1, arg: a }, ParserArg { result: r2, arg: b }) => {
                let result = self.meet_inftype(*r1, *r2)?;
                let arg = self.join_inftype(*a, *b)?;
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
                let result = self.meet_inftype(*result1, *result2)?;
                if args1.len() != args2.len() {
                    return Err(TypeError);
                }
                let args_vec: Vec<_> = args1
                    .iter()
                    .zip(args2.iter())
                    .map(|(arg1, arg2)| self.join_inftype(*arg1, *arg2))
                    .collect::<Result<_, _>>()?;
                let args = self.ctx.slice_interner.intern_slice(&args_vec);
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
                    (Some(arg1), Some(arg2)) => Some(self.meet_inftype(*arg1, *arg2)?),
                    // nominal types with same def should have same arity per construction
                    _ => unreachable!(),
                };
                let fun_args = fun_args1
                    .iter()
                    .zip(fun_args2.iter())
                    .map(|(arg1, arg2)| self.meet_inftype(*arg1, *arg2))
                    .collect::<Result<Vec<_>, _>>()?;
                let fun_args = self.ctx.slice_interner.intern_slice(&*fun_args);
                let ty_args = ty_args1
                    .iter()
                    .zip(ty_args2.iter())
                    .map(|(arg1, arg2)| self.join_inftype(*arg1, *arg2))
                    .collect::<Result<Vec<_>, _>>()?;
                let ty_args = self.ctx.slice_interner.intern_slice(&*ty_args);
                Nominal(NominalInfHead {
                    kind: *kind,
                    def: *def1,
                    parse_arg,
                    fun_args,
                    ty_args,
                    internal: *internal1 || *internal2,
                })
            }
            (nom @ Nominal(_), other) | (other, nom @ Nominal(_)) => {
                let nomhead = TypeHead::try_from(nom)?;
                let otherhead = TypeHead::try_from(other)?;
                let mut next = |ty: &_| -> Result<Option<&InferenceType>, TypeError> {
                    if let InferenceType::Nominal(nom) = ty {
                        Ok(self.ctx.deref(nom)?.map(|x| x.value()))
                    } else {
                        Ok(None)
                    }
                };
                let mut nom_ty = nom;
                loop {
                    if TypeHead::try_from(nom_ty)? == otherhead {
                        let [a_id, b_id] =
                            [&nom_ty, other].map(|x| self.ctx.intern_infty(x.clone()));
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
                    if TypeHead::try_from(other_ty)? == nomhead {
                        let [a_id, b_id] =
                            [other_ty, nom].map(|x| self.ctx.intern_infty(x.clone()));
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
    fn join_inftype(
        &mut self,
        lhs: InfTypeId<'intern>,
        rhs: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        use InferenceType::*;
        if let Some(x) = self.join.enter_fun((lhs, rhs)) {
            return x;
        }
        let res = match (lhs.value(), rhs.value()) {
            (Unknown, _) | (_, Unknown) => Unknown,
            (Any, _) | (_, Any) => Any,
            (InferField(..), InferField(..)) => Bot,
            (Bot | InferField(..), other) | (other, Bot | InferField(..)) => other.clone(),
            (Primitive(p), Primitive(q)) if p == q => Primitive(*p),
            (Loop(kind1, inner1), Loop(kind2, inner2)) => {
                let kind = kind1.max(kind2);
                let inner = self.join_inftype(*inner1, *inner2)?;
                Loop(*kind, inner)
            }
            (ParserArg { result: r1, arg: a }, ParserArg { result: r2, arg: b }) => {
                let result = self.join_inftype(*r1, *r2)?;
                let arg = self.meet_inftype(*a, *b)?;
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
                let result = self.join_inftype(*result1, *result2)?;
                if args1.len() != args2.len() {
                    return Err(TypeError);
                }
                let args_vec: Vec<_> = args1
                    .iter()
                    .zip(args2.iter())
                    .map(|(arg1, arg2)| self.meet_inftype(*arg1, *arg2))
                    .collect::<Result<_, _>>()?;
                let args = self.ctx.slice_interner.intern_slice(&args_vec);
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
                    (Some(arg1), Some(arg2)) => Some(self.join_inftype(*arg1, *arg2)?),
                    // nominal types with same def should have same arity per construction
                    _ => unreachable!(),
                };
                let fun_args = fun_args1
                    .iter()
                    .zip(fun_args2.iter())
                    .map(|(arg1, arg2)| self.join_inftype(*arg1, *arg2))
                    .collect::<Result<Vec<_>, _>>()?;
                let fun_args = self.ctx.slice_interner.intern_slice(&fun_args);
                let ty_args = ty_args1
                    .iter()
                    .zip(ty_args2.iter())
                    .map(|(arg1, arg2)| self.join_inftype(*arg1, *arg2))
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
            (nom @ Nominal(NominalInfHead { .. }), other)
            | (other, nom @ Nominal(NominalInfHead { .. })) => {
                let mut other_upset = HashMap::new();
                let mut next = |ty: &_| -> Result<_, TypeError> {
                    if let InferenceType::Nominal(nom) = ty {
                        Ok(self.ctx.deref(nom)?.map(|x| x.value()))
                    } else {
                        Ok(None)
                    }
                };
                let mut other_ty = other;
                loop {
                    other_upset.insert(TypeHead::try_from(other_ty)?, other_ty.clone());
                    match next(&other_ty)? {
                        Some(n) => other_ty = n,
                        None => break,
                    }
                }
                let mut res = None;
                let mut nom_ty = nom;
                loop {
                    if let Some(other_ty) = other_upset.remove(&TypeHead::try_from(nom_ty)?) {
                        let other = self.ctx.intern_infty(other_ty);
                        let nom = self.ctx.intern_infty(nom_ty.clone());
                        res = Some(self.join_inftype(other, nom)?);
                        break;
                    }
                    match next(&nom_ty)? {
                        Some(n) => nom_ty = n,
                        None => break,
                    }
                }
                if let Some(r) = res {
                    r.value().clone()
                } else {
                    return Err(TypeError);
                }
            }
            _ => return Err(TypeError),
        };
        let ret = self.ctx.intern_infty(res);
        self.join.leave_fun((lhs, rhs), ret)
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
                        result = self.ctx.intern_infty(InferenceType::TypeVarRef(id, 0, *n));
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
    pub(crate) fn to_type_internal(
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
            InferenceType::TypeVarRef(loc, level, index) => Type::TypeVarRef(*loc, *level, *index),
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
                    kind: *kind,
                    def: *def,
                    parse_arg,
                    fun_args: Arc::new(fun_args),
                    ty_args: Arc::new(ty_args),
                })
            }
            InferenceType::Loop(kind, inner) => Type::Loop(*kind, self.to_type_internal(*inner)?),
            InferenceType::ParserArg { result, arg } => Type::ParserArg {
                result: self.to_type_internal(*result)?,
                arg: self.to_type_internal(*arg)?,
            },
            InferenceType::FunctionArgs { result, args } => {
                let args = args
                    .iter()
                    .map(|&x| self.to_type_internal(x))
                    .collect::<Result<_, _>>()?;
                Type::FunctionArg(self.to_type_internal(*result)?, Arc::new(args))
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
        infty: InfTypeId<'intern>,
        n_vars: u32,
        at: DefId,
    ) -> Result<(TypeId, u32), TypeError> {
        self.var_count = Some((at, n_vars));
        let ret = self.to_type_internal(infty);
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
