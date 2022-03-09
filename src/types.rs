use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    sync::Arc,
};

use salsa::InternId;

use crate::{
    ast::ArrayKind,
    interner::{FieldName, HirId, Identifier},
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

#[salsa::query_group(TypeInternerDatabase)]
pub trait TypeInterner: crate::source::Files {
    #[salsa::interned]
    fn intern_type(&self, ty: Type) -> TypeId;
    #[salsa::interned]
    fn intern_inference_type(&self, ty: InferenceType) -> InfTypeId;
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NominalTypeHead {
    pub kind: NominalKind,
    pub def: HirId,
    pub parse_arg: TypeId,
    pub fun_args: Arc<Vec<TypeId>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Any,
    Bot,
    Error,
    Primitive(PrimitiveType),
    TypeVarRef(u32, u32),
    ForAll(TypeId, Arc<Vec<VarDef>>),
    Nominal(NominalTypeHead),
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
pub struct NominalInfHead {
    pub kind: NominalKind,
    pub def: HirId,
    pub parse_arg: InfTypeId,
    pub fun_args: Box<Vec<InfTypeId>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum InferenceType {
    Any,
    Bot,
    Primitive(PrimitiveType),
    Var(VarId),
    Unknown(VarId),
    Nominal(NominalInfHead),
    Loop(ArrayKind, InfTypeId, InfTypeId),
    ParserArg {
        result: InfTypeId,
        arg: InfTypeId,
    },
    FunctionArgs {
        result: InfTypeId,
        args: Box<Vec<InfTypeId>>,
    },
    InferField(FieldName, InfTypeId),
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
            InferenceType::Nominal(NominalInfHead { def, .. }) => TypeHead::Nominal(*def),
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

pub struct InferenceContext<'a, DB: TypeInterner + ?Sized> {
    var_store: VarStore,
    db: &'a DB,
    tr: &'a dyn TypeResolver,
    cache: HashSet<(InfTypeId, InfTypeId)>,
}

pub trait TypeResolver {
    fn field_type(&self, ty: &NominalInfHead, name: FieldName) -> Option<InfTypeId>;
    fn deref(&self, ty: &NominalInfHead) -> Option<InferenceType>;
    fn lookup(&self, context: HirId, name: FieldName) -> Option<InfTypeId>;
}

impl<'a, DB: TypeInterner + ?Sized> InferenceContext<'a, DB> {
    pub fn new(db: &'a DB, tr: &'a dyn TypeResolver) -> Self {
        Self {
            var_store: VarStore::new(),
            db,
            tr,
            cache: HashSet::new(),
        }
    }
    pub fn field_type(&self, ty: &NominalInfHead, name: FieldName) -> Option<InfTypeId> {
        self.tr.field_type(ty, name)
    }
    pub fn deref(&self, ty: &NominalInfHead) -> Option<InferenceType> {
        self.tr.deref(ty)
    }
    pub fn lookup(&self, context: HirId, name: FieldName) -> Option<InfTypeId> {
        self.tr.lookup(context, name)
    }
    pub fn constrain(&mut self, lower: InfTypeId, upper: InfTypeId) -> Result<(), TypeError> {
        use InferenceType::*;
        if !self.cache.insert((lower, upper)) {
            return Ok(());
        }
        let [lower_ty, upper_ty] = [lower, upper].map(|x| self.db.lookup_intern_inference_type(x));
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
                self.var_store.get_mut(var_id).upper.push(upper);
                // idx is needed because var.lower may change during the loop
                let mut idx = 0;
                while let Some(&lower_bound) = self.var_store.get(var_id).lower.get(idx) {
                    idx += 1;
                    self.constrain(lower_bound, upper)?;
                }
                Ok(())
            }

            (_, Var(var_id) | Unknown(var_id)) => {
                self.var_store.get_mut(var_id).lower.push(lower);
                // idx is needed because var.lower may change during the loop
                let mut idx = 0;
                while let Some(&upper_bound) = self.var_store.get(var_id).upper.get(idx) {
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

            (Nominal(ref nom), InferField(name, fieldtype)) => {
                let field_ty = self.field_type(nom, name).ok_or(TypeError)?;
                self.constrain(field_ty, fieldtype)
            }

            (
                Nominal(NominalInfHead {
                    def: def1,
                    fun_args: type_args1,
                    ..
                }),
                Nominal(NominalInfHead {
                    def: def2,
                    fun_args: type_args2,
                    ..
                }),
            ) if def1 == def2 => {
                if type_args1.len() != type_args2.len() {
                    panic!("Internal Compiler Error: compared same nominal types do not have same number of type args");
                }
                type_args1
                    .iter()
                    .zip(type_args2.iter())
                    .try_for_each(|(&arg1, &arg2)| self.constrain(arg1, arg2))
            }

            (Nominal(l), _) => {
                // if they are not the same head, try upcasting/dereferencing/evaluating
                if let Some(deref) = self.deref(&l).map(|x| self.db.intern_inference_type(x)) {
                    self.constrain(deref, upper)
                } else {
                    Err(TypeError)
                }
            }

            _ => Err(TypeError),
        }
    }
    pub fn equal(&mut self, left: InfTypeId, right: InfTypeId) -> Result<(), TypeError> {
        self.constrain(left, right)?;
        self.constrain(right, left)
    }
    pub fn var(&mut self) -> InfTypeId {
        let inftype = InferenceType::Var(self.var_store.add_var());
        self.db.intern_inference_type(inftype)
    }
    pub fn unknown(&mut self) -> InfTypeId {
        let inftype = InferenceType::Unknown(self.var_store.add_var());
        self.db.intern_inference_type(inftype)
    }
    pub fn int(&self) -> InfTypeId {
        let int = InferenceType::Primitive(PrimitiveType::Int);
        self.db.intern_inference_type(int)
    }
    pub fn char(&self) -> InfTypeId {
        let char = InferenceType::Primitive(PrimitiveType::Char);
        self.db.intern_inference_type(char)
    }
    pub fn bit(&self) -> InfTypeId {
        let bit = InferenceType::Primitive(PrimitiveType::Bit);
        self.db.intern_inference_type(bit)
    }
    pub fn parser(&self, result: InfTypeId, arg: InfTypeId) -> InfTypeId {
        self.db.intern_inference_type(InferenceType::ParserArg{ result, arg })
    }
    pub fn array_call(&mut self, kind: ArrayKind, inner: InfTypeId) -> Result<InfTypeId, TypeError> {
        let arg = self.var();
        let result = self.parser_apply(inner, arg)?;
        let array = InferenceType::Loop(kind, arg, result);
        let array = self.db.intern_inference_type(array);
        Ok(self.parser(array, arg))
    }
    pub fn block_call(&mut self, id: HirId) -> Result<InfTypeId, TypeError> {
        let arg = self.var();
        let nominal = NominalInfHead {
            kind: NominalKind::Block,
            def: id,
            parse_arg: arg,
            fun_args: Box::new(vec![]),
        };
        let result = self.db.intern_inference_type(InferenceType::Nominal(nominal));
        Ok(self.parser(result, arg))

    }
    pub fn one_of(&mut self, those: &[InfTypeId]) -> Result<InfTypeId, TypeError> {
        let ret = self.var();
        for &inftype in those {
            self.constrain(ret, inftype)?;
        }
        Ok(ret)
    }
    pub fn parser_compose(
        &mut self,
        first: InfTypeId,
        second: InfTypeId,
    ) -> Result<InfTypeId, TypeError> {
        let arg = self.var();
        let between = self.parser_apply(first, arg)?;
        let result = self.parser_apply(second, between)?;
        let new_parser = self
            .db
            .intern_inference_type(InferenceType::ParserArg { arg, result });
        Ok(new_parser)
    }
    pub fn parser_apply(
        &mut self,
        parser: InfTypeId,
        arg: InfTypeId,
    ) -> Result<InfTypeId, TypeError> {
        let var = self.var();
        let new_parser = self
            .db
            .intern_inference_type(InferenceType::ParserArg { arg, result: var });
        self.constrain(parser, new_parser)?;
        Ok(var)
    }
    pub fn function_apply(
        &mut self,
        function: InfTypeId,
        args: &[InfTypeId],
    ) -> Result<InfTypeId, TypeError> {
        let var = self.var();
        let new_function = InferenceType::FunctionArgs {
            args: Box::new(Vec::from(args)),
            result: var,
        };
        let new_function = self.db.intern_inference_type(new_function);
        self.constrain(function, new_function)?;
        Ok(var)
    }
    pub fn access_field(
        &mut self,
        accessed: InfTypeId,
        name: FieldName,
    ) -> Result<InfTypeId, TypeError> {
        let var = self.var();
        let infer_access = self
            .db
            .intern_inference_type(InferenceType::InferField(name, var));
        self.constrain(accessed, infer_access)?;
        Ok(var)
    }
    pub fn force_ref(&mut self, ref_type: InfTypeId, inner: InfTypeId) -> Result<(), TypeError> {
        match self.db.lookup_intern_inference_type(inner) {
            InferenceType::Nominal(m) => {
                self.equal(m.parse_arg, ref_type)?;
            }
            InferenceType::Loop(_, loop_from, _) => {
                self.equal(loop_from, ref_type)?;
            }
            InferenceType::Primitive(_)
            | InferenceType::Var(_)
            | InferenceType::Unknown(_)
            | InferenceType::ParserArg { .. }
            | InferenceType::FunctionArgs { .. }
            | InferenceType::InferField(_, _)
            | InferenceType::Any
            | InferenceType::Bot => return Err(TypeError),
        };
        Ok(())
    }
    pub fn from_type(&mut self, ty: &Type) -> Result<InfTypeId, TypeError> {
        self.from_type_internal(ty, None)
    }
    pub fn from_type_with_args(
        &mut self,
        ty: &Type,
        args: &[InfTypeId],
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
                let inner = self.db.lookup_intern_type(*inner);
                self.from_type_internal(&inner, Some(&vars))
            }
            _ => self.from_type_internal(ty, None),
        }
    }
    fn from_type_internal(
        &mut self,
        ty: &Type,
        var_stack: Option<&VarStack>,
    ) -> Result<InfTypeId, TypeError> {
        let mut recurse = |x| self.from_type_internal(&self.db.lookup_intern_type(x), var_stack);
        let ret = match ty {
            Type::Any => InferenceType::Any,
            Type::Bot => InferenceType::Bot,
            Type::Error => return Ok(self.unknown()),
            Type::Primitive(p) => InferenceType::Primitive(*p),
            Type::TypeVarRef(level, index) => {
                return var_stack
                    .and_then(|x| x.resolve(*level, *index))
                    .ok_or(TypeError)
            }
            Type::ForAll(inner, defvars) => {
                let current = defvars.iter().map(|_| self.var()).collect::<Vec<_>>();
                let new_stack = VarStack {
                    cur: &current,
                    next: var_stack,
                };
                let inner = self.db.lookup_intern_type(*inner);
                return self.from_type_internal(&inner, Some(&new_stack));
            }
            Type::Nominal(NominalTypeHead {
                kind,
                def,
                parse_arg,
                fun_args,
            }) => {
                let parse_arg = recurse(*parse_arg)?;
                let fun_args = Box::new(
                    fun_args
                        .iter()
                        .copied()
                        .map(recurse)
                        .collect::<Result<Vec<_>, _>>()?,
                );
                InferenceType::Nominal(NominalInfHead {
                    kind: *kind,
                    def: *def,
                    parse_arg,
                    fun_args,
                })
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
        Ok(self.db.intern_inference_type(ret))
    }
    pub fn to_type(&self, infty: InfTypeId, pol: Polarity) -> Result<TypeId, TypeError> {
        let mut converter = TypeConvertMemo::new(self);
        converter.to_type_internal(infty, pol)
    }
}

pub struct TypeError;

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

pub struct TypeConvertMemo<'a, DB: TypeInterner + ?Sized> {
    convert: MemoRecursor<(InfTypeId, Polarity), TypeId>,
    normalize: MemoRecursor<(InfTypeId, Polarity), InfTypeId>,
    meet: MemoRecursor<(InfTypeId, InfTypeId), InfTypeId>,
    join: MemoRecursor<(InfTypeId, InfTypeId), InfTypeId>,
    ctx: &'a InferenceContext<'a, DB>,
}

impl<'a, DB: TypeInterner + ?Sized> TypeConvertMemo<'a, DB> {
    pub fn new(ctx: &'a InferenceContext<'a, DB>) -> Self {
        fn default<T: Default>() -> T {
            T::default()
        }
        TypeConvertMemo {
            convert: default(),
            normalize: default(),
            meet: default(),
            join: default(),
            ctx,
        }
    }
    fn meet_inftype(&mut self, lhs: InfTypeId, rhs: InfTypeId) -> Result<InfTypeId, TypeError> {
        use InferenceType::*;
        if let Some(x) = self.join.enter_fun((lhs, rhs)) {
            return x;
        }
        let [lhs_ty, rhs_ty] = [lhs, rhs].map(|x| self.ctx.db.lookup_intern_inference_type(x));
        match (lhs_ty, rhs_ty) {
            (Bot, _) | (_, Bot) => Bot,
            (InferField(..), InferField(..)) => Any,
            (Any | InferField(..), other) | (other, Any | InferField(..)) => other,
            (Primitive(p), Primitive(q)) if p == q => Primitive(p),
            (Loop(kind1, from1, inner1), Loop(kind2, from2, inner2)) => {
                let kind = kind1.min(kind2);
                let from = self.meet_inftype(from1, from2)?;
                let inner = self.meet_inftype(inner1, inner2)?;
                Loop(kind, from, inner)
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
                }),
                Nominal(NominalInfHead {
                    def: def2,
                    parse_arg: parse_arg2,
                    fun_args: fun_args2,
                    ..
                }),
            ) if def1 == def2 => {
                let parse_arg = self.meet_inftype(parse_arg1, parse_arg2)?;
                let fun_args = Box::new(
                    fun_args1
                        .iter()
                        .zip(fun_args2.iter())
                        .map(|(arg1, arg2)| self.meet_inftype(*arg1, *arg2))
                        .collect::<Result<_, _>>()?,
                );
                Nominal(NominalInfHead {
                    kind,
                    def: def1,
                    parse_arg,
                    fun_args,
                })
            }
            (nom @ Nominal(_), other) | (other, nom @ Nominal(_)) => {
                let nomhead = TypeHead::try_from(&nom)?;
                let otherhead = TypeHead::try_from(&other)?;
                let next = |ty: &_| {
                    if let InferenceType::Nominal(nom) = ty {
                        self.ctx.deref(nom)
                    } else {
                        None
                    }
                };
                let mut ret = |a, b| {
                    let [a_id, b_id] = [a, b].map(|x| self.ctx.db.intern_inference_type(x));
                    self.meet_inftype(a_id, b_id)
                        .and_then(|x| self.meet.leave_fun((lhs, rhs), x))
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
    fn join_inftype(&mut self, lhs: InfTypeId, rhs: InfTypeId) -> Result<InfTypeId, TypeError> {
        use InferenceType::*;
        if let Some(x) = self.join.enter_fun((lhs, rhs)) {
            return x;
        }
        let [other, nom] = [lhs, rhs].map(|x| self.ctx.db.lookup_intern_inference_type(x));
        let res = match (other, nom) {
            (Any, _) | (_, Any) => Any,
            (InferField(..), InferField(..)) => Bot,
            (Bot | InferField(..), other) | (other, Bot | InferField(..)) => other,
            (Primitive(p), Primitive(q)) if p == q => Primitive(p),
            (Loop(kind1, from1, inner1), Loop(kind2, from2, inner2)) => {
                let kind = kind1.max(kind2);
                let from = self.join_inftype(from1, from2)?;
                let inner = self.join_inftype(inner1, inner2)?;
                Loop(kind, from, inner)
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
                }),
                Nominal(NominalInfHead {
                    def: def2,
                    parse_arg: parse_arg2,
                    fun_args: fun_args2,
                    ..
                }),
            ) if def1 == def2 => {
                let parse_arg = self.join_inftype(parse_arg1, parse_arg2)?;
                let fun_args = Box::new(
                    fun_args1
                        .iter()
                        .zip(fun_args2.iter())
                        .map(|(arg1, arg2)| self.join_inftype(*arg1, *arg2))
                        .collect::<Result<_, _>>()?,
                );
                Nominal(NominalInfHead {
                    kind,
                    def: def1,
                    parse_arg,
                    fun_args,
                })
            }
            (nom @ Nominal(NominalInfHead { .. }), other)
            | (other, nom @ Nominal(NominalInfHead { .. })) => {
                let mut other_upset = HashMap::new();
                let next = |ty: &_| {
                    if let InferenceType::Nominal(nom) = ty {
                        self.ctx.deref(nom)
                    } else {
                        None
                    }
                };
                for other_ty in std::iter::successors(Some(other), next) {
                    other_upset.insert(TypeHead::try_from(&other_ty)?, other_ty);
                }
                let mut res = None;
                for nom_ty in std::iter::successors(Some(nom), next) {
                    if let Some(other_ty) = other_upset.remove(&TypeHead::try_from(&nom_ty)?) {
                        let other = self.ctx.db.intern_inference_type(other_ty);
                        let nom = self.ctx.db.intern_inference_type(nom_ty);
                        res = Some(self.join_inftype(other, nom)?);
                        break;
                    } else {
                        res = None
                    }
                }
                if let Some(res) = res {
                    self.ctx.db.lookup_intern_inference_type(res)
                } else {
                    return Err(TypeError);
                }
            }
            _ => return Err(TypeError),
        };
        let ret = self.ctx.db.intern_inference_type(res);
        self.join.leave_fun((lhs, rhs), ret)
    }
    fn normalize_children(
        &mut self,
        infty: InfTypeId,
        pol: Polarity,
    ) -> Result<InfTypeId, TypeError> {
        let mut ty = self.ctx.db.lookup_intern_inference_type(infty);
        if let InferenceType::Var(_) | InferenceType::Unknown(_) = ty {
            ty = match pol {
                Polarity::Positive => InferenceType::Bot,
                Polarity::Negative => InferenceType::Any,
            };
        } else {
            for (child, pol) in ty.children(pol) {
                *child = self.normalize_inftype(*child, pol)?;
            }
        }
        Ok(self.ctx.db.intern_inference_type(ty))
    }
    fn normalize_inftype(
        &mut self,
        infty: InfTypeId,
        pol: Polarity,
    ) -> Result<InfTypeId, TypeError> {
        if let Some(x) = self.normalize.enter_fun((infty, pol)) {
            return x;
        }
        let inf = self.ctx.db.lookup_intern_inference_type(infty);
        let res = match (inf, pol) {
            (InferenceType::Var(v) | InferenceType::Unknown(v), Polarity::Positive) => {
                let mut result = self.ctx.db.intern_inference_type(InferenceType::Bot);
                for ty in self.ctx.var_store.get(v).lower.iter() {
                    let normalized = self.normalize_children(*ty, pol)?;
                    result = self.join_inftype(result, normalized)?;
                }
                result
            }
            (InferenceType::Var(v) | InferenceType::Unknown(v), Polarity::Negative) => {
                let mut result = self.ctx.db.intern_inference_type(InferenceType::Any);
                for ty in self.ctx.var_store.get(v).upper.iter() {
                    let normalized = self.normalize_children(*ty, pol)?;
                    result = self.meet_inftype(result, normalized)?;
                }
                // the result we got upon here is the most general, however in the upper bounds there can be
                // types like InferField which are more like type classes, so we join with the lower bounds,
                // because the join operation still prioritizes concrete types over type classes
                for ty in self.ctx.var_store.get(v).lower.iter() {
                    let normalized = self.normalize_children(*ty, pol)?;
                    result = self.join_inftype(result, normalized)?;
                }
                result
            }
            (_, _) => self.normalize_children(infty, pol)?,
        };
        self.normalize.leave_fun((infty, pol), res)
    }
    fn to_type_internal(&mut self, infty: InfTypeId, pol: Polarity) -> Result<TypeId, TypeError> {
        if let Some(x) = self.convert.enter_fun((infty, pol)) {
            return x;
        }
        let infty = self.normalize_inftype(infty, pol)?;
        let inf = self.ctx.db.lookup_intern_inference_type(infty);
        let res = match inf {
            InferenceType::Any => Type::Any,
            InferenceType::Bot => Type::Bot,
            InferenceType::Primitive(p) => Type::Primitive(p),
            InferenceType::Var(..) => {
                panic!("Internal Compiler Error: normalized inference type contains variable");
            }
            InferenceType::Unknown(_) => Type::Error,
            InferenceType::Nominal(NominalInfHead {
                kind,
                def,
                parse_arg,
                fun_args,
            }) => {
                let parse_arg = self.to_type_internal(parse_arg, pol)?;
                let type_args = fun_args
                    .iter()
                    .copied()
                    .map(|x| self.to_type_internal(x, pol))
                    .collect::<Result<_, _>>()?;
                Type::Nominal(NominalTypeHead {
                    kind,
                    def,
                    parse_arg,
                    fun_args: Arc::new(type_args),
                })
            }
            InferenceType::Loop(kind, from, inner) => Type::Loop(
                kind,
                self.to_type_internal(from, pol)?,
                self.to_type_internal(inner, pol)?,
            ),
            InferenceType::ParserArg { result, arg } => Type::ParserArg(
                self.to_type_internal(result, pol)?,
                self.to_type_internal(arg, pol.reverse())?,
            ),
            InferenceType::FunctionArgs { result, args } => {
                let args = args
                    .iter()
                    .map(|&x| self.to_type_internal(x, pol.reverse()))
                    .collect::<Result<_, _>>()?;
                Type::FunctionArg(self.to_type_internal(result, pol)?, Arc::new(args))
            }
            InferenceType::InferField(_, _) => {
                panic!("Internal Compiler Error: InferField in normalized inference type")
            }
        };
        self.convert
            .leave_fun((infty, pol), self.ctx.db.intern_type(res))
    }
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
