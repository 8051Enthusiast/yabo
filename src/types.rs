mod represent;

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    sync::Arc,
};

use salsa::InternId;

use crate::{
    ast::ArrayKind,
    interner::{FieldName, HirId, TypeVar, Interner}, types::represent::print_inftype,
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
    fn type_contains_unknown(&self, ty: TypeId) -> bool;
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NominalTypeHead {
    pub kind: NominalKind,
    pub def: HirId,
    pub parse_arg: Option<TypeId>,
    pub fun_args: Arc<Vec<TypeId>>,
    pub ty_args: Arc<Vec<TypeId>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Any,
    Bot,
    Unknown,
    Primitive(PrimitiveType),
    TypeVarRef(HirId, u32, u32),
    ForAll(TypeId, Arc<Vec<VarDef>>),
    Nominal(NominalTypeHead),
    Loop(ArrayKind, TypeId),
    ParserArg { result: TypeId, arg: TypeId },
    FunctionArg(TypeId, Arc<Vec<TypeId>>),
}

pub fn type_contains_unknown(db: &dyn TypeInterner, id: TypeId) -> bool {
    match db.lookup_intern_type(id) {
        Type::Any | Type::Bot | Type::Primitive(_) | Type::TypeVarRef(_, _, _) => false,
        Type::Unknown => true,
        Type::ForAll(inner, _) => db.type_contains_unknown(inner),
        Type::Nominal(NominalTypeHead {
            parse_arg,
            fun_args,
            ..
        }) => {
            parse_arg.map_or(false, |x| db.type_contains_unknown(x))
                || fun_args.iter().any(|x| db.type_contains_unknown(*x))
        }
        Type::Loop(_, inner) => db.type_contains_unknown(inner),
        Type::ParserArg { result, arg } => {
            db.type_contains_unknown(result) || db.type_contains_unknown(arg)
        }
        Type::FunctionArg(a, b) => {
            db.type_contains_unknown(a) || b.iter().any(|x| db.type_contains_unknown(*x))
        }
    }
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
    pub parse_arg: Option<InfTypeId>,
    pub fun_args: Box<Vec<InfTypeId>>,
    pub ty_args: Box<Vec<InfTypeId>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum InferenceType {
    Any,
    Bot,
    Primitive(PrimitiveType),
    TypeVarRef(HirId, u32, u32),
    Var(VarId),
    Unknown,
    Nominal(NominalInfHead),
    Loop(ArrayKind, InfTypeId),
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
            InferenceType::Loop(_, inner) => {
                vec![(inner, pol)]
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
            InferenceType::Var(..) => {
                panic!("Internal Compiler Error: taking children of variable");
            }
            InferenceType::Nominal(nom) => nom.ty_args.iter_mut().map(|x| (x, pol)).collect(),
            InferenceType::Any
            | InferenceType::Bot
            | InferenceType::Primitive(_)
            | InferenceType::TypeVarRef(_, _, _)
            | InferenceType::Unknown => vec![],
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeHead {
    Any,
    Bot,
    Primitive(PrimitiveType),
    TypeVarRef,
    Nominal(HirId),
    Loop(ArrayKind),
    ParserArg,
    FunctionArgs,
    Unknown,
}

impl TryFrom<&InferenceType> for TypeHead {
    type Error = TypeError;

    fn try_from(value: &InferenceType) -> Result<Self, Self::Error> {
        Ok(match value {
            InferenceType::Any => TypeHead::Any,
            InferenceType::Bot => TypeHead::Bot,
            InferenceType::Primitive(p) => TypeHead::Primitive(*p),
            InferenceType::TypeVarRef(..) => TypeHead::TypeVarRef,
            InferenceType::Nominal(NominalInfHead { def, .. }) => TypeHead::Nominal(*def),
            InferenceType::Loop(kind, _) => TypeHead::Loop(*kind),
            InferenceType::ParserArg { .. } => TypeHead::ParserArg,
            InferenceType::FunctionArgs { .. } => TypeHead::FunctionArgs,
            InferenceType::Unknown => TypeHead::Unknown,
            InferenceType::Var(_) | InferenceType::InferField(..) => return Err(TypeError),
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
pub struct Signature {
    pub ty_args: Arc<Vec<VarDef>>,
    pub from: Option<TypeId>,
    pub args: Arc<Vec<TypeId>>,
    pub thunk: TypeId,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct VarDef {
    pub name: Option<TypeVar>,
}

impl VarDef {
    pub fn new(name: Option<TypeVar>) -> Self {
        Self { name }
    }
}

#[derive(Clone, Default)]
struct VarStore {
    vars: Vec<InferenceVar>,
}

impl VarStore {
    fn new() -> Self {
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

pub struct InferenceContext<TR: TypeResolver> {
    var_store: VarStore,
    pub tr: TR,
    cache: HashSet<(InfTypeId, InfTypeId)>,
}

pub trait TypeResolver {
    type DB: TypeInterner + Interner + ?Sized;
    fn db(&self) -> &Self::DB;
    fn field_type(&self, ty: &NominalInfHead, name: FieldName) -> Result<TypeId, ()>;
    fn deref(&self, ty: &NominalInfHead) -> Result<Option<TypeId>, ()>;
    fn signature(&self, ty: &NominalInfHead) -> Result<Signature, ()>;
    fn lookup(&self, context: HirId, name: FieldName) -> Result<TypeId, ()>;
}

impl<TR: TypeResolver> InferenceContext<TR> {
    pub fn new(tr: TR) -> Self {
        Self {
            var_store: VarStore::new(),
            tr,
            cache: HashSet::new(),
        }
    }
    fn lookup_infty(&self, id: InfTypeId) -> InferenceType {
        self.tr.db().lookup_intern_inference_type(id)
    }
    fn intern_infty(&self, infty: InferenceType) -> InfTypeId {
        self.tr.db().intern_inference_type(infty)
    }
    pub fn field_type(
        &mut self,
        ty: &NominalInfHead,
        name: FieldName,
    ) -> Result<InfTypeId, TypeError> {
        Ok(self.from_type_with_args(
            self.tr.field_type(ty, name).map_err(|()| TypeError)?,
            &ty.ty_args,
        ))
    }
    pub fn deref(&mut self, ty: &NominalInfHead) -> Result<Option<InfTypeId>, TypeError> {
        Ok(self
            .tr
            .deref(ty)
            .map_err(|()| TypeError)?
            .map(|x| self.from_type_with_args(x, &ty.ty_args)))
    }
    pub fn signature(&self, ty: &NominalInfHead) -> Result<Signature, TypeError> {
        self.tr.signature(ty).map_err(|()| TypeError)
    }
    pub fn lookup(&mut self, context: HirId, name: FieldName) -> Result<InfTypeId, TypeError> {
        Ok(self.from_type(self.tr.lookup(context, name).map_err(|()| TypeError)?))
    }
    pub fn constrain(&mut self, lower: InfTypeId, upper: InfTypeId) -> Result<(), TypeError> {
        use InferenceType::*;
        if !self.cache.insert((lower, upper)) {
            return Ok(());
        }
        let mut debug_out = String::new();
        print_inftype(self.tr.db(), lower, &mut debug_out);
        debug_out.push_str(" <= ");
        print_inftype(self.tr.db(), upper, &mut debug_out);
        println!("{}", debug_out);
        let [lower_ty, upper_ty] = [lower, upper].map(|x| self.lookup_infty(x));
        match (lower_ty, upper_ty) {
            (_, Any) => Ok(()),

            (Bot, _) => Ok(()),

            (Var(var_id), _) => {
                self.var_store.get_mut(var_id).upper.push(upper);
                // idx is needed because var.lower may change during the loop
                let mut idx = 0;
                while let Some(&lower_bound) = self.var_store.get(var_id).lower.get(idx) {
                    idx += 1;
                    self.constrain(lower_bound, upper)?;
                }
                Ok(())
            }

            (_, Var(var_id)) => {
                self.var_store.get_mut(var_id).lower.push(lower);
                // idx is needed because var.lower may change during the loop
                let mut idx = 0;
                while let Some(&upper_bound) = self.var_store.get(var_id).upper.get(idx) {
                    idx += 1;
                    self.constrain(lower, upper_bound)?;
                }
                Ok(())
            }

            (Unknown, _) | (_, Unknown) => Ok(()),

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

            (Loop(kind1, inner1), Loop(kind2, inner2)) => {
                if matches!((kind1, kind2), (ArrayKind::For, ArrayKind::Each)) {
                    return Err(TypeError);
                }
                self.constrain(inner1, inner2)
            }

            (InferField(name1, inner1), InferField(name2, inner2)) => {
                if name1 != name2 {
                    return Ok(());
                }
                self.constrain(inner1, inner2)
            }

            (Nominal(ref nom), InferField(name, fieldtype)) => {
                let field_ty = self.field_type(nom, name)?;
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
                if let Some(deref) = self.deref(&l)? {
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
        self.intern_infty(inftype)
    }
    pub fn unknown(&self) -> InfTypeId {
        let inftype = InferenceType::Unknown;
        self.intern_infty(inftype)
    }
    pub fn int(&self) -> InfTypeId {
        let int = InferenceType::Primitive(PrimitiveType::Int);
        self.intern_infty(int)
    }
    pub fn char(&self) -> InfTypeId {
        let char = InferenceType::Primitive(PrimitiveType::Char);
        self.intern_infty(char)
    }
    pub fn bit(&self) -> InfTypeId {
        let bit = InferenceType::Primitive(PrimitiveType::Bit);
        self.intern_infty(bit)
    }
    pub fn single(&mut self) -> InfTypeId {
        let ty_var = self.var();
        let for_loop = self.intern_infty(InferenceType::Loop(ArrayKind::For, ty_var));
        self.intern_infty(InferenceType::ParserArg {
            result: ty_var,
            arg: for_loop,
        })
    }
    pub fn parser(&self, result: InfTypeId, arg: InfTypeId) -> InfTypeId {
        self.intern_infty(InferenceType::ParserArg { result, arg })
    }
    pub fn array_call(
        &mut self,
        kind: ArrayKind,
        inner: InfTypeId,
    ) -> Result<InfTypeId, TypeError> {
        let arg = self.var();
        let result = self.parser_apply(inner, arg)?;
        let array = InferenceType::Loop(kind, result);
        let array = self.intern_infty(array);
        Ok(self.parser(array, arg))
    }
    pub fn block_call(&mut self, id: HirId) -> Result<InfTypeId, TypeError> {
        let arg = self.var();
        let nominal = NominalInfHead {
            kind: NominalKind::Block,
            def: id,
            parse_arg: Some(arg),
            fun_args: Box::default(),
            ty_args: Box::default(),
        };
        let result = self.intern_infty(InferenceType::Nominal(nominal));
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
        let new_parser = self.intern_infty(InferenceType::ParserArg { arg, result });
        Ok(new_parser)
    }
    pub fn parser_apply(
        &mut self,
        parser: InfTypeId,
        arg: InfTypeId,
    ) -> Result<InfTypeId, TypeError> {
        let var = self.var();
        let new_parser = self.intern_infty(InferenceType::ParserArg { arg, result: var });
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
        let new_function = self.intern_infty(new_function);
        self.constrain(function, new_function)?;
        Ok(var)
    }
    pub fn access_field(
        &mut self,
        accessed: InfTypeId,
        name: FieldName,
    ) -> Result<InfTypeId, TypeError> {
        let var = self.var();
        let infer_access = self.intern_infty(InferenceType::InferField(name, var));
        self.constrain(accessed, infer_access)?;
        Ok(var)
    }
    pub fn from_type(&mut self, ty: TypeId) -> InfTypeId {
        let ty = self.tr.db().lookup_intern_type(ty);
        self.from_type_internal(&ty, None)
    }
    pub fn from_type_with_args(&mut self, ty: TypeId, args: &[InfTypeId]) -> InfTypeId {
        let ty = self.tr.db().lookup_intern_type(ty);
        match &ty {
            // we ignore the args here and replace with our own args
            Type::ForAll(inner, _args) => {
                if args.len() != _args.len() {
                    panic!("Internal Compiler Error: forall args has different length from substituted args!");
                }
                let vars = VarStack {
                    cur: args,
                    next: None,
                };
                let inner = self.tr.db().lookup_intern_type(*inner);
                self.from_type_internal(&inner, Some(&vars))
            }
            _ => self.from_type_internal(&ty, None),
        }
    }
    fn from_type_internal(&mut self, ty: &Type, var_stack: Option<&VarStack>) -> InfTypeId {
        let mut recurse =
            |x| self.from_type_internal(&self.tr.db().lookup_intern_type(x), var_stack);
        let ret = match ty {
            Type::Any => InferenceType::Any,
            Type::Bot => InferenceType::Bot,
            Type::Unknown => InferenceType::Unknown,
            Type::Primitive(p) => InferenceType::Primitive(*p),
            Type::TypeVarRef(loc, level, index) => {
                match var_stack.and_then(|x| x.resolve(*level, *index)) {
                    Some(x) => return x,
                    None => InferenceType::TypeVarRef(*loc, *level, *index),
                }
            }
            Type::ForAll(inner, defvars) => {
                let current = defvars.iter().map(|_| self.var()).collect::<Vec<_>>();
                let new_stack = VarStack {
                    cur: &current,
                    next: var_stack,
                };
                let inner = self.tr.db().lookup_intern_type(*inner);
                return self.from_type_internal(&inner, Some(&new_stack));
            }
            Type::Nominal(NominalTypeHead {
                kind,
                def,
                parse_arg,
                fun_args,
                ty_args,
            }) => {
                let parse_arg = parse_arg.map(&mut recurse);
                let fun_args = Box::new(
                    fun_args
                        .iter()
                        .copied()
                        .map(&mut recurse)
                        .collect::<Vec<_>>(),
                );
                let ty_args = Box::new(
                    ty_args
                        .iter()
                        .copied()
                        .map(&mut recurse)
                        .collect::<Vec<_>>(),
                );
                InferenceType::Nominal(NominalInfHead {
                    kind: *kind,
                    def: *def,
                    parse_arg,
                    fun_args,
                    ty_args,
                })
            }
            Type::Loop(kind, inner) => InferenceType::Loop(*kind, recurse(*inner)),
            Type::ParserArg { result, arg } => {
                let result = recurse(*result);
                let arg = recurse(*arg);
                InferenceType::ParserArg { result, arg }
            }
            Type::FunctionArg(function, args) => {
                let result = recurse(*function);
                let args = Box::new(args.iter().copied().map(recurse).collect::<Vec<_>>());
                InferenceType::FunctionArgs { result, args }
            }
        };
        self.intern_infty(ret)
    }
    pub fn to_type(&mut self, infty: InfTypeId, pol: Polarity) -> Result<TypeId, TypeError> {
        let mut converter = TypeConvertMemo::new(self);
        converter.to_type_internal(infty, pol)
    }
}

#[derive(Clone, Hash, Debug, PartialEq, Eq)]
pub struct TypeError;

pub struct VarStack<'a> {
    cur: &'a [InfTypeId],
    next: Option<&'a VarStack<'a>>,
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
    convert: MemoRecursor<(InfTypeId, Polarity), TypeId>,
    normalize: MemoRecursor<(InfTypeId, Polarity), InfTypeId>,
    meet: MemoRecursor<(InfTypeId, InfTypeId), InfTypeId>,
    join: MemoRecursor<(InfTypeId, InfTypeId), InfTypeId>,
    ctx: &'a mut InferenceContext<TR>,
}

impl<'a, TR: TypeResolver> TypeConvertMemo<'a, TR> {
    pub fn new(ctx: &'a mut InferenceContext<TR>) -> Self {
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
        let [lhs_ty, rhs_ty] = [lhs, rhs].map(|x| self.ctx.lookup_infty(x));
        match (lhs_ty, rhs_ty) {
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
                }),
                Nominal(NominalInfHead {
                    def: def2,
                    parse_arg: parse_arg2,
                    fun_args: fun_args2,
                    ty_args: ty_args2,
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
        todo!()
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
                }),
                Nominal(NominalInfHead {
                    def: def2,
                    parse_arg: parse_arg2,
                    fun_args: fun_args2,
                    ty_args: ty_args2,
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
                })
            }
            (nom @ Nominal(NominalInfHead { .. }), other)
            | (other, nom @ Nominal(NominalInfHead { .. })) => {
                let mut other_upset = HashMap::new();
                let mut next = |ty: &_| {
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
    fn normalize_children(
        &mut self,
        infty: InfTypeId,
        pol: Polarity,
    ) -> Result<InfTypeId, TypeError> {
        let mut ty = self.ctx.lookup_infty(infty);
        if let InferenceType::Var(_) = ty {
            ty = match pol {
                Polarity::Positive => InferenceType::Bot,
                Polarity::Negative => InferenceType::Any,
            };
        } else {
            for (child, pol) in ty.children(pol) {
                *child = self.normalize_inftype(*child, pol)?;
            }
        }
        Ok(self.ctx.intern_infty(ty))
    }
    fn normalize_inftype(
        &mut self,
        infty: InfTypeId,
        pol: Polarity,
    ) -> Result<InfTypeId, TypeError> {
        if let Some(x) = self.normalize.enter_fun((infty, pol)) {
            return x;
        }
        let inf = self.ctx.lookup_infty(infty);
        let res = match (inf, pol) {
            (InferenceType::Var(v), Polarity::Positive) => {
                let mut result = self.ctx.intern_infty(InferenceType::Bot);
                let var_store = self.ctx.var_store.clone();
                for ty in var_store.get(v).lower.iter() {
                    let normalized = self.normalize_children(*ty, pol)?;
                    result = self.join_inftype(result, normalized)?;
                }
                result
            }
            (InferenceType::Var(v), Polarity::Negative) => {
                let mut result = self.ctx.intern_infty(InferenceType::Any);
                let var_store = self.ctx.var_store.clone();
                for ty in var_store.get(v).upper.iter() {
                    let normalized = self.normalize_children(*ty, pol)?;
                    result = self.meet_inftype(result, normalized)?;
                }
                // the result we got upon here is the most general, however in the upper bounds there can be
                // types like InferField which are more like type classes, so we join with the lower bounds,
                // because the join operation still prioritizes concrete types over type classes
                for ty in var_store.get(v).lower.iter() {
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
            }) => {
                let parse_arg = parse_arg
                    .map(|x| self.to_type_internal(x, pol))
                    .transpose()?;
                let fun_args = fun_args
                    .iter()
                    .copied()
                    .map(|x| self.to_type_internal(x, pol))
                    .collect::<Result<_, _>>()?;
                let ty_args = ty_args
                    .iter()
                    .copied()
                    .map(|x| self.to_type_internal(x, pol))
                    .collect::<Result<_, _>>()?;
                Type::Nominal(NominalTypeHead {
                    kind,
                    def,
                    parse_arg,
                    fun_args: Arc::new(fun_args),
                    ty_args: Arc::new(ty_args),
                })
            }
            InferenceType::Loop(kind, inner) => {
                Type::Loop(kind, self.to_type_internal(inner, pol)?)
            }
            InferenceType::ParserArg { result, arg } => Type::ParserArg {
                result: self.to_type_internal(result, pol)?,
                arg: self.to_type_internal(arg, pol.reverse())?,
            },
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
            .leave_fun((infty, pol), self.ctx.tr.db().intern_type(res))
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
