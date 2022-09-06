use std::ops::Deref;

use bumpalo::Bump;

use crate::low_effort_interner::{self, Uniq};

use super::*;

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct InfTypeId<'intern>(&'intern Uniq<InferenceType<'intern>>);

impl<'intern> InfTypeId<'intern> {
    pub fn value(self) -> &'intern InferenceType<'intern> {
        &self.0 .0
    }
}

impl<'intern> Deref for InfTypeId<'intern> {
    type Target = InferenceType<'intern>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum InferenceType<'intern> {
    Any,
    Bot,
    Primitive(PrimitiveType),
    TypeVarRef(DefId, u32, u32),
    Var(VarId),
    Unknown,
    Nominal(NominalInfHead<'intern>),
    Loop(ArrayKind, InfTypeId<'intern>),
    ParserArg {
        result: InfTypeId<'intern>,
        arg: InfTypeId<'intern>,
    },
    FunctionArgs {
        result: InfTypeId<'intern>,
        args: Box<Vec<InfTypeId<'intern>>>,
    },
    InferField(FieldName, InfTypeId<'intern>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NominalInfHead<'intern> {
    pub kind: NominalKind,
    pub def: DefId,
    pub parse_arg: Option<InfTypeId<'intern>>,
    pub fun_args: Box<Vec<InfTypeId<'intern>>>,
    pub ty_args: Box<Vec<InfTypeId<'intern>>>,
    pub internal: bool,
}

impl<'intern> InferenceType<'intern> {
    pub fn children(&mut self) -> Vec<&mut InfTypeId<'intern>> {
        match self {
            InferenceType::Loop(_, inner) => {
                vec![inner]
            }
            InferenceType::ParserArg { result, arg } => {
                vec![result, arg]
            }
            InferenceType::FunctionArgs { result, args } => {
                let mut ret = args.iter_mut().collect::<Vec<_>>();
                ret.push(result);
                ret
            }
            InferenceType::InferField(_, inner) => {
                vec![inner]
            }
            InferenceType::Var(..) => {
                panic!("Internal Compiler Error: taking children of variable");
            }
            InferenceType::Nominal(nom) => nom.ty_args.iter_mut().collect(),
            InferenceType::Any
            | InferenceType::Bot
            | InferenceType::Primitive(_)
            | InferenceType::TypeVarRef(_, _, _)
            | InferenceType::Unknown => vec![],
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarId(pub(super) usize);

impl VarId {
    fn usize(&self) -> usize {
        self.0
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct InferenceVar<'a> {
    id: usize,
    upper: Vec<InfTypeId<'a>>,
    lower: Vec<InfTypeId<'a>>,
}

impl<'intern> InferenceVar<'intern> {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            upper: Vec::new(),
            lower: Vec::new(),
        }
    }
    pub fn lower(&self) -> &[InfTypeId<'intern>] {
        &self.lower
    }
    pub fn upper(&self) -> &[InfTypeId<'intern>] {
        &self.upper
    }
}

impl<'intern> TryFrom<&InferenceType<'intern>> for TypeHead {
    type Error = TypeError;

    fn try_from(value: &InferenceType) -> Result<Self, Self::Error> {
        Ok(match value {
            InferenceType::Any => TypeHead::Any,
            InferenceType::Bot => TypeHead::Bot,
            &InferenceType::Primitive(p) => TypeHead::Primitive(p),
            &InferenceType::TypeVarRef(id, lvl, idx) => TypeHead::TypeVarRef(id, lvl, idx),
            InferenceType::Nominal(NominalInfHead { def, .. }) => TypeHead::Nominal(*def),
            InferenceType::Loop(kind, _) => TypeHead::Loop(*kind),
            InferenceType::ParserArg { .. } => TypeHead::ParserArg,
            InferenceType::FunctionArgs { args, .. } => TypeHead::FunctionArgs(args.len()),
            InferenceType::Unknown => TypeHead::Unknown,
            InferenceType::Var(_) | InferenceType::InferField(..) => return Err(TypeError),
        })
    }
}

#[derive(Clone, Default)]
pub struct VarStore<'intern> {
    vars: Vec<InferenceVar<'intern>>,
}

impl<'intern> VarStore<'intern> {
    fn new() -> Self {
        Self::default()
    }
    fn add_var(&mut self) -> VarId {
        let id = self.vars.len();
        self.vars.push(InferenceVar::new(id));
        VarId(id)
    }
    pub fn get(&self, id: VarId) -> &InferenceVar<'intern> {
        &self.vars[id.usize()]
    }
    fn get_mut(&mut self, id: VarId) -> &mut InferenceVar<'intern> {
        &mut self.vars[id.usize()]
    }
}

pub struct InferenceContext<'intern, TR: TypeResolver<'intern>> {
    pub var_store: VarStore<'intern>,
    pub tr: TR,
    pub interner: low_effort_interner::Interner<'intern, InferenceType<'intern>>,
    cache: HashSet<(InfTypeId<'intern>, InfTypeId<'intern>)>,
    trace: bool,
}

pub trait TypeResolver<'intern> {
    type DB: TypeInterner + Interner + ?Sized;
    fn db(&self) -> &Self::DB;
    fn field_type(
        &self,
        ty: &NominalInfHead<'intern>,
        name: FieldName,
    ) -> Result<EitherType<'intern>, TypeError>;
    fn deref(&self, ty: &NominalInfHead<'intern>) -> Result<Option<TypeId>, TypeError>;
    fn signature(&self, ty: &NominalInfHead<'intern>) -> Result<Signature, TypeError>;
    fn lookup(&self, val: DefId) -> Result<EitherType<'intern>, TypeError>;
    fn parserdef(&self, pd: DefId) -> Result<EitherType<'intern>, TypeError>;
    fn name(&self) -> String;
}

const TRACING_ENABLED: bool = false;

impl<'intern, TR: TypeResolver<'intern>> InferenceContext<'intern, TR> {
    pub fn new(tr: TR, bump: &'intern Bump) -> Self {
        Self {
            var_store: VarStore::new(),
            tr,
            cache: HashSet::new(),
            trace: TRACING_ENABLED,
            interner: low_effort_interner::Interner::new(bump),
        }
    }
    pub fn intern_infty(&mut self, infty: InferenceType<'intern>) -> InfTypeId<'intern> {
        InfTypeId(self.interner.intern(infty))
    }
    pub fn field_type(
        &mut self,
        ty: &NominalInfHead<'intern>,
        name: FieldName,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let ret = match self.tr.field_type(ty, name)? {
            EitherType::Regular(result_type) => self.from_type_with_args(result_type, &ty.ty_args),
            EitherType::Inference(infty) => infty,
        };
        if self.trace {
            dbeprintln!(
                self.tr.db(),
                "[{}] field_type {} of {}: {}",
                &self.tr.name(),
                &name,
                &ty.def,
                &ret
            );
        }
        Ok(ret)
    }
    pub fn deref(
        &mut self,
        ty: &NominalInfHead<'intern>,
    ) -> Result<Option<InfTypeId<'intern>>, TypeError> {
        let ret = self
            .tr
            .deref(ty)?
            .map(|x| self.from_type_with_args(x, &ty.ty_args));
        if self.trace {
            if let Some(x) = ret {
                dbeprintln!(
                    self.tr.db(),
                    "[{}] deref {}: {}",
                    &self.tr.name(),
                    &ty.def,
                    &x
                );
            }
        }
        Ok(ret)
    }
    pub fn signature(&self, ty: &NominalInfHead<'intern>) -> Result<Signature, TypeError> {
        self.tr.signature(ty)
    }
    pub fn lookup(&mut self, val: DefId) -> Result<InfTypeId<'intern>, TypeError> {
        let ret = match self.tr.lookup(val)? {
            EitherType::Regular(result_type) => {
                let ret = self.from_type(result_type);
                if self.trace {
                    dbeprintln!(
                        self.tr.db(),
                        "[{}] lookup {}: {}",
                        &self.tr.name(),
                        &val,
                        &ret
                    );
                }
                ret
            }
            EitherType::Inference(infty) => infty,
        };
        Ok(ret)
    }
    pub fn parserdef(&mut self, pd: DefId) -> Result<InfTypeId<'intern>, TypeError> {
        let ret = match self.tr.parserdef(pd)? {
            EitherType::Regular(result_type) => {
                let ret = self.from_type(result_type);
                if self.trace {
                    dbeprintln!(
                        self.tr.db(),
                        "[{}] parserdef {}: {}",
                        &self.tr.name(),
                        &pd,
                        &ret
                    );
                }
                ret
            }
            EitherType::Inference(infty) => infty,
        };
        Ok(ret)
    }
    pub fn constrain(
        &mut self,
        lower: InfTypeId<'intern>,
        upper: InfTypeId<'intern>,
    ) -> Result<(), TypeError> {
        use InferenceType::*;
        if !self.cache.insert((lower, upper)) {
            return Ok(());
        }
        if self.trace {
            dbeprintln!(
                self.tr.db(),
                "[{}] constrain: {} :< {}",
                &self.tr.name(),
                &lower,
                &upper
            );
        }
        let [lower_ty, upper_ty] = [lower, upper].map(|x| x.value());
        match (lower_ty, upper_ty) {
            (_, Any) => Ok(()),

            (Bot, _) => Ok(()),

            (Var(var_id), _) => {
                self.var_store.get_mut(*var_id).upper.push(upper);
                // idx is needed because var.lower may change during the loop
                let mut idx = 0;
                while let Some(&lower_bound) = self.var_store.get(*var_id).lower.get(idx) {
                    idx += 1;
                    self.constrain(lower_bound, upper)?;
                }
                Ok(())
            }

            (_, Var(var_id)) => {
                self.var_store.get_mut(*var_id).lower.push(lower);
                // idx is needed because var.lower may change during the loop
                let mut idx = 0;
                while let Some(&upper_bound) = self.var_store.get(*var_id).upper.get(idx) {
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
                self.constrain(*res1, *res2)?;
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
                self.constrain(*res1, *res2)?;
                self.constrain(*arg2, *arg1)
            }

            (Loop(kind1, inner1), Loop(kind2, inner2)) => {
                if matches!((kind1, kind2), (ArrayKind::For, ArrayKind::Each)) {
                    return Err(TypeError);
                }
                self.constrain(*inner1, *inner2)
            }

            (InferField(name1, inner1), InferField(name2, inner2)) => {
                if name1 != name2 {
                    return Ok(());
                }
                self.constrain(*inner1, *inner2)
            }

            (
                Nominal(
                    ref nom @ NominalInfHead {
                        kind: NominalKind::Block,
                        ..
                    },
                ),
                InferField(name, fieldtype),
            ) => {
                let field_ty = self.field_type(nom, *name)?;
                self.constrain(field_ty, *fieldtype)
            }

            (
                Nominal(NominalInfHead {
                    def: def1,
                    fun_args: type_args1,
                    parse_arg: parse_arg1,
                    ..
                }),
                Nominal(NominalInfHead {
                    def: def2,
                    fun_args: type_args2,
                    parse_arg: parse_arg2,
                    ..
                }),
            ) if def1 == def2 => {
                if type_args1.len() != type_args2.len() {
                    panic!("Internal Compiler Error: compared same nominal types do not have same number of type args");
                }
                if parse_arg1.is_some() != parse_arg2.is_some() {
                    panic!("Internal Compiler Error: compared same nominal types do not have same number of from args");
                }
                type_args1
                    .iter()
                    .chain(parse_arg1.iter())
                    .zip(type_args2.iter().chain(parse_arg2.iter()))
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

            (TypeVarRef(loc, level, index), TypeVarRef(loc2, level2, index2)) => {
                if loc != loc2 || level != level2 || index != index2 {
                    Err(TypeError)
                } else {
                    Ok(())
                }
            }

            _ => Err(TypeError),
        }
    }
    pub fn equal(
        &mut self,
        left: InfTypeId<'intern>,
        right: InfTypeId<'intern>,
    ) -> Result<(), TypeError> {
        self.constrain(left, right)?;
        self.constrain(right, left)
    }
    pub fn var(&mut self) -> InfTypeId<'intern> {
        let inftype = InferenceType::Var(self.var_store.add_var());
        self.intern_infty(inftype)
    }
    pub fn unknown(&mut self) -> InfTypeId<'intern> {
        let inftype = InferenceType::Unknown;
        self.intern_infty(inftype)
    }
    pub fn int(&mut self) -> InfTypeId<'intern> {
        let int = InferenceType::Primitive(PrimitiveType::Int);
        self.intern_infty(int)
    }
    pub fn char(&mut self) -> InfTypeId<'intern> {
        let char = InferenceType::Primitive(PrimitiveType::Char);
        self.intern_infty(char)
    }
    pub fn bit(&mut self) -> InfTypeId<'intern> {
        let bit = InferenceType::Primitive(PrimitiveType::Bit);
        self.intern_infty(bit)
    }
    pub fn single(&mut self) -> InfTypeId<'intern> {
        let ty_var = self.var();
        let for_loop = self.intern_infty(InferenceType::Loop(ArrayKind::For, ty_var));
        self.intern_infty(InferenceType::ParserArg {
            result: ty_var,
            arg: for_loop,
        })
    }
    pub fn parser(
        &mut self,
        result: InfTypeId<'intern>,
        arg: InfTypeId<'intern>,
    ) -> InfTypeId<'intern> {
        self.intern_infty(InferenceType::ParserArg { result, arg })
    }
    pub fn array_call(
        &mut self,
        kind: ArrayKind,
        inner: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let arg = self.var();
        let result = self.parser_apply(inner, arg)?;
        let array = InferenceType::Loop(kind, result);
        let array = self.intern_infty(array);
        Ok(self.parser(array, arg))
    }
    pub fn block_call(
        &mut self,
        id: DefId,
        ty_args: &[InfTypeId<'intern>],
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let arg = self.var();
        let nominal = NominalInfHead {
            kind: NominalKind::Block,
            def: id,
            parse_arg: Some(arg),
            fun_args: Box::default(),
            ty_args: Box::new(ty_args.to_vec()),
            internal: true,
        };
        let result = self.intern_infty(InferenceType::Nominal(nominal));
        Ok(self.parser(result, arg))
    }
    pub fn one_of(
        &mut self,
        these: &[InfTypeId<'intern>],
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let ret = self.var();
        for &inftype in these {
            self.constrain(inftype, ret)?;
        }
        Ok(ret)
    }
    pub fn reuse_parser_arg(
        &mut self,
        parser: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let result = self.var();
        let arg = self.var();
        let other_parser = self.parser(result, arg);
        self.constrain(other_parser, parser)?;
        Ok(arg)
    }
    pub fn parser_compose(
        &mut self,
        first: InfTypeId<'intern>,
        second: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let arg = self.var();
        let between = self.parser_apply(first, arg)?;
        let result = self.parser_apply(second, between)?;
        let new_parser = self.intern_infty(InferenceType::ParserArg { arg, result });
        Ok(new_parser)
    }
    pub fn parser_apply(
        &mut self,
        parser: InfTypeId<'intern>,
        arg: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let var = self.var();
        let new_parser = self.intern_infty(InferenceType::ParserArg { arg, result: var });
        self.constrain(parser, new_parser)?;
        Ok(var)
    }
    pub fn function_apply(
        &mut self,
        function: InfTypeId<'intern>,
        args: &[InfTypeId<'intern>],
    ) -> Result<InfTypeId<'intern>, TypeError> {
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
        accessed: InfTypeId<'intern>,
        name: FieldName,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let var = self.var();
        let infer_access = self.intern_infty(InferenceType::InferField(name, var));
        self.constrain(accessed, infer_access)?;
        Ok(var)
    }
    pub fn from_type(&mut self, ty: TypeId) -> InfTypeId<'intern> {
        let ty = self.tr.db().lookup_intern_type(ty);
        self.from_type_internal(&ty, None)
    }
    pub fn from_type_with_args(
        &mut self,
        ty: TypeId,
        args: &[InfTypeId<'intern>],
    ) -> InfTypeId<'intern> {
        if self.trace {
            let args = args
                .iter()
                .map(|id| dbformat!(self.tr.db(), "{}, ", id))
                .collect::<String>();
            eprintln!(
                "[{}] from_type_with_args: forall [{}]",
                &self.tr.name(),
                &args
            );
        }
        let ty = self.tr.db().lookup_intern_type(ty);
        let vars = VarStack {
            cur: args,
            next: None,
        };
        match &ty {
            // we ignore the args here and replace with our own args
            Type::ForAll(inner, _args) => {
                if args.len() != _args.len() {
                    panic!("Internal Compiler Error: forall args has different length from substituted args!");
                }
                let inner = self.tr.db().lookup_intern_type(*inner);
                self.from_type_internal(&inner, Some(&vars))
            }
            _ => self.from_type_internal(&ty, Some(&vars)),
        }
    }
    fn from_type_internal(
        &mut self,
        ty: &Type,
        var_stack: Option<&VarStack<'_, 'intern>>,
    ) -> InfTypeId<'intern> {
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
                    internal: false,
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
    pub fn to_type(&mut self, infty: InfTypeId<'intern>) -> Result<TypeId, TypeError> {
        let mut converter = TypeConvertMemo::new(self);
        converter.to_type_internal(infty)
    }
    pub fn to_types_with_vars(
        &mut self,
        inftys: &[InfTypeId<'intern>],
        mut n_vars: u32,
        at: DefId,
    ) -> Result<(Vec<TypeId>, u32), TypeError> {
        let mut converter = TypeConvertMemo::new(self);
        let mut ret = Vec::new();
        for infty in inftys {
            let (new_ty, new_n) = converter.to_type_internal_with_vars(*infty, n_vars, at)?;
            n_vars = new_n;
            ret.push(new_ty);
        }
        Ok((ret, n_vars))
    }
}
