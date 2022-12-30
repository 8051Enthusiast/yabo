use std::{convert::Infallible, ops::Deref};

use bumpalo::Bump;
use fxhash::FxHashMap;

use yaboc_base::low_effort_interner::{self, Uniq};

use super::*;

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct InfTypeId<'intern>(pub &'intern Uniq<InferenceType<'intern>>);

pub type InfSlice<'intern> = &'intern Uniq<[InfTypeId<'intern>]>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum InferenceType<'intern> {
    Any,
    Bot,
    Primitive(PrimitiveType),
    TypeVarRef(DefId, u32),
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
        args: InfSlice<'intern>,
    },
    InferField(FieldName, InfTypeId<'intern>),
    InferIfResult(
        Option<InfTypeId<'intern>>,
        InfTypeId<'intern>,
        InfTypeId<'intern>,
    ),
}

impl<'intern> InferenceType<'intern> {
    pub(crate) fn is_fun(&self) -> bool {
        matches!(
            self,
            InferenceType::Nominal(NominalInfHead {
                kind: NominalKind::Fun,
                ..
            })
        )
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NominalInfHead<'intern> {
    pub kind: NominalKind,
    pub def: DefId,
    pub parse_arg: Option<InfTypeId<'intern>>,
    pub fun_args: InfSlice<'intern>,
    pub ty_args: InfSlice<'intern>,
    pub internal: bool,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum ChildLocation {
    FunArg(usize),
    ParserArg,
    NomParseArg,
    NomFunArg(usize),
    NomTyArg(usize),
    FunResult,
    LoopBody,
    ParserResult,
    FieldResult,
    IfSaved,
    IfCont,
    IfBound,
}

impl ChildLocation {
    pub fn opposite_polarity(&self) -> bool {
        matches!(self, ChildLocation::FunArg(_) | ChildLocation::ParserArg)
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum InfTypeHead {
    Any,
    Bot,
    Primitive(PrimitiveType),
    TypeVarRef(DefId, u32),
    Nominal(DefId),
    Loop(ArrayKind),
    ParserArg,
    FunctionArgs(usize),
    Unknown,
    InferField(FieldName),
    InferIfResult(bool),
    Var(VarId),
}

impl<'intern> From<InfTypeId<'intern>> for InfTypeHead {
    fn from(ty: InfTypeId<'intern>) -> Self {
        match ty.value() {
            InferenceType::Any => InfTypeHead::Any,
            InferenceType::Bot => InfTypeHead::Bot,
            InferenceType::Primitive(p) => InfTypeHead::Primitive(*p),
            InferenceType::TypeVarRef(def, id) => InfTypeHead::TypeVarRef(*def, *id),
            InferenceType::Nominal(head) => InfTypeHead::Nominal(head.def),
            InferenceType::Loop(kind, _) => InfTypeHead::Loop(*kind),
            InferenceType::ParserArg { .. } => InfTypeHead::ParserArg,
            InferenceType::FunctionArgs { args, .. } => InfTypeHead::FunctionArgs(args.len()),
            InferenceType::Unknown => InfTypeHead::Unknown,
            InferenceType::InferField(name, _) => InfTypeHead::InferField(*name),
            InferenceType::InferIfResult(a, ..) => InfTypeHead::InferIfResult(a.is_some()),
            InferenceType::Var(var) => InfTypeHead::Var(*var),
        }
    }
}

impl<'intern> InfTypeId<'intern> {
    pub fn value(self) -> &'intern InferenceType<'intern> {
        &self.0 .1
    }
    pub fn try_map_children<I: InfTypeInterner<'intern>, E>(
        self,
        interner: &mut I,
        mut f: impl FnMut(&mut I, Self, ChildLocation) -> Result<Self, E>,
    ) -> Result<Self, E> {
        match self.value() {
            InferenceType::ParserArg { result, arg } => {
                let result = f(interner, *result, ChildLocation::ParserResult)?;
                let arg = f(interner, *arg, ChildLocation::ParserArg)?;
                Ok(interner.intern_infty(InferenceType::ParserArg { result, arg }))
            }
            InferenceType::FunctionArgs { result, args } => {
                let fun_args = args
                    .iter()
                    .enumerate()
                    .map(|(index, arg)| f(interner, *arg, ChildLocation::FunArg(index)))
                    .collect::<Result<Vec<_>, _>>()?;
                let args = interner.intern_infty_slice(&fun_args);
                let result = f(interner, *result, ChildLocation::FunResult)?;
                Ok(interner.intern_infty(InferenceType::FunctionArgs { result, args }))
            }
            InferenceType::Loop(kind, body) => {
                let body = f(interner, *body, ChildLocation::LoopBody)?;
                Ok(interner.intern_infty(InferenceType::Loop(*kind, body)))
            }
            InferenceType::Nominal(NominalInfHead {
                kind,
                def,
                parse_arg,
                fun_args,
                ty_args,
                internal,
            }) => {
                let parse_arg = match parse_arg {
                    Some(parse_arg) => Some(f(interner, *parse_arg, ChildLocation::NomParseArg)?),
                    None => None,
                };
                let fun_args = fun_args
                    .iter()
                    .enumerate()
                    .map(|(index, arg)| f(interner, *arg, ChildLocation::NomFunArg(index)))
                    .collect::<Result<Vec<_>, _>>()?;
                let fun_args = interner.intern_infty_slice(&fun_args);
                let ty_args = ty_args
                    .iter()
                    .enumerate()
                    .map(|(index, arg)| f(interner, *arg, ChildLocation::NomTyArg(index)))
                    .collect::<Result<Vec<_>, _>>()?;
                let ty_args = interner.intern_infty_slice(&ty_args);
                Ok(
                    interner.intern_infty(InferenceType::Nominal(NominalInfHead {
                        kind: *kind,
                        def: *def,
                        parse_arg,
                        fun_args,
                        ty_args,
                        internal: *internal,
                    })),
                )
            }
            InferenceType::InferField(name, result) => {
                let result = f(interner, *result, ChildLocation::FieldResult)?;
                Ok(interner.intern_infty(InferenceType::InferField(*name, result)))
            }
            InferenceType::InferIfResult(a, b, c) => {
                let a = if let Some(a) = a {
                    Some(f(interner, *a, ChildLocation::IfSaved)?)
                } else {
                    None
                };
                let b = f(interner, *b, ChildLocation::IfBound)?;
                let c = f(interner, *c, ChildLocation::IfCont)?;
                Ok(interner.intern_infty(InferenceType::InferIfResult(a, b, c)))
            }
            _ => Ok(self),
        }
    }
    pub fn try_for_each_child_pair<E>(
        self,
        other: Self,
        mut f: impl FnMut(Self, Self, ChildLocation) -> Result<(), E>,
    ) -> Result<(), Option<(E, ChildLocation)>> {
        let mut call_f = |a, b, loc| f(a, b, loc).map_err(|e| Some((e, loc)));
        match (self.value(), other.value()) {
            (
                InferenceType::ParserArg { result, arg },
                InferenceType::ParserArg {
                    result: other_result,
                    arg: other_arg,
                },
            ) => {
                call_f(*result, *other_result, ChildLocation::ParserResult)?;
                call_f(*arg, *other_arg, ChildLocation::ParserArg)?;
            }
            (
                InferenceType::FunctionArgs { result, args },
                InferenceType::FunctionArgs {
                    result: other_result,
                    args: other_args,
                },
            ) => {
                for (index, (arg, other_arg)) in args.iter().zip(other_args.iter()).enumerate() {
                    call_f(*arg, *other_arg, ChildLocation::FunArg(index))?;
                }
                call_f(*result, *other_result, ChildLocation::FunResult)?;
            }
            (InferenceType::Loop(_, body), InferenceType::Loop(_, other_body)) => {
                call_f(*body, *other_body, ChildLocation::LoopBody)?;
            }
            (
                InferenceType::Nominal(NominalInfHead {
                    def,
                    parse_arg,
                    fun_args,
                    ty_args,
                    ..
                }),
                InferenceType::Nominal(NominalInfHead {
                    def: other_def,
                    parse_arg: other_parse_arg,
                    fun_args: other_fun_args,
                    ty_args: other_ty_args,
                    ..
                }),
            ) => {
                if def != other_def {
                    return Err(None);
                }
                if let (Some(parse_arg), Some(other_parse_arg)) = (parse_arg, other_parse_arg) {
                    call_f(*parse_arg, *other_parse_arg, ChildLocation::NomParseArg)?;
                }
                for (index, (arg, other_arg)) in
                    fun_args.iter().zip(other_fun_args.iter()).enumerate()
                {
                    call_f(*arg, *other_arg, ChildLocation::NomFunArg(index))?;
                }
                for (index, (arg, other_arg)) in
                    ty_args.iter().zip(other_ty_args.iter()).enumerate()
                {
                    call_f(*arg, *other_arg, ChildLocation::NomTyArg(index))?;
                }
            }
            (
                InferenceType::InferField(name, result),
                InferenceType::InferField(other_name, other_result),
            ) => {
                if name != other_name {
                    return Err(None);
                }
                call_f(*result, *other_result, ChildLocation::FieldResult)?;
            }
            (
                InferenceType::InferIfResult(a1, b1, c1),
                InferenceType::InferIfResult(a2, b2, c2),
            ) => {
                if let Some((a1, a2)) = a1.zip(*a2) {
                    call_f(a1, a2, ChildLocation::IfSaved)?;
                } else if a1.is_some() != a2.is_some() {
                    return Err(None);
                }
                call_f(*b1, *b2, ChildLocation::IfBound)?;
                call_f(*c1, *c2, ChildLocation::IfCont)?;
            }
            (InferenceType::Any, InferenceType::Any)
            | (InferenceType::Bot, InferenceType::Bot)
            | (InferenceType::Primitive(_), InferenceType::Primitive(_))
            | (InferenceType::Var(_), InferenceType::Var(_))
            | (InferenceType::Unknown, InferenceType::Unknown)
            | (InferenceType::TypeVarRef(_, _), InferenceType::TypeVarRef(_, _))
                if self == other => {}
            _ => return Err(None),
        };
        Ok(())
    }
}

impl<'intern> Deref for InfTypeId<'intern> {
    type Target = InferenceType<'intern>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

pub trait InfTypeInterner<'intern> {
    fn intern_infty(&mut self, infty: InferenceType<'intern>) -> InfTypeId<'intern>;
    fn intern_infty_slice(&mut self, slice: &[InfTypeId<'intern>]) -> InfSlice<'intern>;
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
    type Error = ();

    fn try_from(value: &InferenceType) -> Result<Self, Self::Error> {
        Ok(match value {
            InferenceType::Any => TypeHead::Any,
            InferenceType::Bot => TypeHead::Bot,
            &InferenceType::Primitive(p) => TypeHead::Primitive(p),
            &InferenceType::TypeVarRef(id, idx) => TypeHead::TypeVarRef(id, idx),
            InferenceType::Nominal(NominalInfHead { def, .. }) => TypeHead::Nominal(*def),
            InferenceType::Loop(kind, _) => TypeHead::Loop(*kind),
            InferenceType::ParserArg { .. } => TypeHead::ParserArg,
            InferenceType::FunctionArgs { args, .. } => TypeHead::FunctionArgs(args.len()),
            InferenceType::Unknown => TypeHead::Unknown,
            InferenceType::Var(_)
            | InferenceType::InferField(..)
            | InferenceType::InferIfResult(..) => return Err(()),
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
    pub slice_interner: low_effort_interner::Interner<'intern, [InfTypeId<'intern>]>,
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
    fn deref(&self, ty: &NominalInfHead<'intern>)
        -> Result<Option<EitherType<'intern>>, TypeError>;
    fn signature(&self, pd: DefId) -> Result<Signature, TypeError>;
    fn lookup(&self, val: DefId) -> Result<EitherType<'intern>, TypeError>;
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
            slice_interner: low_effort_interner::Interner::new(bump),
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
            EitherType::Regular(result_type) => {
                self.convert_type_into_inftype_with_args(result_type, ty.ty_args)
            }
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
        if let Some(deref_ty) = self.tr.deref(ty)? {
            let ret = match deref_ty {
                EitherType::Regular(deref_ty) => {
                    self.convert_type_into_inftype_with_args(deref_ty, ty.ty_args)
                }
                // if we return an inference type, inference variables as return types from recursive calls
                // would cause weird higher-ranked inference which i don't want to deal with so the variables
                // are replaced with unknowns
                EitherType::Inference(deref_ty) => {
                    let unknown = self.unknown();
                    let mut replacements = FxHashMap::default();
                    // also replace the type variables with the ones in the infhead
                    for (i, replacement) in ty.ty_args.iter().enumerate() {
                        let ty_var = self.type_var(ty.def, i as u32);
                        replacements.insert(ty_var, *replacement);
                    }
                    self.replace_infvars_with(deref_ty, &mut replacements, unknown)
                }
            };
            if self.trace {
                dbeprintln!(
                    self.tr.db(),
                    "[{}] deref {}: {}",
                    &self.tr.name(),
                    &ty.def,
                    &ret
                );
            }
            Ok(Some(ret))
        } else {
            Ok(None)
        }
    }
    pub fn signature(&self, pd: DefId) -> Result<Signature, TypeError> {
        self.tr.signature(pd)
    }
    pub fn lookup(&mut self, val: DefId) -> Result<InfTypeId<'intern>, TypeError> {
        let ret = match self.tr.lookup(val) {
            Ok(EitherType::Regular(result_type)) => {
                let ret = self.convert_type_into_inftype(result_type);
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
            Ok(EitherType::Inference(infty)) => infty,
            Err(TypeError::Silenced(_)) => self.unknown(),
            Err(e) => return Err(e),
        };
        Ok(ret)
    }
    pub fn parserdef(&mut self, pd: DefId) -> Result<InfTypeId<'intern>, TypeError> {
        let sig = self.tr.signature(pd)?;
        let ret = self.tr.db().intern_type(Type::Nominal(sig.thunk));
        let vars = sig.ty_args.iter().map(|_| self.var()).collect::<Vec<_>>();
        let mut ret = self.convert_type_into_inftype_with_args(ret, &vars);
        if let Some(parser_arg) = sig.from {
            let parser_arg = self.convert_type_into_inftype_with_args(parser_arg, &vars);
            ret = self.parser(ret, parser_arg);
        }
        if let Some(fun_args) = sig.args {
            let fun_args = fun_args
                .iter()
                .map(|arg| self.convert_type_into_inftype_with_args(*arg, &vars))
                .collect::<Vec<_>>();
            let fun_args_slice = self.intern_infty_slice(&fun_args);
            ret = self.function(ret, fun_args_slice);
        }
        if self.trace {
            dbeprintln!(
                self.tr.db(),
                "[{}] parserdef {}: {}",
                &self.tr.name(),
                &pd,
                &ret
            );
        }
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
        let [lower_head, upper_head] = [lower, upper].map(InfTypeHead::from);
        if if let (InfTypeHead::Loop(f), InfTypeHead::Loop(g)) = (lower_head, upper_head) {
            f <= g
        } else {
            lower_head == upper_head
        } {
            let res = lower.try_for_each_child_pair(upper, |l, u, loc| {
                if loc.opposite_polarity() {
                    self.constrain(u, l)
                } else {
                    self.constrain(l, u)
                }
            });
            return res.map_err(|x| x.unwrap().0);
        }
        match (lower.value(), upper.value()) {
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

            (Unknown, _) => {
                upper.try_map_children(self, |ctx, child, loc| -> Result<_, TypeError> {
                    if loc.opposite_polarity() {
                        ctx.constrain(child, lower)
                    } else {
                        ctx.constrain(lower, child)
                    }?;
                    Ok(child)
                })?;
                Ok(())
            }

            (_, Unknown) => {
                lower.try_map_children(self, |ctx, child, loc| -> Result<_, TypeError> {
                    if loc.opposite_polarity() {
                        ctx.constrain(upper, child)
                    } else {
                        ctx.constrain(child, upper)
                    }?;
                    Ok(child)
                })?;
                Ok(())
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

            (Nominal(l), InferIfResult(None, res, cont)) if l.kind != NominalKind::Block => {
                let if_result_with_lower =
                    self.intern_infty(InferIfResult(Some(lower), *res, *cont));
                self.constrain(lower, if_result_with_lower)
            }

            (FunctionArgs { result, args }, FunctionArgs { args: args2, .. })
                if args.len() > args2.len() =>
            {
                let inner_args = self.intern_infty_slice(&args[args2.len()..]);
                let inner_ty = self.intern_infty(FunctionArgs {
                    result: *result,
                    args: inner_args,
                });
                let outer_args = self.intern_infty_slice(&args[..args2.len()]);
                let outer_ty = self.intern_infty(FunctionArgs {
                    result: inner_ty,
                    args: outer_args,
                });
                self.constrain(outer_ty, upper)
            }

            (Nominal(l), _) if l.kind != NominalKind::Block => {
                // if they are not the same head, try upcasting/dereferencing/evaluating
                if let Some(deref) = self.deref(l)? {
                    self.constrain(deref, upper)
                } else {
                    Err(TypeError::HeadMismatch(
                        InfTypeHead::Nominal(l.def),
                        upper.into(),
                    ))
                }
            }

            (ParserArg { result, .. }, InferIfResult(_, infer_res, cont)) => {
                self.constrain(lower, *cont)?;
                self.constrain(*result, *infer_res)
            }

            (_, InferIfResult(orig, infer_res, cont)) => {
                let orig = orig.unwrap_or(lower);
                self.constrain(orig, *cont)?;
                self.constrain(orig, *infer_res)
            }

            _ => Err(TypeError::HeadMismatch(lower.into(), upper.into())),
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
    pub fn nil(&mut self) -> InfTypeId<'intern> {
        let ty_var = self.var();
        let for_loop = self.intern_infty(InferenceType::Loop(ArrayKind::For, ty_var));
        let unit_type = self.intern_infty(InferenceType::Primitive(PrimitiveType::Unit));
        self.intern_infty(InferenceType::ParserArg {
            result: unit_type,
            arg: for_loop,
        })
    }
    pub fn regex(&mut self) -> InfTypeId<'intern> {
        let int = self.int();
        let int_array = self.array(ArrayKind::For, int);
        self.parser(int_array, int_array)
    }
    pub fn type_var(&mut self, id: DefId, index: u32) -> InfTypeId<'intern> {
        let inftype = InferenceType::TypeVarRef(id, index);
        self.intern_infty(inftype)
    }
    pub fn parser(
        &mut self,
        result: InfTypeId<'intern>,
        arg: InfTypeId<'intern>,
    ) -> InfTypeId<'intern> {
        self.intern_infty(InferenceType::ParserArg { result, arg })
    }
    pub fn function(
        &mut self,
        result: InfTypeId<'intern>,
        args: InfSlice<'intern>,
    ) -> InfTypeId<'intern> {
        self.intern_infty(InferenceType::FunctionArgs { result, args })
    }
    pub fn array(&mut self, kind: ArrayKind, inner: InfTypeId<'intern>) -> InfTypeId<'intern> {
        self.intern_infty(InferenceType::Loop(kind, inner))
    }
    pub fn if_checked(
        &mut self,
        to_be_checked: InfTypeId<'intern>,
    ) -> Result<(InfTypeId<'intern>, InfTypeId<'intern>), TypeError> {
        let result = self.var();
        let cont = self.var();
        let if_result = self.intern_infty(InferenceType::InferIfResult(None, result, cont));
        self.constrain(to_be_checked, if_result)?;
        Ok((result, cont))
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
            fun_args: self.slice_interner.intern_slice(&[]),
            ty_args: self.slice_interner.intern_slice(ty_args),
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
        let ret = self.var();
        let new_parser = self.intern_infty(InferenceType::ParserArg { arg, result: ret });
        self.constrain(parser, new_parser)?;
        Ok(ret)
    }
    pub fn function_apply(
        &mut self,
        function: InfTypeId<'intern>,
        args: &[InfTypeId<'intern>],
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let var = self.var();
        let new_function = InferenceType::FunctionArgs {
            args: self.slice_interner.intern_slice(args),
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
    pub fn replace_infvars_with(
        &mut self,
        ty: InfTypeId<'intern>,
        replace_map: &mut FxHashMap<InfTypeId<'intern>, InfTypeId<'intern>>,
        infvar_replace: InfTypeId<'intern>,
    ) -> InfTypeId<'intern> {
        if let Some(&cached) = replace_map.get(&ty) {
            return cached;
        }
        let res = if let InferenceType::Var(_) = ty.0 .1 {
            infvar_replace
        } else {
            ty.try_map_children(self, |this, child, _| -> Result<_, Infallible> {
                Ok(this.replace_infvars_with(child, replace_map, infvar_replace))
            })
            .unwrap()
        };
        replace_map.insert(ty, res);
        res
    }
    pub fn convert_type_into_inftype(&mut self, ty: TypeId) -> InfTypeId<'intern> {
        let ty = self.tr.db().lookup_intern_type(ty);
        self.convert_type_into_inftype_internal(&ty, None)
    }
    pub fn convert_type_into_inftype_with_args(
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
        let vars = TyVars { cur: args };
        match &ty {
            // we ignore the args here and replace with our own args
            Type::ForAll(inner, _args) => {
                if args.len() != _args.len() {
                    panic!("Internal Compiler Error: forall args has different length from substituted args!");
                }
                let inner = self.tr.db().lookup_intern_type(*inner);
                self.convert_type_into_inftype_internal(&inner, Some(&vars))
            }
            _ => self.convert_type_into_inftype_internal(&ty, Some(&vars)),
        }
    }
    fn convert_type_into_inftype_internal(
        &mut self,
        ty: &Type,
        var_stack: Option<&TyVars<'_, 'intern>>,
    ) -> InfTypeId<'intern> {
        let mut recurse = |x| {
            self.convert_type_into_inftype_internal(&self.tr.db().lookup_intern_type(x), var_stack)
        };
        let ret = match ty {
            Type::Any => InferenceType::Any,
            Type::Bot => InferenceType::Bot,
            Type::Unknown => InferenceType::Unknown,
            Type::Primitive(p) => InferenceType::Primitive(*p),
            Type::TypeVarRef(loc, index) => match var_stack.and_then(|x| x.resolve(*index)) {
                Some(x) => return x,
                None => InferenceType::TypeVarRef(*loc, *index),
            },
            Type::ForAll(inner, defvars) => {
                let current = defvars.iter().map(|_| self.var()).collect::<Vec<_>>();
                let new_stack = TyVars { cur: &current };
                let inner = self.tr.db().lookup_intern_type(*inner);
                return self.convert_type_into_inftype_internal(&inner, Some(&new_stack));
            }
            Type::Nominal(NominalTypeHead {
                kind,
                def,
                parse_arg,
                fun_args,
                ty_args,
            }) => {
                let parse_arg = parse_arg.map(&mut recurse);
                let fun_args = fun_args
                    .iter()
                    .copied()
                    .map(&mut recurse)
                    .collect::<Vec<_>>();
                let ty_args = ty_args
                    .iter()
                    .copied()
                    .map(&mut recurse)
                    .collect::<Vec<_>>();
                let fun_args = self.slice_interner.intern_slice(&fun_args);
                let ty_args = self.slice_interner.intern_slice(&ty_args);
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
                let args_vec = args.iter().copied().map(recurse).collect::<Vec<_>>();
                let args = self.slice_interner.intern_slice(&args_vec);
                InferenceType::FunctionArgs { result, args }
            }
        };
        self.intern_infty(ret)
    }
    pub fn to_type(&mut self, infty: InfTypeId<'intern>) -> Result<TypeId, TypeError> {
        let mut converter = TypeConvertMemo::new(self);
        converter.convert_to_type_internal(infty)
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
            let (new_ty, new_n) =
                converter.convert_to_type_internal_with_vars(*infty, n_vars, at)?;
            n_vars = new_n;
            ret.push(new_ty);
        }
        Ok((ret, n_vars))
    }
}
impl<'intern, TR: TypeResolver<'intern>> InfTypeInterner<'intern>
    for InferenceContext<'intern, TR>
{
    fn intern_infty(&mut self, infty: InferenceType<'intern>) -> InfTypeId<'intern> {
        self.intern_infty(infty)
    }

    fn intern_infty_slice(&mut self, slice: &[InfTypeId<'intern>]) -> InfSlice<'intern> {
        self.slice_interner.intern_slice(slice)
    }
}
