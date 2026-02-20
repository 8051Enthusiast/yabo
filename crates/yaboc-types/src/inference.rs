use std::{convert::Infallible, ops::Deref};

use bumpalo::Bump;
use ena::unify::InPlaceUnificationTable;
use fxhash::FxHashMap;

use yaboc_base::{
    error::SResult,
    low_effort_interner::{self, Uniq},
};

use crate::binary_tree::BinaryTree;

use self::connections::Connections;

use super::*;

pub const TRACING_ENABLED: bool = false;

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct InfTypeId<'intern>(pub &'intern Uniq<InferenceType<InfTypeId<'intern>>>);

pub type InfSlice<'intern> = &'intern Uniq<[InfTypeId<'intern>]>;

pub trait AsArray: Sized {
    type ArrayType: Clone + std::fmt::Debug + Hash + PartialEq + Eq;
    fn slice(arr: &Self::ArrayType) -> &[Self];
}

impl<'intern> AsArray for InfTypeId<'intern> {
    type ArrayType = InfSlice<'intern>;
    fn slice(arr: &Self::ArrayType) -> &[Self] {
        &arr.1
    }
}

impl<T: Clone + std::fmt::Debug + Hash + PartialEq + Eq> AsArray for Vec<T> {
    type ArrayType = Vec<Vec<T>>;
    fn slice(arr: &Self::ArrayType) -> &[Self] {
        arr
    }
}

pub trait MakeArray<Elements: AsArray> {
    fn make_array(&mut self, elements: &[Elements]) -> Elements::ArrayType;
}

impl<'intern, I: InfTypeInterner<'intern>> MakeArray<InfTypeId<'intern>> for I {
    fn make_array(&mut self, elements: &[InfTypeId<'intern>]) -> InfSlice<'intern> {
        self.intern_infty_slice(elements)
    }
}

impl<T: Clone + std::fmt::Debug + Hash + PartialEq + Eq> MakeArray<Vec<T>> for () {
    fn make_array(&mut self, elements: &[Vec<T>]) -> Vec<Vec<T>> {
        elements.to_vec()
    }
}

enum Constraint<'intern> {
    HasField(FieldName, InfTypeId<'intern>),
    SizeOf,
    ParserIf(InfTypeId<'intern>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum InferenceType<Ty: AsArray> {
    Primitive(PrimitiveType),
    TypeVarRef(TypeVarRef),
    Var(VarId),
    Unknown,
    Block(BlockInfHead<Ty>),
    Loop(ArrayKind, Ty),
    ParserArg { result: Ty, arg: Ty },
    FunArgCons(Ty, Ty),
    FunArgNil,
    FunctionArgs { result: Ty, args: Ty },
}

impl<Inner: AsArray> InferenceType<Inner> {
    pub fn try_map<Out: AsArray, Ctx: MakeArray<Out>, E>(
        self,
        ctx: &mut Ctx,
        mut f: impl FnMut(&mut Ctx, Inner, ChildLocation) -> Result<Out, E>,
    ) -> Result<InferenceType<Out>, E>
    where
        Inner: Clone,
    {
        Ok(match self {
            InferenceType::ParserArg { result, arg } => InferenceType::ParserArg {
                result: f(ctx, result, ChildLocation::ParserResult)?,
                arg: f(ctx, arg, ChildLocation::ParserArg)?,
            },
            InferenceType::FunctionArgs { result, args } => {
                let result = f(ctx, result, ChildLocation::FunResult)?;
                let args = f(ctx, args, ChildLocation::FunArg)?;
                InferenceType::FunctionArgs { result, args }
            }
            InferenceType::Loop(kind, body) => {
                InferenceType::Loop(kind, f(ctx, body, ChildLocation::LoopBody)?)
            }
            InferenceType::Block(BlockInfHead {
                def,
                parse_arg,
                ty_args,
                internal,
            }) => {
                let parse_arg = parse_arg
                    .map(|x| f(ctx, x, ChildLocation::BlockParseArg))
                    .transpose()?;
                let ty_args = Inner::slice(&ty_args)
                    .iter()
                    .map(|arg| f(ctx, arg.clone(), ChildLocation::BlockTyArg(0)))
                    .collect::<Result<Vec<_>, _>>()?;
                InferenceType::Block(BlockInfHead {
                    def,
                    parse_arg,
                    ty_args: ctx.make_array(&ty_args),
                    internal,
                })
            }
            InferenceType::Primitive(p) => InferenceType::Primitive(p),
            InferenceType::TypeVarRef(v) => InferenceType::TypeVarRef(v),
            InferenceType::Var(v) => InferenceType::Var(v),
            InferenceType::Unknown => InferenceType::Unknown,
            InferenceType::FunArgCons(lhs, rhs) => InferenceType::FunArgCons(
                f(ctx, lhs, ChildLocation::FunArgConsHead)?,
                f(ctx, rhs, ChildLocation::FunArgConsTail)?,
            ),
            InferenceType::FunArgNil => InferenceType::FunArgNil,
        })
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct BlockInfHead<Ty: AsArray> {
    pub def: DefId,
    pub parse_arg: Option<Ty>,
    pub ty_args: <Ty as AsArray>::ArrayType,
    pub internal: bool,
}

pub type InternedBlockHead<'intern> = BlockInfHead<InfTypeId<'intern>>;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum ChildLocation {
    FunArg,
    FunArgConsHead,
    FunArgConsTail,
    ParserArg,
    BlockParseArg,
    BlockFunArg(usize),
    BlockTyArg(usize),
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
        matches!(self, ChildLocation::FunArg | ChildLocation::ParserArg)
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Application {
    Partial,
    Full,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum InfTypeHead {
    Primitive(PrimitiveType),
    TypeVarRef(TypeVarRef),
    Block(DefId, bool),
    Loop(ArrayKind),
    ParserArg,
    FunctionArgs,
    FunctionArgCons,
    FunctionArgNil,
    Unknown,
    Var(VarId),
}

impl<T: AsArray> From<&InferenceType<T>> for InfTypeHead {
    fn from(ty: &InferenceType<T>) -> Self {
        match ty {
            InferenceType::Primitive(p) => InfTypeHead::Primitive(*p),
            InferenceType::TypeVarRef(var) => InfTypeHead::TypeVarRef(*var),
            InferenceType::Block(head) => InfTypeHead::Block(head.def, head.parse_arg.is_some()),
            InferenceType::Loop(kind, _) => InfTypeHead::Loop(*kind),
            InferenceType::ParserArg { .. } => InfTypeHead::ParserArg,
            InferenceType::FunctionArgs { .. } => InfTypeHead::FunctionArgs,
            InferenceType::FunArgCons(..) => InfTypeHead::FunctionArgCons,
            InferenceType::FunArgNil => InfTypeHead::FunctionArgNil,
            InferenceType::Unknown => InfTypeHead::Unknown,
            InferenceType::Var(var) => InfTypeHead::Var(*var),
        }
    }
}

impl<'intern> From<InfTypeId<'intern>> for InfTypeHead {
    fn from(ty: InfTypeId<'intern>) -> Self {
        ty.value().into()
    }
}

impl<'intern> InfTypeId<'intern> {
    pub fn value(self) -> &'intern InferenceType<InfTypeId<'intern>> {
        &self.0 .1
    }
    pub fn child_arrays(self) -> InferenceType<Vec<Self>> {
        self.value()
            .clone()
            .try_map(&mut (), |_, x, _| Ok::<_, Infallible>(vec![x]))
            .unwrap()
    }
    pub fn try_map_children<I: InfTypeInterner<'intern>, E>(
        self,
        interner: &mut I,
        mut f: impl FnMut(&mut I, Self, ChildLocation) -> Result<Self, E>,
    ) -> Result<Self, E> {
        let res = self
            .value()
            .clone()
            .try_map(interner, |ctx, x, loc| f(ctx, x, loc))?;
        Ok(interner.intern_infty(res))
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
                InferenceType::FunctionArgs { result, args, .. },
                InferenceType::FunctionArgs {
                    result: other_result,
                    args: other_arg,
                    ..
                },
            ) => {
                call_f(*args, *other_arg, ChildLocation::FunArg)?;
                call_f(*result, *other_result, ChildLocation::FunResult)?;
            }
            (InferenceType::FunArgCons(head1, tail1), InferenceType::FunArgCons(head2, tail2)) => {
                call_f(*head1, *head2, ChildLocation::FunArgConsHead)?;
                call_f(*tail1, *tail2, ChildLocation::FunArgConsTail)?;
            }
            (InferenceType::Loop(_, body), InferenceType::Loop(_, other_body)) => {
                call_f(*body, *other_body, ChildLocation::LoopBody)?;
            }
            (
                InferenceType::Block(BlockInfHead {
                    def,
                    parse_arg,
                    ty_args,
                    ..
                }),
                InferenceType::Block(BlockInfHead {
                    def: other_def,
                    parse_arg: other_parse_arg,
                    ty_args: other_ty_args,
                    ..
                }),
            ) => {
                if def != other_def {
                    return Err(None);
                }
                if let (Some(parse_arg), Some(other_parse_arg)) = (parse_arg, other_parse_arg) {
                    call_f(*parse_arg, *other_parse_arg, ChildLocation::BlockParseArg)?;
                }
                for (index, (arg, other_arg)) in
                    ty_args.iter().zip(other_ty_args.iter()).enumerate()
                {
                    call_f(*arg, *other_arg, ChildLocation::BlockTyArg(index))?;
                }
            }
            (
                InferenceType::Primitive(_)
                | InferenceType::Var(_)
                | InferenceType::Unknown
                | InferenceType::TypeVarRef(_)
                | InferenceType::FunArgNil,
                _,
            ) if self == other => {}
            _ => return Err(None),
        };
        Ok(())
    }
}

impl<'intern> Deref for InfTypeId<'intern> {
    type Target = InferenceType<InfTypeId<'intern>>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

pub trait InfTypeInterner<'intern> {
    fn intern_infty(&mut self, infty: InferenceType<InfTypeId<'intern>>) -> InfTypeId<'intern>;
    fn intern_infty_slice(&mut self, slice: &[InfTypeId<'intern>]) -> InfSlice<'intern>;
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarId(pub(super) u32);

impl VarId {
    fn usize(&self) -> usize {
        self.0 as usize
    }
}

impl ena::unify::UnifyKey for VarId {
    type Value = ();
    fn index(&self) -> u32 {
        self.0
    }
    fn from_index(u: u32) -> Self {
        VarId(u)
    }
    fn tag() -> &'static str {
        "typecheck"
    }
}

pub struct InferenceContext<'intern, TR: TypeResolver<'intern>> {
    pub tr: TR,
    pub interner: low_effort_interner::Interner<'intern, InferenceType<InfTypeId<'intern>>>,
    pub slice_interner: low_effort_interner::Interner<'intern, [InfTypeId<'intern>]>,
    constraints: Vec<BinaryTree<Constraint<'intern>>>,
    unify: InPlaceUnificationTable<VarId>,
    instantiations: Vec<Option<InfTypeId<'intern>>>,
    trace: bool,
    connections: Connections,
}

pub trait TypeResolver<'intern> {
    type DB: TypeInterner + Interner + ?Sized;
    fn db(&self) -> &Self::DB;
    fn field_type(
        &self,
        ty: &InternedBlockHead<'intern>,
        name: FieldName,
    ) -> Result<EitherType<'intern>, TypeError>;
    fn signature(&self, pd: DefId) -> SResult<(EitherType<'intern>, usize)>;
    fn lookup(&self, val: DefId) -> Result<EitherType<'intern>, TypeError>;
    fn name(&self) -> String;
}

impl<'intern, TR: TypeResolver<'intern>> InferenceContext<'intern, TR> {
    pub fn new(tr: TR, bump: &'intern Bump) -> Self {
        Self {
            tr,
            trace: TRACING_ENABLED,
            interner: low_effort_interner::Interner::new(bump),
            slice_interner: low_effort_interner::Interner::new(bump),
            connections: Connections::new(),
            constraints: Default::default(),
            unify: Default::default(),
            instantiations: Default::default(),
        }
    }
    pub fn intern_infty(
        &'_ mut self,
        infty: InferenceType<InfTypeId<'intern>>,
    ) -> InfTypeId<'intern> {
        InfTypeId(self.interner.intern(infty))
    }

    pub fn field_type(
        &mut self,
        ty: &InternedBlockHead<'intern>,
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

    pub fn lookup(&mut self, val: DefId) -> Result<InfTypeId<'intern>, TypeError> {
        if self.trace {
            dbeprintln!(self.tr.db(), "[{}] looking up {}", &self.tr.name(), &val,);
        }
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

    pub fn parserdef(&mut self, pd: DefId) -> SResult<InfTypeId<'intern>> {
        let (ty, tyvar_count) = self.tr.signature(pd)?;
        let ty = match ty {
            EitherType::Regular(type_id) => {
                let vars = (0..tyvar_count).map(|_| self.var()).collect::<Vec<_>>();
                self.convert_type_into_inftype_with_args(type_id, &vars)
            }
            EitherType::Inference(inf_type_id) => inf_type_id,
        };
        if self.trace {
            dbeprintln!(
                self.tr.db(),
                "[{}] parserdef {}: {}",
                &self.tr.name(),
                &pd,
                &ty
            );
        }
        Ok(ty)
    }

    fn has_field(
        &mut self,
        ty: InfTypeId<'intern>,
        name: FieldName,
        field_type: InfTypeId<'intern>,
    ) -> Result<(), TypeError> {
        if let InferenceType::Block(block) = ty.value() {
            let f = self.field_type(block, name)?;
            return self.constrain(f, field_type);
        }
        Err(TypeError::UnknownField(name))
    }

    fn sized(&mut self, ty: InfTypeId<'intern>) -> Result<(), TypeError> {
        if !matches!(
            ty.value(),
            InferenceType::Loop(..) | InferenceType::ParserArg { .. }
        ) {
            return Err(TypeError::NotSized);
        }
        Ok(())
    }

    fn parser_if(
        &mut self,
        ty: InfTypeId<'intern>,
        target: InfTypeId<'intern>,
    ) -> Result<(), TypeError> {
        match ty.value() {
            InferenceType::ParserArg { result, .. } => self.constrain(*result, target),
            _ => self.constrain(ty, target),
        }
    }

    fn apply_constraint(
        &mut self,
        ty: InfTypeId<'intern>,
        constraint: Constraint<'intern>,
    ) -> Result<(), TypeError> {
        match constraint {
            Constraint::HasField(field_name, field_type) => {
                self.has_field(ty, field_name, field_type)
            }
            Constraint::SizeOf => self.sized(ty),
            Constraint::ParserIf(target) => self.parser_if(ty, target),
        }
    }

    fn instantiate(&mut self, var: VarId, ty: InfTypeId<'intern>) -> Result<(), TypeError> {
        self.instantiations[var.usize()] = Some(ty);
        let constraints = self.constraints[var.usize()].take();
        constraints.try_iterate(|constraint| self.apply_constraint(ty, constraint))
    }

    pub fn constrain(
        &mut self,
        lower: InfTypeId<'intern>,
        upper: InfTypeId<'intern>,
    ) -> Result<(), TypeError> {
        use InferenceType::*;
        let lower = self.canon(lower);
        let upper = self.canon(upper);
        if self.trace {
            dbeprintln!(
                self.tr.db(),
                "[{}] constrain: {} :< {}",
                &self.tr.name(),
                &lower,
                &upper
            );
        }
        let lower_head = InfTypeHead::from(lower);
        let upper_head = InfTypeHead::from(upper);
        if lower_head == upper_head {
            match lower.try_for_each_child_pair(upper, |l, u, _| self.constrain(l, u)) {
                Ok(()) => return Ok(()),
                Err(None) => {}
                Err(Some((e, _))) => return Err(e),
            }
        }
        match (lower, upper) {
            (InfTypeId(Uniq(_, Unknown)), _) | (_, InfTypeId(Uniq(_, Unknown))) => Ok(()),

            (InfTypeId(Uniq(_, Var(l))), InfTypeId(Uniq(_, Var(u)))) => {
                self.unify.union(*l, *u);
                let lc = self.constraints[l.usize()].take();
                let uc = self.constraints[u.usize()].take();
                let res = self.unify.find(*l);
                self.constraints[res.usize()] = lc.merge(uc);
                Ok(())
            }

            (InfTypeId(Uniq(_, Var(var))), ty) | (ty, InfTypeId(Uniq(_, Var(var)))) => {
                self.instantiate(*var, ty)
            }

            (InfTypeId(Uniq(_, TypeVarRef(var1))), InfTypeId(Uniq(_, TypeVarRef(var2))))
                if var1.0 != var2.0 =>
            {
                self.connections.add_edge(*var1, *var2);
                Ok(())
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
    pub fn fresh_var_id(&mut self) -> VarId {
        let var = self.unify.new_key(());
        self.instantiations.push(None);
        self.constraints.push(BinaryTree::new_empty());
        var
    }
    pub fn var(&mut self) -> InfTypeId<'intern> {
        let var = self.fresh_var_id();
        let inftype = InferenceType::Var(var);
        self.intern_infty(inftype)
    }
    fn var_with_constraints(&mut self, constraint: Constraint<'intern>) -> InfTypeId<'intern> {
        let var = self.fresh_var_id();
        self.constraints[var.0 as usize] = BinaryTree::new_single(constraint);
        let inftype = InferenceType::Var(var);
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
    pub fn range(&mut self) -> InfTypeId<'intern> {
        let int = self.int();
        self.intern_infty(InferenceType::Loop(ArrayKind::Each, int))
    }
    pub fn single(&mut self) -> InfTypeId<'intern> {
        let ty_var = self.var();
        let for_loop = self.intern_infty(InferenceType::Loop(ArrayKind::Each, ty_var));
        self.intern_infty(InferenceType::ParserArg {
            result: ty_var,
            arg: for_loop,
        })
    }
    pub fn byte_array(&mut self) -> InfTypeId<'intern> {
        let int = self.int();
        self.array(ArrayKind::Each, int)
    }
    pub fn regex(&mut self) -> InfTypeId<'intern> {
        let u8_array = self.byte_array();
        self.parser(u8_array, u8_array)
    }
    pub fn array_parser(&mut self) -> InfTypeId<'intern> {
        // the type of an array is (['t] ~> 'r, int) -> ['t] ~> ['r]
        let int = self.int();
        let from = self.var();
        let to = self.var();
        let from_array = self.array(ArrayKind::Each, from);
        let from_for_array = self.array(ArrayKind::Each, from);
        let to_array = self.array(ArrayKind::Each, to);
        let parser_arg = self.parser(to, from_for_array);
        let returned_parser = self.parser(to_array, from_array);
        let args = self.intern_infty_slice(&[parser_arg, int]);
        self.function(returned_parser, args, None)
    }
    pub fn array_fill_parser(&mut self) -> InfTypeId<'intern> {
        // the type of an array fill is (['t] ~> 'r) -> ['t] ~> ['r]
        let from = self.var();
        let to = self.var();
        let from_array = self.array(ArrayKind::Each, from);
        let to_array = self.array(ArrayKind::Each, to);
        let parser_arg = self.parser(to, from_array);
        let returned_parser = self.parser(to_array, from_array);
        let args = self.intern_infty_slice(&[parser_arg]);
        self.function(returned_parser, args, None)
    }
    pub fn type_var(&mut self, id: DefId, index: u32) -> InfTypeId<'intern> {
        let inftype = InferenceType::TypeVarRef(TypeVarRef(id, index));
        self.intern_infty(inftype)
    }
    pub fn check_size_of(
        &mut self,
        ty: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let t = self.var_with_constraints(Constraint::SizeOf);
        self.constrain(ty, t)?;
        let int = self.int();
        Ok(int)
    }
    pub fn parser(
        &mut self,
        result: InfTypeId<'intern>,
        arg: InfTypeId<'intern>,
    ) -> InfTypeId<'intern> {
        self.intern_infty(InferenceType::ParserArg { result, arg })
    }

    pub(crate) fn canon(&mut self, ty: InfTypeId<'intern>) -> InfTypeId<'intern> {
        let InferenceType::Var(id) = ty.value() else {
            return ty;
        };
        let canon_id = self.unify.find(*id);
        match self.instantiations[canon_id.0 as usize] {
            Some(instantiation) => instantiation,
            None if canon_id == *id => ty,
            None => self.intern_infty(InferenceType::Var(canon_id)),
        }
    }

    fn slice_to_function_arg_list(
        &mut self,
        args: impl DoubleEndedIterator<Item = InfTypeId<'intern>>,
        tail_args: Option<InfTypeId<'intern>>,
    ) -> InfTypeId<'intern> {
        let mut res = match tail_args {
            Some(tail) => tail,
            None => self.intern_infty(InferenceType::FunArgNil),
        };
        for arg in args.rev() {
            res = self.intern_infty(InferenceType::FunArgCons(arg, res));
        }
        res
    }
    pub fn function(
        &mut self,
        result: InfTypeId<'intern>,
        args: &[InfTypeId<'intern>],
        tail_args: Option<InfTypeId<'intern>>,
    ) -> InfTypeId<'intern> {
        let args = self.slice_to_function_arg_list(args.iter().copied(), tail_args);
        self.intern_infty(InferenceType::FunctionArgs { result, args })
    }
    pub fn zero_arg_function(&mut self, result: InfTypeId<'intern>) -> InfTypeId<'intern> {
        let args = self.slice_interner.intern_slice(&[]);
        self.function(result, args, None)
    }
    pub fn array(&mut self, kind: ArrayKind, inner: InfTypeId<'intern>) -> InfTypeId<'intern> {
        self.intern_infty(InferenceType::Loop(kind, inner))
    }
    pub fn if_checked(
        &mut self,
        to_be_checked: InfTypeId<'intern>,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let result = self.var();
        let if_result = self.var_with_constraints(Constraint::ParserIf(result));
        self.constrain(to_be_checked, if_result)?;
        Ok(result)
    }
    pub fn check_parser(&mut self, ty: InfTypeId<'intern>) -> Result<(), TypeError> {
        let result = self.var();
        let arg = self.var();
        let parser = self.parser(result, arg);
        self.constrain(ty, parser)
    }
    pub fn block(
        &mut self,
        id: DefId,
        is_parser: bool,
        ty_args: &[InfTypeId<'intern>],
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let arg = is_parser.then(|| self.var());
        let block = BlockInfHead {
            def: id,
            parse_arg: arg,
            ty_args: self.slice_interner.intern_slice(ty_args),
            internal: true,
        };
        let result = self.intern_infty(InferenceType::Block(block));
        if let Some(arg) = arg {
            Ok(self.parser(result, arg))
        } else {
            Ok(self.zero_arg_function(result))
        }
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
        partial: Application,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        match partial {
            Application::Partial => {
                let result = self.var();
                let args_tail = self.var();
                let result_function = self.intern_infty(InferenceType::FunctionArgs {
                    result,
                    args: args_tail,
                });
                let new_function = self.function(result, args, Some(args_tail));
                self.constrain(function, new_function)?;
                Ok(result_function)
            }
            Application::Full => {
                let result = self.var();
                let new_function = self.function(result, args, None);
                self.constrain(function, new_function)?;
                Ok(result)
            }
        }
    }
    pub fn access_field(
        &mut self,
        accessed: InfTypeId<'intern>,
        name: FieldName,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let var = self.var();
        let infer_access = self.var_with_constraints(Constraint::HasField(name, var));
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
            dbeprintln!(
                self.tr.db(),
                "[{}] from_type_with_args: forall [{}] {}",
                &self.tr.name(),
                &args,
                &ty,
            );
        }
        let ty = self.tr.db().lookup_intern_type(ty);
        let vars = TyVars { cur: args };
        self.convert_type_into_inftype_internal(&ty, Some(&vars))
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
            Type::Unknown => InferenceType::Unknown,
            Type::Primitive(p) => InferenceType::Primitive(*p),
            Type::TypeVarRef(TypeVarRef(loc, index)) => {
                match var_stack.and_then(|x| x.resolve(*index)) {
                    Some(x) => return x,
                    None => InferenceType::TypeVarRef(TypeVarRef(*loc, *index)),
                }
            }
            Type::Block(BlockTypeHead {
                def,
                parse_arg,
                ty_args,
            }) => {
                let parse_arg = parse_arg.map(&mut recurse);
                let ty_args = ty_args
                    .iter()
                    .copied()
                    .map(&mut recurse)
                    .collect::<Vec<_>>();
                let ty_args = self.slice_interner.intern_slice(&ty_args);
                InferenceType::Block(BlockInfHead {
                    def: *def,
                    parse_arg,
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
                let args = self.slice_to_function_arg_list(args_vec.iter().copied(), None);
                InferenceType::FunctionArgs { result, args }
            }
        };
        self.intern_infty(ret)
    }
    pub fn type_converter(
        &mut self,
        at: DefId,
    ) -> Result<TypeConvertMemo<'_, 'intern, TR>, TypeConvError> {
        let map = self.connections.connections_map()?;
        Ok(TypeConvertMemo::new(self, at, map))
    }
}
impl<'intern, TR: TypeResolver<'intern>> InfTypeInterner<'intern>
    for InferenceContext<'intern, TR>
{
    fn intern_infty(&mut self, infty: InferenceType<InfTypeId<'intern>>) -> InfTypeId<'intern> {
        self.intern_infty(infty)
    }

    fn intern_infty_slice(&mut self, slice: &[InfTypeId<'intern>]) -> InfSlice<'intern> {
        self.slice_interner.intern_slice(slice)
    }
}
