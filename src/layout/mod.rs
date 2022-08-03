pub mod collect;
pub mod mir_subst;
pub mod represent;
pub mod prop;
pub mod vtable;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use fxhash::{FxHashMap, FxHashSet};

use crate::absint::{AbsInt, AbsIntCtx, AbstractDomain, Arg};
use crate::error::{IsSilenced, SResult, SilencedError};
use crate::expr::{self, ExpressionHead, ValBinOp, ValUnOp};
use crate::hir::variable_set::VarStatus;
use crate::hir::{self, HirIdWrapper};
use crate::hir_types::NominalId;
use crate::interner::{DefId, FieldName};
use crate::low_effort_interner::{Interner, Uniq};
use crate::mir::DupleField;
use crate::order::expr::ResolvedAtom;
use crate::order::ResolvedExpr;
use crate::types::{PrimitiveType, Type, TypeId};

use self::represent::{LayoutHasher, LayoutPart, LayoutSymbol};
use self::prop::{PSize, SizeAlign, TargetSized, Zst};

#[derive(Clone, PartialEq, Eq, Default)]
pub struct StructManifestation {
    field_offsets: FxHashMap<DefId, PSize>,
    discriminant_mapping: FxHashMap<DefId, PSize>,
    discriminant_offset: PSize,
    size: SizeAlign,
}

struct UnfinishedManifestation(StructManifestation);

impl UnfinishedManifestation {
    pub fn new() -> Self {
        UnfinishedManifestation(Default::default())
    }
    pub fn add_field(&mut self, id: DefId, field_size: SizeAlign, discriminated: bool) {
        self.0.size = self.0.size.cat(field_size);
        let offset = self.0.size.size - field_size.size;
        self.0.field_offsets.insert(id, offset);
        if discriminated {
            self.0
                .discriminant_mapping
                .insert(id, self.0.discriminant_mapping.len() as PSize);
        }
    }
    pub fn finalize(self) -> StructManifestation {
        let mut manifest = self.0;
        manifest.discriminant_offset = manifest.size.size;
        manifest.size.cat(SizeAlign {
            size: ((manifest.discriminant_mapping.len() + 7) / 8) as PSize,
            align_mask: 0,
        });
        manifest
    }
}

#[salsa::query_group(LayoutDatabase)]
pub trait Layouts: AbsInt {}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Layout<Inner> {
    None,
    Mono(MonoLayout<Inner>, TypeId),
    Multi(MultiLayout<Inner>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum MonoLayout<Inner> {
    Primitive(PrimitiveType),
    Pointer,
    Single,
    Nominal(hir::ParserDefId, Option<Inner>),
    NominalParser(hir::ParserDefId),
    Block(hir::BlockId, BTreeMap<FieldName, Inner>),
    BlockParser(hir::BlockId, BTreeMap<DefId, Inner>),
    ComposedParser(Inner, Inner),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct MultiLayout<Inner> {
    pub layouts: Vec<Inner>,
}

pub type ILayout<'a> = &'a Uniq<InternerLayout<'a>>;

#[derive(Clone, Hash, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct InternerLayout<'a> {
    pub layout: Layout<&'a Uniq<Self>>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(transparent)]
pub struct IMonoLayout<'a>(&'a Uniq<InternerLayout<'a>>);

impl<'a> IMonoLayout<'a> {
    pub fn mono_layout(self) -> (&'a MonoLayout<ILayout<'a>>, TypeId) {
        match self.0.layout {
            Layout::Mono(ref mono, ty) => (mono, ty),
            _ => unreachable!("Expected mono layout"),
        }
    }
    pub fn inner(self) -> ILayout<'a> {
        self.0
    }
    pub fn symbol<DB: Layouts + ?Sized>(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        part: LayoutPart,
        db: &DB,
    ) -> String {
        let sym = LayoutSymbol { layout: self, part };
        sym.symbol(&mut ctx.dcx.hashes, db)
    }
}

fn flat_layouts<'a, 'l>(
    layout: &'l ILayout<'a>,
) -> impl ExactSizeIterator<Item = IMonoLayout<'a>> + Clone + 'l {
    match &layout.layout {
        Layout::Mono(_, _) => std::slice::from_ref(layout),
        Layout::Multi(l) => l.layouts.as_slice(),
        Layout::None => &[],
    }
    .iter()
    .map(|l| match l.layout {
        Layout::Mono(_, _) => IMonoLayout(l),
        Layout::Multi(_) => panic!("MultiLayout inside MultiLayout not supported"),
        Layout::None => panic!("Empty layout inside MultiLayout not supported"),
    })
}

impl<'a> Uniq<InternerLayout<'a>> {
    fn map(
        &'a self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        mut f: impl FnMut(IMonoLayout<'a>, &mut AbsIntCtx<'a, ILayout<'a>>) -> ILayout<'a>,
    ) -> ILayout<'a> {
        let mut acc = BTreeSet::new();
        let layouts = flat_layouts(&self);
        for layout in layouts {
            let result_layout = f(layout, ctx);
            match &result_layout.layout {
                Layout::Mono(_, _) => {
                    acc.insert(result_layout);
                }
                Layout::Multi(l) => {
                    acc.extend(l.layouts.iter().copied());
                }
                Layout::None => {}
            }
        }
        let acc_vec = acc.into_iter().collect::<Vec<_>>();
        match &acc_vec[..] {
            [single] => *single,
            [] => ctx.dcx.intern.intern(InternerLayout {
                layout: Layout::None,
            }),
            _ => ctx.dcx.intern.intern(InternerLayout {
                layout: Layout::Multi(MultiLayout { layouts: acc_vec }),
            }),
        }
    }

    fn try_map(
        &'a self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        mut f: impl FnMut(
            IMonoLayout<'a>,
            &mut AbsIntCtx<'a, ILayout<'a>>,
        ) -> Result<ILayout<'a>, LayoutError>,
    ) -> Result<ILayout<'a>, LayoutError> {
        let mut acc = BTreeSet::new();
        let layouts = flat_layouts(&self);
        for layout in layouts {
            let result_layout = f(layout, ctx)?;
            match &result_layout.layout {
                Layout::Mono(_, _) => {
                    acc.insert(result_layout);
                }
                Layout::Multi(l) => {
                    acc.extend(l.layouts.iter().copied());
                }
                Layout::None => {}
            }
        }
        let acc_vec = acc.into_iter().collect::<Vec<_>>();
        Ok(match &acc_vec[..] {
            [single] => *single,
            [] => ctx.dcx.intern.intern(InternerLayout {
                layout: Layout::None,
            }),
            _ => ctx.dcx.intern.intern(InternerLayout {
                layout: Layout::Multi(MultiLayout { layouts: acc_vec }),
            }),
        })
    }

    fn typecast_impl(
        &'a self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        target: Option<hir::ParserDefId>,
    ) -> Result<(ILayout<'a>, bool), LayoutError> {
        let mut changed = false;
        let res = self.try_map(ctx, |layout, ctx| {
            Ok(match layout.mono_layout().0 {
                MonoLayout::Nominal(pd, _) if Some(*pd) == target => layout.0,
                MonoLayout::Nominal(_, _) => {
                    changed = true;
                    let res_ty = layout.mono_layout().1;
                    ctx.eval_pd(layout.inner(), res_ty)
                        .ok_or(SilencedError)?
                        .typecast_impl(ctx, target)?
                        .0
                }
                _ => layout.0,
            })
        });
        res.map(|x| (x, changed))
    }

    fn get_captured(&'a self, id: DefId) -> Option<ILayout<'a>> {
        match match &self.layout {
            Layout::Mono(m, _) => m,
            Layout::Multi(_) => {
                panic!("Attempting to get captured variable inside multi layout block")
            }
            Layout::None => {
                panic!("Attempting to get captured variable inside empty layout")
            }
        } {
            MonoLayout::BlockParser(_, captures) => captures,
            _ => panic!("Attempting to get captured variable outside block"),
        }
        .get(&id)
        .copied()
    }

    fn array_primitive(&'a self, ctx: &mut AbsIntCtx<'a, ILayout<'a>>) -> ILayout<'a> {
        self.map(ctx, |layout, ctx| match layout.mono_layout().0 {
            MonoLayout::Pointer => {
                let int = PrimitiveType::Int;
                let int_ty = ctx.db.intern_type(Type::Primitive(int));
                ctx.dcx.intern.intern(InternerLayout {
                    layout: Layout::Mono(MonoLayout::Primitive(int), int_ty),
                })
            }
            _ => panic!("Attempting to get an primitive element from non-array"),
        })
    }

    fn apply_arg(
        &'a self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        from: ILayout<'a>,
    ) -> Result<ILayout<'a>, LayoutError> {
        self.try_map(ctx, |layout, ctx| {
            let (result_type, arg_type) = match ctx.db.lookup_intern_type(layout.mono_layout().1) {
                Type::ParserArg { result, arg } => (result, arg),
                _ => panic!("Attempting to apply argument to non-parser type"),
            };
            let from = from.typecast(ctx, arg_type)?.0;
            match layout.mono_layout().0 {
                MonoLayout::NominalParser(pd) => ctx.apply_thunk_arg(*pd, result_type, from),
                MonoLayout::BlockParser(block_id, _) => ctx
                    .eval_block(*block_id, self, from, result_type)
                    .ok_or(SilencedError.into()),
                MonoLayout::ComposedParser(first, second) => {
                    let first_result = first.apply_arg(ctx, from)?;
                    second.apply_arg(ctx, first_result)
                }
                MonoLayout::Single => Ok(from.array_primitive(ctx)),
                _ => panic!("Attempting to apply argument to non-parser layout"),
            }
        })
    }

    fn access_field(
        &'a self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        field: FieldName,
    ) -> ILayout<'a> {
        self.map(ctx, |layout, _| match layout.mono_layout().0 {
            MonoLayout::Block(_, fields) => fields[&field],
            _ => panic!("Field access on non-block"),
        })
    }

    fn access_from(&'a self, ctx: &mut AbsIntCtx<'a, ILayout<'a>>) -> ILayout<'a> {
        self.map(ctx, |layout, _| match layout.mono_layout().0 {
            MonoLayout::Nominal(_, from) => {
                from.expect("Attempting to get 'from' field from non-from field containing nominal")
            }
            _ => panic!("Attempting to get 'from' field from non-nominal"),
        })
    }

    fn access_duple(
        &'a self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        duple_field: DupleField,
    ) -> ILayout<'a> {
        self.map(ctx, |layout, _| match layout.mono_layout().0 {
            MonoLayout::ComposedParser(first, second) => match duple_field {
                DupleField::First => first,
                DupleField::Second => second,
            },
            _ => panic!("Attempting to get 'from' field from non-nominal"),
        })
    }

    pub fn size_align(&'a self, ctx: &mut AbsIntCtx<'a, ILayout<'a>>) -> SResult<SizeAlign> {
        if let Some(sa) = ctx.dcx.sizes.get(self) {
            return Ok(*sa);
        }
        let ret = match &self.layout {
            Layout::None => Zst::tsize(),
            Layout::Mono(MonoLayout::Block(id, fields), _) => {
                self.block_manifestation(ctx, *id, fields)?.size
            }
            Layout::Mono(MonoLayout::BlockParser(_, captures), _) => {
                self.block_parser_manifestation(ctx, captures)?.size
            }
            Layout::Mono(MonoLayout::ComposedParser(fst, snd), _) => {
                fst.size_align(ctx)?.cat(snd.size_align(ctx)?)
            }
            Layout::Mono(MonoLayout::Nominal(_, from), _) => from
                .map(|layout| layout.size_align(ctx))
                .transpose()?
                .unwrap_or(Zst::tsize()),
            Layout::Mono(MonoLayout::NominalParser(_), _) => Zst::tsize(),
            Layout::Mono(MonoLayout::Pointer, _) => <&Zst>::tsize(),
            Layout::Mono(MonoLayout::Primitive(PrimitiveType::Bit), _) => bool::tsize(),
            Layout::Mono(MonoLayout::Primitive(PrimitiveType::Char), _) => char::tsize(),
            Layout::Mono(MonoLayout::Primitive(PrimitiveType::Int), _) => i64::tsize(),
            Layout::Mono(MonoLayout::Single, _) => Zst::tsize(),
            Layout::Multi(m) => {
                let inner = m.layouts.iter().try_fold(Zst::tsize(), |sa, layout| {
                    Ok(sa.union(layout.size_align(ctx)?))
                })?;
                <&Zst>::tsize().cat(inner)
            }
        };
        ctx.dcx.sizes.insert(self, ret);
        Ok(ret)
    }

    pub fn block_manifestation(
        &'a self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        id: hir::BlockId,
        fields: &BTreeMap<FieldName, ILayout<'a>>,
    ) -> SResult<Arc<StructManifestation>> {
        if let Some(man) = ctx.dcx.manifestations.get(self) {
            return Ok(man.clone());
        }
        let mut manifest = UnfinishedManifestation::new();
        let block = id.lookup(ctx.db)?;
        let root_ctx = block.root_context.lookup(ctx.db)?;
        for (field, layout) in fields {
            let (needs_discriminant, field_id) = root_ctx
                .vars
                .get(*field)
                .map(|x| (matches!(x, VarStatus::Maybe(_)), *x.inner()))
                .expect("Could not find field during layout calculation");
            let field_size = layout.size_align(ctx)?;
            manifest.add_field(field_id, field_size, needs_discriminant);
        }
        let manifest = Arc::new(manifest.finalize());
        ctx.dcx.manifestations.insert(self, manifest.clone());
        Ok(manifest)
    }

    pub fn block_parser_manifestation(
        &'a self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        captures: &BTreeMap<DefId, ILayout<'a>>,
    ) -> SResult<Arc<StructManifestation>> {
        if let Some(man) = ctx.dcx.manifestations.get(self) {
            return Ok(man.clone());
        }
        let mut manifest = UnfinishedManifestation::new();
        for (capture, layout) in captures.iter() {
            let sa = layout.size_align(ctx)?;
            manifest.add_field(*capture, sa, false);
        }
        let manifest = Arc::new(manifest.finalize());
        ctx.dcx.manifestations.insert(self, manifest.clone());
        Ok(manifest)
    }
}

pub fn canon_layout<'a, 'b>(
    ctx: &'b mut AbsIntCtx<'a, ILayout<'a>>,
    ty: TypeId,
) -> Result<ILayout<'a>, LayoutError> {
    let typ = ctx.db.lookup_intern_type(ty);
    let make_layout = |ctx: &'b mut AbsIntCtx<'a, ILayout<'a>>, x| {
        ctx.dcx.intern.intern(InternerLayout {
            layout: Layout::Mono(x, ty),
        })
    };
    match typ {
        Type::Primitive(x) => Ok(make_layout(ctx, MonoLayout::Primitive(x))),
        Type::Loop(_, inner_ty) => {
            let inner_type = ctx.db.lookup_intern_type(inner_ty);
            match inner_type {
                Type::Primitive(PrimitiveType::Int) => {
                    return Ok(make_layout(ctx, MonoLayout::Pointer))
                }
                _ => return Err(LayoutError),
            }
        }
        Type::Nominal(n) => {
            let def_id = match NominalId::from_nominal_head(&n) {
                NominalId::Def(d) => d,
                NominalId::Block(_) => return Err(LayoutError),
            };
            if !n.fun_args.is_empty() {
                // TODO(8051): i am not yet sure how to handle fun args that are in addition to the parse arg
                // and function args are not implemented anyway so return an error for now
                return Err(LayoutError);
            }
            let from = n.parse_arg.map(|arg| canon_layout(ctx, arg)).transpose()?;
            Ok(make_layout(ctx, MonoLayout::Nominal(def_id, from)))
        }
        Type::ParserArg { .. } | Type::FunctionArg(_, _) => Err(LayoutError),
        Type::TypeVarRef(_, _, _) | Type::Any | Type::Bot | Type::Unknown | Type::ForAll(_, _) => {
            Err(LayoutError)
        }
    }
}

pub fn instantiate<'a>(
    ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    types: &[TypeId],
) -> Result<(), LayoutError> {
    let mut pd_eval_worklist = FxHashSet::default();
    for &ty in types {
        let root_layout = canon_layout(ctx, ty)?;
        pd_eval_worklist.insert((ty, root_layout));
    }
    while !pd_eval_worklist.is_empty() {
        for (ty, layout) in pd_eval_worklist.drain() {
            ctx.eval_pd(layout, ty);
        }
        pd_eval_worklist = ctx.new_pds();
    }
    Ok(())
}

#[derive(Debug)]
pub struct LayoutError;

impl From<crate::error::SilencedError> for LayoutError {
    fn from(_: crate::error::SilencedError) -> Self {
        LayoutError
    }
}

impl IsSilenced for LayoutError {
    fn is_silenced(&self) -> bool {
        true
    }
}

pub type AbsLayoutCtx<'a> = AbsIntCtx<'a, ILayout<'a>>;

pub struct LayoutContext<'a> {
    intern: Interner<'a, InternerLayout<'a>>,
    sizes: FxHashMap<ILayout<'a>, SizeAlign>,
    manifestations: FxHashMap<ILayout<'a>, Arc<StructManifestation>>,
    hashes: LayoutHasher<'a>,
}

impl<'a> LayoutContext<'a> {
    pub fn new(intern: Interner<'a, InternerLayout<'a>>) -> Self {
        Self {
            intern,
            sizes: Default::default(),
            manifestations: Default::default(),
            hashes: LayoutHasher::new(),
        }
    }
}

impl<'a> AbstractDomain<'a> for ILayout<'a> {
    type Err = LayoutError;
    type DomainContext = LayoutContext<'a>;

    fn make_block(
        ctx: &mut AbsIntCtx<'a, Self>,
        id: hir::BlockId,
        ty: TypeId,
        fields: &FxHashMap<FieldName, Self>,
    ) -> Result<Self, Self::Err> {
        let new_layout = InternerLayout {
            layout: Layout::Mono(
                MonoLayout::Block(id, fields.iter().map(|(a, b)| (*a, *b)).collect()),
                ty,
            ),
        };
        Ok(ctx.dcx.intern.intern(new_layout))
    }

    fn join(self, ctx: &mut AbsIntCtx<'a, Self>, other: Self) -> Result<(Self, bool), Self::Err> {
        if self == other {
            return Ok((self, false));
        }
        let layouts = match (&self.layout, &other.layout) {
            (Layout::None, _) => return Ok((other, true)),
            (_, Layout::None) => return Ok((self, false)),
            (Layout::Mono(_, _), Layout::Mono(_, _)) => vec![self, other],
            (Layout::Multi(first), Layout::Mono(_, _)) => match first.layouts.binary_search(&other)
            {
                Ok(_) => return Ok((self, false)),
                Err(i) => {
                    let mut new_layouts = first.layouts.clone();
                    new_layouts.insert(i, other);
                    new_layouts
                }
            },
            (Layout::Mono(_, _), Layout::Multi(second)) => {
                match second.layouts.binary_search(&self) {
                    Ok(_) => return Ok((other, true)),
                    Err(i) => {
                        let mut new_layouts = second.layouts.clone();
                        new_layouts.insert(i, self);
                        new_layouts
                    }
                }
            }
            (Layout::Multi(first), Layout::Multi(second)) => {
                let mut first_index = 0;
                let mut second_index = 0;
                let mut new_layouts = Vec::new();
                loop {
                    if first_index == first.layouts.len() {
                        new_layouts.extend(second.layouts[second_index..].iter().cloned());
                        break;
                    }
                    if second_index == second.layouts.len() {
                        new_layouts.extend(first.layouts[first_index..].iter().cloned());
                        break;
                    }
                    match first.layouts[first_index].cmp(&second.layouts[second_index]) {
                        Ordering::Less => {
                            new_layouts.push(first.layouts[first_index]);
                            first_index += 1;
                        }
                        Ordering::Greater => {
                            new_layouts.push(second.layouts[second_index]);
                            second_index += 1;
                        }
                        Ordering::Equal => {
                            new_layouts.push(first.layouts[first_index]);
                            first_index += 1;
                            second_index += 1;
                        }
                    }
                }
                new_layouts
            }
        };
        let new_layout = InternerLayout {
            layout: Layout::Multi(MultiLayout { layouts }),
        };
        Ok((ctx.dcx.intern.intern(new_layout), true))
    }

    fn widen(self, ctx: &mut AbsIntCtx<'a, Self>, other: Self) -> Result<(Self, bool), Self::Err> {
        self.join(ctx, other)
    }

    fn eval_expr(
        ctx: &mut AbsIntCtx<'a, Self>,
        expr: ExpressionHead<expr::KindWithData<ResolvedExpr, TypeId>, Self>,
    ) -> Result<Self, Self::Err> {
        let ret_type = *expr.root_data();
        let mut make_layout = |x| {
            ctx.dcx.intern.intern(InternerLayout {
                layout: Layout::Mono(x, ret_type),
            })
        };
        Ok(match expr {
            ExpressionHead::Niladic(n) => match n.inner {
                ResolvedAtom::Char(_) => make_layout(MonoLayout::Primitive(PrimitiveType::Char)),
                ResolvedAtom::Number(_) => make_layout(MonoLayout::Primitive(PrimitiveType::Int)),
                ResolvedAtom::Val(id) => ctx.var_by_id(id),
                ResolvedAtom::Single => make_layout(MonoLayout::Single),
                ResolvedAtom::ParserDef(pd) => make_layout(MonoLayout::NominalParser(pd)),
                ResolvedAtom::Block(block_id) => {
                    let mut captures = BTreeMap::new();
                    for capture in ctx.db.captures(block_id).iter() {
                        let capture_value = ctx
                            .active_block()
                            .and_then(|s| s.get_captured(*capture))
                            .unwrap_or_else(|| ctx.var_by_id(*capture));
                        captures.insert(*capture, capture_value);
                    }
                    ctx.dcx.intern.intern(InternerLayout {
                        layout: Layout::Mono(MonoLayout::BlockParser(block_id, captures), ret_type),
                    })
                }
                ResolvedAtom::Captured(capture) => {
                    let capture_value = ctx
                        .active_block()
                        .and_then(|s| s.get_captured(capture))
                        .expect("Captured variable outside block");
                    capture_value
                }
            },
            ExpressionHead::Monadic(m) => match m.op.inner {
                ValUnOp::Not | ValUnOp::Neg | ValUnOp::Pos => {
                    make_layout(MonoLayout::Primitive(PrimitiveType::Int))
                }
                ValUnOp::Wiggle(_, _) => m.inner,
                ValUnOp::Dot(a) => m.inner.access_field(ctx, a),
            },
            ExpressionHead::Dyadic(d) => match d.op.inner {
                ValBinOp::ParserApply => d.inner[1].apply_arg(ctx, d.inner[0])?,
                ValBinOp::Compose => {
                    make_layout(MonoLayout::ComposedParser(d.inner[0], d.inner[1]))
                }
                ValBinOp::Else => d.inner[0].join(ctx, d.inner[1])?.0,
                ValBinOp::LesserEq
                | ValBinOp::Lesser
                | ValBinOp::GreaterEq
                | ValBinOp::Greater
                | ValBinOp::Uneq
                | ValBinOp::Equals => make_layout(MonoLayout::Primitive(PrimitiveType::Bit)),
                ValBinOp::And
                | ValBinOp::Xor
                | ValBinOp::Or
                | ValBinOp::ShiftR
                | ValBinOp::ShiftL
                | ValBinOp::Minus
                | ValBinOp::Plus
                | ValBinOp::Div
                | ValBinOp::Modulo
                | ValBinOp::Mul => make_layout(MonoLayout::Primitive(PrimitiveType::Int)),
            },
        })
    }

    fn typecast(
        self,
        ctx: &mut AbsIntCtx<'a, Self>,
        ty: TypeId,
    ) -> Result<(Self, bool), Self::Err> {
        let non_derefed_pd_id = match ctx.db.lookup_intern_type(ty) {
            Type::Nominal(nom) => match NominalId::from_nominal_head(&nom) {
                NominalId::Def(id) => Some(id),
                NominalId::Block(_) => None,
            },
            _ => None,
        };
        self.typecast_impl(ctx, non_derefed_pd_id)
    }

    fn get_arg(
        self,
        ctx: &mut AbsIntCtx<'a, Self>,
        arg: crate::absint::Arg,
    ) -> Result<Self, Self::Err> {
        Ok(self.map(ctx, |l, _| match l.mono_layout().0 {
            MonoLayout::Nominal(_, from) => match arg {
                Arg::Named(_) => unimplemented!("args not yet implemented"),
                Arg::From => from.expect("unexpectedly did not have from arg"),
            },
            _ => panic!("get_arg called on non-nominal layout"),
        }))
    }

    fn make_thunk(
        ctx: &mut AbsIntCtx<'a, Self>,
        id: hir::ParserDefId,
        ty: TypeId,
        fields: &FxHashMap<Arg, Self>,
    ) -> Result<Self, Self::Err> {
        let from = fields.get(&Arg::From).map(|x| *x);
        if fields.len() > from.iter().len() {
            panic!("unknown args in {:?}", fields)
        }
        let new_layout = InternerLayout {
            layout: Layout::Mono(MonoLayout::Nominal(id, from), ty),
        };
        Ok(ctx.dcx.intern.intern(new_layout))
    }

    fn bottom(ctx: &mut Self::DomainContext) -> Self {
        ctx.intern.intern(InternerLayout {
            layout: Layout::None,
        })
    }
}

#[cfg(test)]
mod tests {
    use bumpalo::Bump;

    use crate::{context::Context, dbformat, hir_types::TyHirs};

    use super::*;

    #[test]
    fn layouts() {
        let ctx = Context::mock(
            r"
def for['a] *> first = ~
def for['b] *> second = ~
def for[int] *> main = {
    a: ~,
    b: ~,
    c: {
        let c: for[int] *> first = first,
        ;
        let c: for[int] *> second = second,
    },
    d: c.c,
}
        ",
        );
        let mut bump = Bump::new();
        let intern = Interner::<InternerLayout>::new(&mut bump);
        let layout_ctx = LayoutContext::new(intern);
        let mut outlayer = AbsIntCtx::<ILayout>::new(&ctx.db, layout_ctx);
        let main = ctx.parser("main");
        let main_ty = ctx.db.parser_args(main).unwrap().thunk;
        instantiate(&mut outlayer, &[main_ty]).unwrap();
        let canon_2006 = canon_layout(&mut outlayer, main_ty).unwrap();
        for lay in flat_layouts(&canon_2006) {
            assert_eq!(
                lay.symbol(&mut outlayer, LayoutPart::LenImpl(0), &ctx.db),
                "main$e16412415cc84158$len_0"
            );
        }
        let main_block = outlayer.pd_result()[canon_2006].as_ref().unwrap().returned;
        for lay in flat_layouts(&main_block) {
            assert_eq!(
                lay.symbol(&mut outlayer, LayoutPart::LenImpl(0), &ctx.db),
                "block_1b15571abd710f7a$000fdd43004bec7e$len_0"
            );
        }
        let field = |name| FieldName::Ident(ctx.id(name));
        assert_eq!(
            dbformat!(
                &ctx.db,
                "{}",
                &main_block.access_field(&mut outlayer, field("a"))
            ),
            "int"
        );
        assert_eq!(
            dbformat!(
                &ctx.db,
                "{}",
                &main_block.access_field(&mut outlayer, field("b"))
            ),
            "int"
        );
        assert_eq!(
            dbformat!(&ctx.db, "{}", &main_block.access_field(&mut outlayer, field("c")).access_field(&mut outlayer, field("c"))),
            "nominal-parser[for[int] *> for[int] &> file[_].second] | nominal-parser[for[int] *> for[int] &> file[_].first]"
        );
        assert_eq!(
            dbformat!(
                &ctx.db,
                "{}",
                &main_block.access_field(&mut outlayer, field("d"))
            ),
            "int"
        );
    }
}
