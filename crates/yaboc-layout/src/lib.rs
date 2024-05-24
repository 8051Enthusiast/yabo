#![allow(clippy::type_complexity)]
pub mod collect;
pub mod mir_subst;
pub mod represent;
pub mod vtable;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use bumpalo::Bump;
use fxhash::FxHashMap;

use hir::HirConstraintId;
use yaboc_absint::{AbsInt, AbsIntCtx, AbstractDomain, Arg};
use yaboc_ast::expr::{BtMarkKind, WiggleKind};
use yaboc_ast::ArrayKind;
use yaboc_base::dbpanic;
use yaboc_base::error::{IsSilenced, SResult, SilencedError};
use yaboc_base::interner::{DefId, FieldName, Regex};
use yaboc_base::low_effort_interner::{Interner, Uniq};
use yaboc_expr::ExprHead;
use yaboc_hir::{self as hir, HirIdWrapper};
use yaboc_hir_types::{ty_head_discriminant, DerefLevel, HeadDiscriminant, NominalId};
use yaboc_mir::Mirs;
use yaboc_resolve::expr::{Resolved, ResolvedAtom, ValBinOp, ValUnOp, ValVarOp};
use yaboc_target::layout::{PSize, SizeAlign, TargetLayoutData, TargetSized, Zst};
use yaboc_types::{PrimitiveType, Type, TypeId, TypeInterner};

pub use self::collect::TailInfo;
use self::represent::{LayoutHasher, LayoutPart, LayoutSymbol};

#[derive(Clone, PartialEq, Eq, Default, Debug)]
pub struct StructManifestation {
    pub field_offsets: FxHashMap<DefId, PSize>,
    pub discriminant_mapping: Arc<FxHashMap<DefId, PSize>>,
    pub discriminant_offset: PSize,
    pub size: SizeAlign,
    pub padding_mask: Vec<u8>,
}

#[salsa::query_group(LayoutDatabase)]
pub trait Layouts: AbsInt + Mirs {}

struct UnfinishedManifestation(StructManifestation);

impl UnfinishedManifestation {
    pub fn new() -> Self {
        UnfinishedManifestation(Default::default())
    }
    pub fn add_field(&mut self, id: DefId, field_size: SizeAlign) {
        self.0.size = self.0.size.cat(field_size);
        let offset = self.0.size.after - field_size.after;
        self.0.field_offsets.insert(id, offset);
        self.0.padding_mask.resize(self.0.size.after as usize, 0);
        // note that this is only the padding we need to care about,
        // the padding in the individual fields is taken care of by calls
        // to the mask funs of those fields
        for i in (offset - field_size.before)..(offset + field_size.after) {
            self.0.padding_mask[i as usize] = 0xff;
        }
    }
    pub fn finalize(
        self,
        discriminant_mapping: Arc<FxHashMap<DefId, PSize>>,
    ) -> StructManifestation {
        let mut manifest = self.0;
        manifest.discriminant_mapping = discriminant_mapping;
        manifest.discriminant_offset = 0;
        let disc_sa = SizeAlign {
            before: 0,
            after: ((manifest.discriminant_mapping.len() + 7) / 8) as PSize,
            align_mask: 0,
        };
        let size = manifest.size.after;
        manifest.size = disc_sa.cat(manifest.size);
        let added_offset = manifest.size.after - size;
        for offset in manifest.field_offsets.values_mut() {
            *offset += added_offset;
        }
        let mut disc_padding = vec![0; added_offset as usize];
        for i in 0..disc_sa.after {
            disc_padding[i as usize] = 0xff;
        }
        if manifest.discriminant_mapping.len() % 8 != 0 {
            let last_byte = manifest.discriminant_mapping.len() / 8;
            let last_bit = manifest.discriminant_mapping.len() % 8;
            disc_padding[last_byte] = !(0xff << last_bit);
        }
        disc_padding.append(&mut manifest.padding_mask);
        manifest.padding_mask = disc_padding;
        manifest
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Layout<Inner> {
    None,
    Mono(MonoLayout<Inner>, TypeId),
    Multi(MultiLayout<Inner>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum MonoLayout<Inner> {
    Primitive(PrimitiveType),
    SlicePtr,
    Range,
    Single,
    Nil,
    Regex(Regex, bool),
    IfParser(Inner, HirConstraintId, WiggleKind),
    ArrayParser(Option<(Inner, Option<Inner>)>),
    ArrayFillParser(Option<Inner>),
    Nominal(
        hir::ParserDefId,
        Option<(Inner, TypeId)>,
        Vec<(Inner, TypeId)>,
    ),
    NominalParser(hir::ParserDefId, Vec<(Inner, TypeId)>, bool),
    Block(hir::BlockId, BTreeMap<FieldName, Inner>),
    BlockParser(hir::BlockId, BTreeMap<DefId, Inner>, Arc<Vec<TypeId>>, bool),
    Array {
        parser: Inner,
        slice: Inner,
    },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct MultiLayout<Inner> {
    pub layouts: Vec<Inner>,
}

pub type InternedLayout<'a> = Layout<ILayout<'a>>;

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct ILayout<'a> {
    pub layout: &'a Uniq<Layout<Self>>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(transparent)]
pub struct IMonoLayout<'a>(ILayout<'a>);

impl<'a> IMonoLayout<'a> {
    pub fn mono_layout(self) -> (&'a MonoLayout<ILayout<'a>>, TypeId) {
        match self.0.layout.1 {
            Layout::Mono(ref mono, ty) => (mono, ty),
            _ => unreachable!("Expected mono layout"),
        }
    }

    #[inline(always)]
    pub fn inner(self) -> ILayout<'a> {
        self.0
    }

    pub fn u8_single(ctx: &mut AbsIntCtx<'a, ILayout<'a>>) -> IMonoLayout<'a> {
        let u8 = ctx.db.intern_type(Type::Primitive(PrimitiveType::U8));
        let for_u8 = ctx.db.intern_type(Type::Loop(ArrayKind::Each, u8));
        let parser_ty = ctx.db.intern_type(Type::ParserArg {
            result: u8,
            arg: for_u8,
        });
        let single = ctx.dcx.intern(Layout::Mono(MonoLayout::Single, parser_ty));
        single.maybe_mono().unwrap()
    }

    pub fn u8_array(ctx: &mut AbsIntCtx<'a, ILayout<'a>>) -> IMonoLayout<'a> {
        let u8 = ctx.db.intern_type(Type::Primitive(PrimitiveType::U8));
        let for_u8 = ctx.db.intern_type(Type::Loop(ArrayKind::Each, u8));
        let array = ctx.dcx.intern(Layout::Mono(MonoLayout::SlicePtr, for_u8));
        array.maybe_mono().unwrap()
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
    pub fn deref(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> Result<Option<ILayout<'a>>, SilencedError> {
        Ok(match self.mono_layout().0 {
            MonoLayout::Nominal(_, _, _) => {
                let res_ty = self.mono_layout().1;
                Some(
                    ctx.eval_pd(self.inner(), res_ty)
                        .ok_or_else(SilencedError::new)?,
                )
            }
            MonoLayout::Primitive(PrimitiveType::U8) => Some(ctx.dcx.int(ctx.db)),
            _ => None,
        })
    }

    pub fn arg_num<DB: Layouts + ?Sized>(&self, db: &DB) -> SResult<Option<(usize, usize)>> {
        match self.mono_layout().0 {
            MonoLayout::NominalParser(pd, args, _) => Ok(db.argnum(*pd)?.map(|n| (n, args.len()))),
            MonoLayout::ArrayParser(Some((_, Some(_)))) => Ok(Some((2, 2))),
            MonoLayout::ArrayParser(Some((_, None))) => Ok(Some((2, 1))),
            MonoLayout::ArrayParser(None) => Ok(Some((2, 0))),
            MonoLayout::ArrayFillParser(Some(_)) => Ok(Some((1, 1))),
            MonoLayout::ArrayFillParser(None) => Ok(Some((1, 0))),
            _ => Ok(None),
        }
    }

    pub fn head_disc<DB: Layouts + ?Sized>(&self, db: &DB) -> i64 {
        let (mono, ty) = self.mono_layout();
        match mono {
            MonoLayout::SlicePtr => HeadDiscriminant::SlicePtr as i64,
            MonoLayout::Range => HeadDiscriminant::Range as i64,
            _ => ty_head_discriminant(db, ty),
        }
    }

    pub fn set_backtracking(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        bt: bool,
    ) -> IMonoLayout<'a> {
        match self.mono_layout().0 {
            MonoLayout::NominalParser(pd, f, status) if *status != bt => {
                IMonoLayout(ctx.dcx.intern(Layout::Mono(
                    MonoLayout::NominalParser(*pd, f.clone(), bt),
                    self.mono_layout().1,
                )))
            }
            MonoLayout::BlockParser(bd, cap, typeargs, status) if *status != bt => {
                IMonoLayout(ctx.dcx.intern(Layout::Mono(
                    MonoLayout::BlockParser(*bd, cap.clone(), typeargs.clone(), bt),
                    self.mono_layout().1,
                )))
            }
            MonoLayout::Regex(r, status) if *status != bt => IMonoLayout(ctx.dcx.intern(
                Layout::Mono(MonoLayout::Regex(*r, true), self.mono_layout().1),
            )),
            MonoLayout::IfParser(inner, c, status) if bool::from(*status) != bt => {
                let new_status = if bt { WiggleKind::If } else { WiggleKind::Try };
                IMonoLayout(ctx.dcx.intern(Layout::Mono(
                    MonoLayout::IfParser(*inner, *c, new_status),
                    self.mono_layout().1,
                )))
            }
            _ => self,
        }
    }

    pub fn remove_backtracking(self, ctx: &mut AbsIntCtx<'a, ILayout<'a>>) -> IMonoLayout<'a> {
        self.set_backtracking(ctx, false)
    }

    pub fn backtrack_statuses(self, ctx: &mut AbsIntCtx<'a, ILayout<'a>>) -> [IMonoLayout<'a>; 2] {
        [
            self.set_backtracking(ctx, true),
            self.set_backtracking(ctx, false),
        ]
    }

    pub fn unapply_nominal(
        &self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> (ILayout<'a>, IMonoLayout<'a>) {
        let mono = self.mono_layout().0;
        let MonoLayout::Nominal(pd, Some(from), to) = mono else {
            dbpanic!(
                ctx.db,
                "Expected nominal layout with from type, got {}",
                &self.inner()
            );
        };
        let parser_ty = ctx.db.intern_type(Type::ParserArg {
            result: self.mono_layout().1,
            arg: from.1,
        });
        let to_layout = ctx.dcx.intern(Layout::Mono(
            MonoLayout::NominalParser(*pd, to.clone(), false),
            parser_ty,
        ));
        let to_mono = IMonoLayout(to_layout);
        let from_layout = from.0;
        (from_layout, to_mono)
    }

    pub fn make_thunk(
        id: hir::ParserDefId,
        ty: TypeId,
        fields: &FxHashMap<Arg, (ILayout<'a>, TypeId)>,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> SResult<Self> {
        let parserdef = id.lookup(ctx.db)?;
        let from = fields.get(&Arg::From).copied();
        let mut args = Vec::new();
        for arg in parserdef.args.into_iter().flatten() {
            let layout = fields[&Arg::Named(arg.0)];
            args.push(layout);
        }
        let new_layout = Layout::Mono(MonoLayout::Nominal(id, from, args), ty);
        Ok(IMonoLayout(ctx.dcx.intern(new_layout)))
    }
}

impl<'a, 'l> IntoIterator for &'l ILayout<'a> {
    type Item = IMonoLayout<'a>;
    type IntoIter =
        std::iter::Map<std::slice::Iter<'l, ILayout<'a>>, fn(&'l ILayout<'a>) -> IMonoLayout<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        match &self.layout.1 {
            Layout::Mono(_, _) => std::slice::from_ref(self),
            Layout::Multi(l) => l.layouts.as_slice(),
            Layout::None => &[],
        }
        .iter()
        .map(|l| match l.layout.1 {
            Layout::Mono(_, _) => IMonoLayout(*l),
            _ => unreachable!(),
        })
    }
}

impl<'a> ILayout<'a> {
    pub fn maybe_mono(self) -> Option<IMonoLayout<'a>> {
        match self.layout.1 {
            Layout::None => None,
            Layout::Mono(_, _) => Some(IMonoLayout(self)),
            Layout::Multi(_) => None,
        }
    }

    pub fn contains_deref(&self) -> bool {
        self.into_iter()
            .any(|x| matches!(x.mono_layout().0, MonoLayout::Nominal(_, _, _)))
    }

    pub fn is_multi(self) -> bool {
        matches!(self.layout.1, Layout::Multi(_))
    }

    pub fn is_int(self) -> bool {
        matches!(
            self.layout.1,
            Layout::Mono(MonoLayout::Primitive(PrimitiveType::Int), _)
        )
    }
    fn map(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        mut f: impl FnMut(IMonoLayout<'a>, &mut AbsIntCtx<'a, ILayout<'a>>) -> ILayout<'a>,
    ) -> ILayout<'a> {
        let mut acc = BTreeSet::new();
        for layout in &self {
            let result_layout = f(layout, ctx);
            match &result_layout.layout.1 {
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
            [] => ILayout {
                layout: ctx.dcx.intern.intern(Layout::None),
            },
            _ => ctx
                .dcx
                .intern(Layout::Multi(MultiLayout { layouts: acc_vec })),
        }
    }

    fn try_map(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        mut f: impl FnMut(
            IMonoLayout<'a>,
            &mut AbsIntCtx<'a, ILayout<'a>>,
        ) -> Result<ILayout<'a>, LayoutError>,
    ) -> Result<ILayout<'a>, LayoutError> {
        let mut acc = BTreeSet::new();
        for layout in &self {
            let result_layout = f(layout, ctx)?;
            match &result_layout.layout.1 {
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
            [] => ctx.dcx.intern(Layout::None),
            _ => ctx
                .dcx
                .intern(Layout::Multi(MultiLayout { layouts: acc_vec })),
        })
    }

    pub fn deref_to_level(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        level: DerefLevel,
    ) -> Result<(ILayout<'a>, bool), LayoutError> {
        let mut changed = false;
        let res = self.try_map(ctx, |layout, ctx| {
            if let MonoLayout::Primitive(PrimitiveType::U8) = layout.mono_layout().0 {
                if level == DerefLevel::zero() {
                    return Ok(ctx.dcx.int(ctx.db));
                } else {
                    return Ok(layout.0);
                }
            };
            let is_fun = if let MonoLayout::Nominal(pd, _, _) = layout.mono_layout().0 {
                !pd.lookup(ctx.db)?.kind.thunky()
            } else {
                false
            };
            let mono_level = ctx.db.deref_level(layout.mono_layout().1)?;
            Ok(if mono_level <= level && !is_fun {
                layout.0
            } else {
                changed = true;
                let res_ty = layout.mono_layout().1;
                ctx.eval_pd(layout.inner(), res_ty)
                    .ok_or_else(SilencedError::new)?
                    .deref_to_level(ctx, level)?
                    .0
            })
        });
        res.map(|x| (x, changed))
    }

    fn get_captured(
        self,
        ctx: &AbsIntCtx<'a, ILayout<'a>>,
        id: DefId,
    ) -> Result<Option<ILayout<'a>>, LayoutError> {
        Ok(match match &self.layout.1 {
            Layout::Mono(m, _) => m,
            Layout::Multi(_) => {
                panic!("Attempting to get captured variable inside multi layout block")
            }
            Layout::None => {
                panic!("Attempting to get captured variable inside empty layout")
            }
        } {
            MonoLayout::BlockParser(_, captures, ..) => captures,
            MonoLayout::Nominal(pd, _, args) | MonoLayout::NominalParser(pd, args, _) => {
                return Ok(ctx.db.parserdef_arg_index(*pd, id)?.map(|i| args[i].0))
            }
            otherwise => panic!("Attempting to get captured variable inside {:?}", otherwise),
        }
        .get(&id)
        .copied())
    }

    fn array_primitive(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> Result<ILayout<'a>, LayoutError> {
        self.try_map(ctx, |layout, ctx| match layout.mono_layout().0 {
            MonoLayout::SlicePtr => {
                let u8 = PrimitiveType::U8;
                let u8_ty = ctx.db.intern_type(Type::Primitive(u8));
                Ok(ctx
                    .dcx
                    .intern(Layout::Mono(MonoLayout::Primitive(u8), u8_ty)))
            }
            MonoLayout::Range => Ok(ctx.dcx.int(ctx.db)),
            MonoLayout::Array { parser, slice } => parser.apply_arg(ctx, *slice),
            // for calculating the length of a parser, the argument is an int, and the result
            // of applying an int to a parser is never used, so here we use the bottom
            // value
            MonoLayout::Primitive(PrimitiveType::Int) => Ok(ctx.dcx.intern(Layout::None)),
            _ => panic!("Attempting to get an primitive element from non-array"),
        })
    }

    pub fn apply_arg(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        from: ILayout<'a>,
    ) -> Result<ILayout<'a>, LayoutError> {
        self.deref_to_level(ctx, DerefLevel::zero())?
            .0
            .try_map(ctx, |layout, ctx| {
                let (result_type, arg_type) =
                    match ctx.db.lookup_intern_type(layout.mono_layout().1) {
                        Type::ParserArg { result, arg } => (result, arg),
                        _ => panic!("Attempting to apply argument to non-parser type"),
                    };
                let from = from.typecast(ctx, arg_type)?.0;
                match layout.mono_layout().0 {
                    MonoLayout::NominalParser(pd, args, _) => {
                        let ret = ctx.apply_thunk_arg(*pd, result_type, (from, arg_type), args)?;
                        Ok(ret)
                    }
                    MonoLayout::BlockParser(block_id, _, subst, _) => {
                        let res = ctx
                            .eval_block(
                                *block_id,
                                self,
                                Some((from, arg_type)),
                                result_type,
                                subst.clone(),
                            )
                            .ok_or_else(|| SilencedError::new().into());
                        if from.is_int() {
                            Ok(ctx.dcx.intern(Layout::None))
                        } else {
                            res
                        }
                    }
                    MonoLayout::Single => from.array_primitive(ctx),
                    MonoLayout::Nil => Ok(ctx.dcx.intern(Layout::Mono(
                        MonoLayout::Primitive(PrimitiveType::Unit),
                        result_type,
                    ))),
                    MonoLayout::Regex(..) => Ok(from),
                    MonoLayout::IfParser(inner, ..) => inner.apply_arg(ctx, from),
                    MonoLayout::ArrayParser(Some((parser, Some(_))))
                    | MonoLayout::ArrayFillParser(Some(parser)) => {
                        if from.is_int() {
                            return Ok(ctx.dcx.intern(Layout::None));
                        }
                        if let Layout::Mono(MonoLayout::Single, _) = parser.layout.1 {
                            return Ok(from);
                        }
                        Ok(ctx.dcx.intern(Layout::Mono(
                            MonoLayout::Array {
                                parser: *parser,
                                slice: from,
                            },
                            result_type,
                        )))
                    }
                    _ => panic!("Attempting to apply argument to non-parser layout"),
                }
            })
    }

    pub fn eval_fun(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> Result<ILayout<'a>, LayoutError> {
        self.try_map(ctx, |layout, ctx| {
            let Type::FunctionArg(result_type, _) =
                ctx.db.lookup_intern_type(layout.mono_layout().1)
            else {
                panic!("Attempting to apply function to non-function type")
            };
            let (pd, present_args) = match layout.mono_layout().0 {
                MonoLayout::NominalParser(pd, present_args, _) => (pd, present_args),
                MonoLayout::BlockParser(block_id, _, subst, _) => {
                    return ctx
                        .eval_block(*block_id, self, None, result_type, subst.clone())
                        .ok_or_else(|| SilencedError::new().into())
                }
                MonoLayout::ArrayParser(Some((parser, Some(int)))) => {
                    return Ok(ctx.dcx.intern(Layout::Mono(
                        MonoLayout::ArrayParser(Some((*parser, Some(*int)))),
                        result_type,
                    )))
                }
                MonoLayout::ArrayFillParser(Some(parser)) => {
                    return Ok(ctx.dcx.intern(Layout::Mono(
                        MonoLayout::ArrayFillParser(Some(*parser)),
                        result_type,
                    )))
                }
                _ => panic!("Attempting to apply function to non-function layout"),
            };
            let parserdef = pd.lookup(ctx.db)?;
            if parserdef.from.is_some() {
                return Ok(ctx.dcx.intern(Layout::Mono(
                    MonoLayout::NominalParser(*pd, present_args.clone(), true),
                    result_type,
                )));
            }
            Ok(ctx.dcx.intern(Layout::Mono(
                MonoLayout::Nominal(*pd, None, present_args.clone()),
                result_type,
            )))
        })
    }

    pub fn apply_fun(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        args: impl Iterator<Item = (ILayout<'a>, TypeId)> + Clone,
    ) -> Result<ILayout<'a>, LayoutError> {
        self.try_map(ctx, |layout, ctx| {
            let (result_type, arg_types) = match ctx.db.lookup_intern_type(layout.mono_layout().1) {
                Type::FunctionArg(ret, args) => (ret, args),
                _ => panic!("Attempting to apply function to non-function type"),
            };
            let typecast_args = arg_types
                .iter()
                .zip(args.clone())
                .map(|(ty, (arg, _))| arg.typecast(ctx, *ty).map(|x| (x.0, *ty)))
                .collect::<Result<Vec<_>, _>>()?;
            match layout.mono_layout().0 {
                MonoLayout::NominalParser(pd, present_args, backtracks) => {
                    let present_args = present_args
                        .iter()
                        .chain(typecast_args.iter())
                        .copied()
                        .collect();
                    let layout = MonoLayout::NominalParser(*pd, present_args, *backtracks);
                    let ty = ctx.db.intern_type(Type::FunctionArg(
                        result_type,
                        Arc::new(arg_types[typecast_args.len()..].to_vec()),
                    ));
                    let res = ctx.dcx.intern(Layout::Mono(layout, ty));
                    Ok(res)
                }
                MonoLayout::ArrayParser(args) => {
                    let new_args = match (args, typecast_args.as_slice()) {
                        (args, []) => *args,
                        (None, [(parser, _)]) => Some((*parser, None)),
                        (None, [(parser, _), (arg, _)]) | (Some((parser, None)), [(arg, _)]) => {
                            Some(((*parser), Some(*arg)))
                        }
                        _ => panic!("Invalid number of arguments for array parser"),
                    };
                    let layout = MonoLayout::ArrayParser(new_args);
                    let ty = ctx.db.intern_type(Type::FunctionArg(
                        result_type,
                        Arc::new(arg_types[typecast_args.len()..].to_vec()),
                    ));
                    let res = ctx.dcx.intern(Layout::Mono(layout, ty));
                    Ok(res)
                }
                MonoLayout::ArrayFillParser(arg) => {
                    let new_arg = match (arg, typecast_args.as_slice()) {
                        (arg, []) => *arg,
                        (None, [(parser, _)]) => Some(*parser),
                        _ => panic!("Invalid number of arguments for array fill parser"),
                    };
                    let layout = MonoLayout::ArrayFillParser(new_arg);
                    let ty = ctx.db.intern_type(Type::FunctionArg(
                        result_type,
                        Arc::new(arg_types[typecast_args.len()..].to_vec()),
                    ));
                    let res = ctx.dcx.intern(Layout::Mono(layout, ty));
                    Ok(res)
                }
                _ => panic!("Attempting to apply function to non-function layout"),
            }
        })
    }

    fn access_field(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        field: FieldName,
    ) -> Result<ILayout<'a>, LayoutError> {
        self.try_map(ctx, |layout, ctx| {
            let ty = layout.mono_layout().1;
            let ldt = ctx.db.least_deref_type(ty)?;
            let casted_layout = layout.0.typecast(ctx, ldt)?.0;
            Ok(
                casted_layout.map(ctx, |layout, _| match layout.mono_layout().0 {
                    MonoLayout::Block(_, fields) => fields[&field],
                    _ => panic!("Field access on non-block {layout:?}"),
                }),
            )
        })
    }

    fn access_front(self, ctx: &mut AbsIntCtx<'a, ILayout<'a>>) -> ILayout<'a> {
        self.map(ctx, |layout, _| match layout.mono_layout().0 {
            MonoLayout::Nominal(_, from, _) => {
                from.expect(
                    "Attempting to get 'front' field from non-from field containing nominal",
                )
                .0
            }
            MonoLayout::IfParser(inner, _, _) => *inner,
            _ => dbpanic!(
                ctx.db,
                "Attempting to get 'front' field from non-nominal or if-parser {}",
                &self
            ),
        })
    }

    fn with_backtrack_status(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        backtracks: BtMarkKind,
    ) -> ILayout<'a> {
        if backtracks == BtMarkKind::KeepBt {
            return self;
        }
        self.map(ctx, |layout, ctx| layout.remove_backtracking(ctx).inner())
    }

    pub fn size_align_without_vtable(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> SResult<SizeAlign> {
        if let Some(sa) = ctx.dcx.sizes.get(&self) {
            return Ok(*sa);
        }
        let data = &ctx.dcx.target_data;
        let ret = match &self.layout.1 {
            Layout::None => Zst::tsize(data),
            Layout::Mono(MonoLayout::Block(id, fields), _) => {
                self.block_manifestation(ctx, *id, fields)?.size
            }
            Layout::Mono(MonoLayout::BlockParser(_, captures, ..), _) => {
                self.block_parser_manifestation(ctx, captures)?.size
            }
            Layout::Mono(
                MonoLayout::IfParser(inner, _, _)
                | MonoLayout::ArrayFillParser(Some(inner))
                | MonoLayout::ArrayParser(Some((inner, None))),
                _,
            ) => SizeAlign::ZST.cat(inner.size_align(ctx)?),
            Layout::Mono(
                MonoLayout::ArrayParser(Some((parser, Some(l))))
                | MonoLayout::Array { parser, slice: l },
                _,
            ) => SizeAlign::ZST
                .cat(parser.size_align(ctx)?)
                .cat(l.size_align(ctx)?),
            Layout::Mono(MonoLayout::Nominal(id, from, args), _) => {
                self.nominal_manifestation(ctx, *id, *from, args)?.size
            }
            Layout::Mono(MonoLayout::NominalParser(id, args, _), _) => {
                self.nominal_parser_manifestation(ctx, *id, args)?.size
            }
            Layout::Mono(MonoLayout::SlicePtr, _) => <&Zst>::tsize(data).array(2),
            Layout::Mono(MonoLayout::Range, _) => i64::tsize(data).array(2),
            Layout::Mono(MonoLayout::Primitive(PrimitiveType::Bit), _) => bool::tsize(data),
            Layout::Mono(MonoLayout::Primitive(PrimitiveType::Char), _) => char::tsize(data),
            Layout::Mono(MonoLayout::Primitive(PrimitiveType::Int), _) => i64::tsize(data),
            Layout::Mono(MonoLayout::Primitive(PrimitiveType::Unit), _) => Zst::tsize(data),
            Layout::Mono(MonoLayout::Primitive(PrimitiveType::U8), _) => <&Zst>::tsize(data),
            Layout::Mono(
                MonoLayout::Single
                | MonoLayout::Nil
                | MonoLayout::Regex(_, _)
                | MonoLayout::ArrayParser(None)
                | MonoLayout::ArrayFillParser(None),
                _,
            ) => SizeAlign::ZST,
            Layout::Multi(m) => m.layouts.iter().try_fold(Zst::tsize(data), |sa, layout| {
                Ok::<_, SilencedError>(sa.union(layout.size_align(ctx)?))
            })?,
        };
        ctx.dcx.sizes.insert(self, ret);
        Ok(ret)
    }

    pub fn size_align(self, ctx: &mut AbsIntCtx<'a, ILayout<'a>>) -> SResult<SizeAlign> {
        let size = self.size_align_without_vtable(ctx)?;
        Ok(if let Layout::Multi(_) = self.layout.1 {
            size.tac(<*const u8>::tsize(&ctx.dcx.target_data))
        } else {
            size
        })
    }

    pub fn block_manifestation(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        id: hir::BlockId,
        fields: &BTreeMap<FieldName, ILayout<'a>>,
    ) -> SResult<Arc<StructManifestation>> {
        if let Some(man) = ctx.dcx.manifestations.get(&self) {
            return Ok(man.clone());
        }
        let mut manifest = UnfinishedManifestation::new();
        let block = id.lookup(ctx.db)?;
        let root_ctx = block.root_context.lookup(ctx.db)?;
        for (field, layout) in fields {
            let field_id = root_ctx
                .vars
                .get(*field)
                .map(|x| *x.inner())
                .expect("Could not find field during layout calculation");
            let field_size = layout.size_align(ctx)?;
            manifest.add_field(field_id, field_size);
        }
        let discriminant_mapping = ctx.db.discriminant_mapping(id)?;
        let manifest = Arc::new(manifest.finalize(discriminant_mapping));
        ctx.dcx.manifestations.insert(self, manifest.clone());
        Ok(manifest)
    }

    pub fn block_parser_manifestation(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        captures: &BTreeMap<DefId, ILayout<'a>>,
    ) -> SResult<Arc<StructManifestation>> {
        if let Some(man) = ctx.dcx.manifestations.get(&self) {
            return Ok(man.clone());
        }
        let mut manifest = UnfinishedManifestation::new();
        for (capture, layout) in captures.iter() {
            let sa = layout.size_align(ctx)?;
            manifest.add_field(*capture, sa);
        }
        let manifest = Arc::new(manifest.finalize(Default::default()));
        ctx.dcx.manifestations.insert(self, manifest.clone());
        Ok(manifest)
    }

    pub fn nominal_parser_manifestation(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        id: hir::ParserDefId,
        args: &[(ILayout<'a>, TypeId)],
    ) -> SResult<Arc<StructManifestation>> {
        if let Some(man) = ctx.dcx.manifestations.get(&self) {
            return Ok(man.clone());
        }
        let mut manifest = UnfinishedManifestation::new();
        let arg_ids = id.lookup(ctx.db)?.args.unwrap_or_default();
        for ((arg, _), id) in args.iter().zip(arg_ids.iter()) {
            let sa = arg.size_align(ctx)?;
            manifest.add_field(id.0, sa);
        }
        let manifest = Arc::new(manifest.finalize(Default::default()));
        ctx.dcx.manifestations.insert(self, manifest.clone());
        Ok(manifest)
    }

    pub fn nominal_manifestation(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        id: hir::ParserDefId,
        from: Option<(ILayout<'a>, TypeId)>,
        args: &[(ILayout<'a>, TypeId)],
    ) -> SResult<Arc<StructManifestation>> {
        if let Some(man) = ctx.dcx.manifestations.get(&self) {
            return Ok(man.clone());
        }
        let mut manifest = UnfinishedManifestation::new();
        let parserdef = id.lookup(ctx.db)?;
        if let Some((from, _)) = from {
            let sa = from.size_align(ctx)?;
            manifest.add_field(parserdef.from.unwrap().0, sa);
        }
        let arg_ids = id.lookup(ctx.db)?.args.unwrap_or_default();
        for ((arg, _), id) in args.iter().zip(arg_ids.iter()) {
            let sa = arg.size_align(ctx)?;
            manifest.add_field(id.0, sa);
        }
        let manifest = Arc::new(manifest.finalize(Default::default()));
        ctx.dcx.manifestations.insert(self, manifest.clone());
        Ok(manifest)
    }
}

pub fn canon_layout<'a, 'b>(
    ctx: &'b mut AbsIntCtx<'a, ILayout<'a>>,
    ty: TypeId,
) -> Result<ILayout<'a>, LayoutError> {
    let typ = ctx.db.lookup_intern_type(ty);
    let make_layout =
        |ctx: &'b mut AbsIntCtx<'a, ILayout<'a>>, x| ctx.dcx.intern(Layout::Mono(x, ty));
    match typ {
        Type::Primitive(x) => Ok(make_layout(ctx, MonoLayout::Primitive(x))),
        Type::Loop(_, inner_ty) => {
            let inner_type = ctx.db.lookup_intern_type(inner_ty);
            match inner_type {
                Type::Primitive(PrimitiveType::Int | PrimitiveType::U8) => {
                    Ok(make_layout(ctx, MonoLayout::SlicePtr))
                }
                _ => Err(LayoutError::LayoutError),
            }
        }
        Type::Nominal(n) => {
            let def_id = match NominalId::from_nominal_head(&n) {
                NominalId::Def(d) => d,
                NominalId::Block(_) => return Err(LayoutError::LayoutError),
            };
            let from = n
                .parse_arg
                .map(|arg| -> Result<_, LayoutError> { Ok((canon_layout(ctx, arg)?, arg)) })
                .transpose()?;
            let args: Vec<_> = n
                .fun_args
                .iter()
                .map(|arg| Ok((canon_layout(ctx, *arg)?, *arg)))
                .collect::<Result<_, LayoutError>>()?;
            Ok(make_layout(ctx, MonoLayout::Nominal(def_id, from, args)))
        }
        Type::ParserArg { .. } | Type::FunctionArg(_, _) => Err(LayoutError::LayoutError),
        Type::TypeVarRef(_) | Type::Unknown => Err(LayoutError::LayoutError),
    }
}

pub fn init_globals<'comp>(ctx: &mut AbsIntCtx<'comp, ILayout<'comp>>) -> Result<(), LayoutError> {
    let pds = ctx.db.global_sequence()?;
    for pd in pds.iter() {
        let ret_ty = ctx.db.parser_returns(*pd)?.deref;
        let thunk_ty = ctx
            .db
            .intern_type(Type::Nominal(ctx.db.parser_args(*pd)?.thunk));
        let thunk_fun_ty = ctx
            .db
            .intern_type(Type::FunctionArg(thunk_ty, Default::default()));
        let fun_layout = ctx.dcx.intern(Layout::Mono(
            MonoLayout::NominalParser(*pd, Default::default(), true),
            thunk_fun_ty,
        ));
        let return_layout = fun_layout.eval_fun(ctx)?.typecast(ctx, ret_ty)?.0;
        ctx.dcx
            .globals
            .insert(*pd, (fun_layout.maybe_mono().unwrap(), return_layout));
    }
    Ok(())
}

#[derive(Debug)]
pub enum LayoutError {
    LayoutError,
    Silent(SilencedError),
}

impl From<yaboc_base::error::SilencedError> for LayoutError {
    fn from(s: yaboc_base::error::SilencedError) -> Self {
        LayoutError::Silent(s)
    }
}

impl IsSilenced for LayoutError {
    fn is_silenced(&self) -> bool {
        matches!(self, LayoutError::Silent(_))
    }
}

pub type AbsLayoutCtx<'a> = AbsIntCtx<'a, ILayout<'a>>;

type LayoutSlice<'a> = &'a Uniq<[ILayout<'a>]>;

pub struct LayoutContext<'a> {
    pub intern: Interner<'a, InternedLayout<'a>>,
    pub intern_slice: Interner<'a, [ILayout<'a>]>,
    sizes: FxHashMap<ILayout<'a>, SizeAlign>,
    manifestations: FxHashMap<ILayout<'a>, Arc<StructManifestation>>,
    hashes: LayoutHasher<'a>,
    globals: FxHashMap<hir::ParserDefId, (IMonoLayout<'a>, ILayout<'a>)>,
    target_data: TargetLayoutData,
}

impl<'a> LayoutContext<'a> {
    pub fn new(bump: &'a Bump, target_data: TargetLayoutData) -> Self {
        let intern = Interner::new(bump);
        let intern_slice = Interner::new(bump);
        Self {
            intern,
            intern_slice,
            sizes: Default::default(),
            manifestations: Default::default(),
            hashes: LayoutHasher::new(),
            globals: Default::default(),
            target_data,
        }
    }
    pub fn intern(&mut self, layout: InternedLayout<'a>) -> ILayout<'a> {
        ILayout {
            layout: self.intern.intern(layout),
        }
    }
    pub fn manifestation(&self, layout: ILayout<'a>) -> Arc<StructManifestation> {
        self.manifestations[&layout].clone()
    }

    pub fn int(&mut self, db: &(impl TypeInterner + ?Sized)) -> ILayout<'a> {
        let int_ty = db.intern_type(Type::Primitive(PrimitiveType::Int));
        self.intern(Layout::Mono(
            MonoLayout::Primitive(PrimitiveType::Int),
            int_ty,
        ))
    }

    pub fn primitive(
        &mut self,
        db: &(impl TypeInterner + ?Sized),
        prim: PrimitiveType,
    ) -> ILayout<'a> {
        let int_ty = db.intern_type(Type::Primitive(prim));
        self.intern(Layout::Mono(MonoLayout::Primitive(prim), int_ty))
    }

    pub fn layout_hash(&mut self, db: &(impl Layouts + ?Sized), layout: ILayout<'a>) -> [u8; 8] {
        self.hashes.hash(layout, db)[..8].try_into().unwrap()
    }
}

impl<'a> AbstractDomain<'a> for ILayout<'a> {
    type Err = LayoutError;
    type DB = dyn Layouts;
    type DomainContext = LayoutContext<'a>;

    fn make_block(
        ctx: &mut AbsIntCtx<'a, Self>,
        id: hir::BlockId,
        ty: TypeId,
        fields: &FxHashMap<FieldName, Self>,
    ) -> Result<Self, Self::Err> {
        let new_layout = ctx.dcx.intern(Layout::Mono(
            MonoLayout::Block(id, fields.iter().map(|(a, b)| (*a, *b)).collect()),
            ty,
        ));
        Ok(new_layout)
    }

    fn join(self, ctx: &mut AbsIntCtx<'a, Self>, other: Self) -> Result<(Self, bool), Self::Err> {
        if self == other {
            return Ok((self, false));
        }
        let layouts = match (&self.layout.1, &other.layout.1) {
            (Layout::None, Layout::None) => return Ok((self, false)),
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
        Ok((ctx.dcx.intern(Layout::Multi(MultiLayout { layouts })), true))
    }

    fn widen(self, ctx: &mut AbsIntCtx<'a, Self>, other: Self) -> Result<(Self, bool), Self::Err> {
        self.join(ctx, other)
    }

    fn eval_expr(
        ctx: &mut AbsIntCtx<'a, Self>,
        expr: ExprHead<Resolved, &(Self, TypeId)>,
        ty: TypeId,
    ) -> Result<Self, Self::Err> {
        let mut make_layout = |x| ctx.dcx.intern(Layout::Mono(x, ty));
        Ok(match expr {
            ExprHead::Niladic(n) => match n {
                ResolvedAtom::Char(_) => make_layout(MonoLayout::Primitive(PrimitiveType::Char)),
                ResolvedAtom::Number(_) => make_layout(MonoLayout::Primitive(PrimitiveType::Int)),
                ResolvedAtom::Bool(_) => make_layout(MonoLayout::Primitive(PrimitiveType::Bit)),
                ResolvedAtom::Val(id) => ctx.var_by_id(id)?,
                ResolvedAtom::Single => make_layout(MonoLayout::Single),
                ResolvedAtom::Nil => make_layout(MonoLayout::Nil),
                ResolvedAtom::Array => make_layout(MonoLayout::ArrayParser(None)),
                ResolvedAtom::ArrayFill => make_layout(MonoLayout::ArrayFillParser(None)),
                ResolvedAtom::Span(..) => {
                    ctx.active_ambience().expect("Span in non-parser context").0
                }
                ResolvedAtom::Regex(r) => make_layout(MonoLayout::Regex(r, true)),
                ResolvedAtom::ParserDef(pd) => {
                    make_layout(MonoLayout::NominalParser(pd, Vec::new(), true))
                }
                ResolvedAtom::Global(pd) => ctx.dcx.globals[&pd].1,
                ResolvedAtom::Block(block_id, _) => {
                    let mut captures = BTreeMap::new();
                    let capture_ids = ctx.db.captures(block_id);
                    for capture in capture_ids.iter() {
                        let capture_value = ctx
                            .active_block()
                            .unwrap_or(*ctx.active_pd())
                            .get_captured(ctx, *capture)?;
                        let capture_value = match capture_value {
                            Some(x) => x,
                            None => ctx.var_by_id(*capture)?,
                        };
                        captures.insert(*capture, capture_value);
                    }
                    let typeargs = ctx.current_type_substitutions();
                    let res = ctx.dcx.intern(Layout::Mono(
                        MonoLayout::BlockParser(block_id, captures, typeargs, true),
                        ty,
                    ));
                    res
                }
                ResolvedAtom::Captured(capture) => ctx
                    .active_block()
                    .and_then(|s| s.get_captured(ctx, capture).transpose())
                    .unwrap_or_else(|| {
                        Ok(ctx.active_pd().get_arg(ctx, Arg::Named(capture)).unwrap())
                    })?,
            },
            ExprHead::Monadic(op, inner) => match op {
                ValUnOp::Not | ValUnOp::Neg | ValUnOp::Size => {
                    make_layout(MonoLayout::Primitive(PrimitiveType::Int))
                }
                ValUnOp::Wiggle(cid, kind) => {
                    let ldt_parser_ty = ctx.db.least_deref_type(inner.1)?;
                    let parser_type = ctx.db.lookup_intern_type(ldt_parser_ty);
                    if matches!(parser_type, Type::ParserArg { .. }) {
                        let casted_inner = inner.0.typecast(ctx, ldt_parser_ty)?.0;
                        ctx.dcx.intern(Layout::Mono(
                            MonoLayout::IfParser(casted_inner, cid, kind),
                            ty,
                        ))
                    } else {
                        inner.0
                    }
                }
                ValUnOp::Dot(a, ..) => inner.0.access_field(ctx, a)?,
                ValUnOp::BtMark(bt) => inner.0.with_backtrack_status(ctx, bt),
                ValUnOp::EvalFun => inner.0.eval_fun(ctx)?,
                ValUnOp::GetAddr => IMonoLayout::u8_array(ctx).inner(),
            },
            ExprHead::Dyadic(op, [lhs, rhs]) => match op {
                ValBinOp::ParserApply => rhs.0.apply_arg(ctx, lhs.0)?,
                ValBinOp::Else => lhs.0.join(ctx, rhs.0)?.0,
                ValBinOp::Then => rhs.0,
                ValBinOp::Range => make_layout(MonoLayout::Range),
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
            ExprHead::Variadic(ValVarOp::PartialApply(_), inner) => inner[0]
                .0
                .apply_fun(ctx, inner[1..].iter().copied().copied())?,
        })
    }

    fn typecast(
        self,
        ctx: &mut AbsIntCtx<'a, Self>,
        ty: TypeId,
    ) -> Result<(Self, bool), Self::Err> {
        let deref_level = ctx.db.deref_level(ty)?;
        self.deref_to_level(ctx, deref_level)
    }

    fn get_arg(
        self,
        ctx: &mut AbsIntCtx<'a, Self>,
        arg: yaboc_absint::Arg,
    ) -> Result<Self, Self::Err> {
        self.try_map(ctx, |l, _| match l.mono_layout().0 {
            MonoLayout::Nominal(pd, from, args) => match arg {
                Arg::Named(def) => {
                    let idx = ctx
                        .db
                        .parserdef_arg_index(*pd, def)?
                        .expect("arg not found");
                    Ok(args[idx].0)
                }
                Arg::From => Ok(from.expect("unexpectedly did not have from arg").0),
            },
            _ => panic!("get_arg called on non-nominal layout"),
        })
    }

    fn make_thunk(
        ctx: &mut AbsIntCtx<'a, Self>,
        id: hir::ParserDefId,
        ty: TypeId,
        fields: &FxHashMap<Arg, (Self, TypeId)>,
    ) -> Result<Self, Self::Err> {
        Ok(IMonoLayout::make_thunk(id, ty, fields, ctx)?.inner())
    }

    fn bottom(ctx: &mut Self::DomainContext) -> Self {
        ctx.intern(Layout::None)
    }
}

#[cfg(test)]
mod tests {
    use bumpalo::Bump;
    use yaboc_absint::AbsIntDatabase;
    use yaboc_ast::{import::Import, AstDatabase};
    use yaboc_base::{
        config::ConfigDatabase, interner::InternerDatabase, source::FileDatabase, Context,
    };
    use yaboc_base::{dbeprintln, dbformat};
    use yaboc_constraint::ConstraintDatabase;
    use yaboc_dependents::DependentsDatabase;
    use yaboc_hir::{HirDatabase, Parser};
    use yaboc_hir_types::{HirTypesDatabase, TyHirs};
    use yaboc_mir::MirDatabase;
    use yaboc_req::NeededBy;
    use yaboc_resolve::ResolveDatabase;
    use yaboc_types::TypeInternerDatabase;

    #[salsa::database(
        InternerDatabase,
        ConfigDatabase,
        AstDatabase,
        FileDatabase,
        HirDatabase,
        ResolveDatabase,
        TypeInternerDatabase,
        HirTypesDatabase,
        DependentsDatabase,
        ConstraintDatabase,
        MirDatabase,
        AbsIntDatabase,
        LayoutDatabase
    )]
    #[derive(Default)]
    pub struct LayoutTestDatabase {
        storage: salsa::Storage<LayoutTestDatabase>,
    }

    impl salsa::Database for LayoutTestDatabase {}

    use crate::collect::collected_layouts;

    use super::*;

    #[test]
    fn layouts() {
        let ctx = Context::<LayoutTestDatabase>::mock(
            r"
def *first = ~
def *second = ~
def *main = {
    a: ~
    b: ~
    c: {
      | let c: *first = first!
      | let c: *second = second?
    }
    d: c.c
}
        ",
        );
        let bump = Bump::new();
        let layout_ctx = LayoutContext::new(&bump, yaboc_target::layout::POINTER64);
        let mut outlayer = AbsIntCtx::<ILayout>::new(&ctx.db, layout_ctx);
        let main = ctx.parser("main");
        let main_ty = ctx
            .db
            .intern_type(Type::Nominal(ctx.db.parser_args(main).unwrap().thunk));
        collected_layouts(&mut outlayer, &[main]).unwrap();
        let canon_2004 = canon_layout(&mut outlayer, main_ty).unwrap();
        let from = IMonoLayout::u8_array(&mut outlayer);
        let hash = outlayer.dcx.layout_hash(&ctx.db, from.inner());
        for lay in &canon_2004 {
            assert_eq!(
                lay.symbol(
                    &mut outlayer,
                    LayoutPart::Parse(
                        NeededBy::Len | NeededBy::Backtrack,
                        represent::ParserFunKind::Worker,
                        hash
                    ),
                    &ctx.db
                ),
                "main$88082689c1307225$parse_278b484a9faf573c_lb_worker"
            );
        }
        let main_block = outlayer.pd_result()[&canon_2004]
            .val()
            .as_ref()
            .unwrap()
            .returned;
        for lay in &main_block {
            assert_eq!(
                lay.symbol(
                    &mut outlayer,
                    LayoutPart::Parse(
                        NeededBy::Val | NeededBy::Backtrack,
                        represent::ParserFunKind::Wrapper,
                        hash
                    ),
                    &ctx.db
                ),
                "block_6c872ebf06064930$3b75767829875ed1$parse_278b484a9faf573c_vb"
            );
        }
        let field = |name| FieldName::Ident(ctx.id(name));
        assert_eq!(
            dbformat!(
                &ctx.db,
                "{}",
                &main_block.access_field(&mut outlayer, field("a")).unwrap(),
            ),
            "u8"
        );
        assert_eq!(
            dbformat!(
                &ctx.db,
                "{}",
                &main_block.access_field(&mut outlayer, field("b")).unwrap(),
            ),
            "u8"
        );
        let out = dbformat!(
            &ctx.db,
            "{}",
            &main_block
                .access_field(&mut outlayer, field("c"))
                .unwrap()
                .access_field(&mut outlayer, field("c"))
                .unwrap()
        );
        assert!(
            ["nominal-parser?[[u8] *> [u8] &> file[_].second]() | nominal-parser[[u8] *> [u8] &> file[_].first]()",
             "nominal-parser[[u8] *> [u8] &> file[_].first]() | nominal-parser?[[u8] *> [u8] &> file[_].second]()"]
            .contains(&out.as_str())
        );
        assert_eq!(
            dbformat!(
                &ctx.db,
                "{}",
                &main_block.access_field(&mut outlayer, field("d")).unwrap(),
            ),
            "u8"
        );
    }
    #[test]
    fn tailsize() {
        let ctx = Context::<LayoutTestDatabase>::mock(
            r"
export
def *test = [2][3][5]
            ",
        );
        let bump = Bump::new();
        let layout_ctx = LayoutContext::new(&bump, yaboc_target::layout::POINTER64);
        let mut outlayer = AbsIntCtx::<ILayout>::new(&ctx.db, layout_ctx);
        let test = ctx.parser("test");
        let test_ty = ctx
            .db
            .intern_type(Type::Nominal(ctx.db.parser_args(test).unwrap().thunk));
        let layouts = collected_layouts(&mut outlayer, &[test]).unwrap();
        let canon = canon_layout(&mut outlayer, test_ty)
            .unwrap()
            .maybe_mono()
            .unwrap();
        let array_ty = ctx
            .db
            .intern_type(Type::Loop(ArrayKind::Each, ctx.db.int()));
        let slice = outlayer
            .dcx
            .intern(Layout::Mono(MonoLayout::SlicePtr, array_ty));
        dbeprintln!(&ctx.db, "layout: {} -> {}", &slice, &canon);
        for ((from, to), sa) in layouts.tail_sa {
            dbeprintln!(&ctx.db, "tail: {} -> {}", &from, &to);
            eprintln!("{:?}", sa);
        }
    }
}
