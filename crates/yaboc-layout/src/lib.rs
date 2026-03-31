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
use yaboc_ast::expr::WiggleKind;
use yaboc_base::error::{IsSilenced, SResult, SilencedError};
use yaboc_base::interner::{DefId, FieldName, Regex};
use yaboc_base::low_effort_interner::{Interner, Uniq};
use yaboc_base::{dbformat, dbpanic};
use yaboc_expr::ExprHead;
use yaboc_hir::{self as hir, DefKind, HirIdWrapper, ParserDefId};
use yaboc_hir_types::HeadDiscriminant;
use yaboc_mir::Mirs;
use yaboc_resolve::expr::{Resolved, ResolvedAtom, ValBinOp, ValUnOp, ValVarOp};
use yaboc_target::layout::{PSize, SizeAlign, TargetLayoutData, TargetSized, Zst};
use yaboc_types::{PrimitiveType, Type, TypeId};

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
    pub fn add_field(&mut self, id: Option<DefId>, field_size: SizeAlign) {
        self.0.size = self.0.size.cat(field_size);
        let offset = self.0.size.after - field_size.after;
        if let Some(id) = id {
            self.0.field_offsets.insert(id, offset);
        }
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
            after: manifest.discriminant_mapping.len().div_ceil(8) as PSize,
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
        if !manifest.discriminant_mapping.len().is_multiple_of(8) {
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
    Mono(MonoLayout<Inner>),
    Multi(MultiLayout<Inner>),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum FuncLayoutKind {
    Fun,
    Parse,
}
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum MonoLayout<Inner> {
    Primitive(PrimitiveType),
    Ptr,
    SlicePtr,
    Range,
    Single,
    Regex(Regex),
    IfParser(Inner, HirConstraintId),
    ArrayParser(Option<(Inner, Option<(Inner, FuncLayoutKind)>)>),
    ArrayFillParser(Option<(Inner, FuncLayoutKind)>),
    Nominal(hir::ParserDefId, Option<Inner>, Vec<Inner>),
    NominalParser(hir::ParserDefId, Vec<Inner>, FuncLayoutKind),
    Block(hir::BlockId, BTreeMap<FieldName, Inner>),
    BlockParser(hir::BlockId, BTreeMap<DefId, Inner>),
    Lambda(hir::LambdaId, BTreeMap<DefId, Inner>, Vec<Inner>),
    Array { parser: Inner, slice: Inner },
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
    pub fn mono_layout(self) -> &'a MonoLayout<ILayout<'a>> {
        match self.0.layout.1 {
            Layout::Mono(ref mono) => mono,
            _ => unreachable!("Expected mono layout"),
        }
    }

    #[inline(always)]
    pub fn inner(self) -> ILayout<'a> {
        self.0
    }

    pub fn u8_single(ctx: &mut AbsIntCtx<'a, ILayout<'a>>) -> IMonoLayout<'a> {
        let single = ctx.dcx.intern(Layout::Mono(MonoLayout::Single));
        single.maybe_mono().unwrap()
    }

    pub fn u8_array(ctx: &mut AbsIntCtx<'a, ILayout<'a>>) -> IMonoLayout<'a> {
        let array = ctx.dcx.intern(Layout::Mono(MonoLayout::SlicePtr));
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
        Ok(match self.mono_layout() {
            MonoLayout::Nominal(pd, _, _) => Some(
                ctx.eval_pd(self.inner(), *pd)
                    .ok_or_else(SilencedError::new)?,
            ),
            MonoLayout::Ptr => Some(ctx.dcx.int()),
            _ => None,
        })
    }

    pub fn arg_num<DB: Layouts + ?Sized>(&self, db: &DB) -> SResult<Option<(usize, usize)>> {
        match self.mono_layout() {
            MonoLayout::NominalParser(pd, args, _) => Ok(db.argnum(*pd)?.map(|n| (n, args.len()))),
            MonoLayout::ArrayParser(Some((_, Some(_)))) => Ok(Some((2, 2))),
            MonoLayout::ArrayParser(Some((_, None))) => Ok(Some((2, 1))),
            MonoLayout::ArrayParser(None) => Ok(Some((2, 0))),
            MonoLayout::ArrayFillParser(Some(_)) => Ok(Some((1, 1))),
            MonoLayout::ArrayFillParser(None) => Ok(Some((1, 0))),
            MonoLayout::Lambda(id, _, args) => {
                let lambda = id.lookup(db)?;
                let arg_count = lambda.args.len();
                Ok(Some((arg_count, args.len())))
            }
            MonoLayout::BlockParser(..) => Ok(Some((0, 0))),
            _ => Ok(None),
        }
    }

    pub fn head_kind<DB: Layouts + ?Sized>(&self, db: &DB) -> HeadDiscriminant {
        let mono = self.mono_layout();
        match mono {
            MonoLayout::SlicePtr => HeadDiscriminant::SlicePtr,
            MonoLayout::Range => HeadDiscriminant::Range,
            MonoLayout::Primitive(PrimitiveType::Bit) => HeadDiscriminant::Bit,
            MonoLayout::Primitive(PrimitiveType::Char) => HeadDiscriminant::Char,
            MonoLayout::Primitive(PrimitiveType::Int) => HeadDiscriminant::Int,
            MonoLayout::Ptr => HeadDiscriminant::U8,
            MonoLayout::Primitive(PrimitiveType::Unit) => HeadDiscriminant::Unit,
            MonoLayout::NominalParser(.., kind) => match kind {
                FuncLayoutKind::Fun => HeadDiscriminant::FunctionArgs,
                FuncLayoutKind::Parse => HeadDiscriminant::Parser,
            },
            MonoLayout::Block(_, _) => HeadDiscriminant::Block,
            MonoLayout::Lambda(..) => HeadDiscriminant::FunctionArgs,
            MonoLayout::Array { .. } => HeadDiscriminant::Loop,
            MonoLayout::BlockParser(bid, ..) => match bid.lookup(db).unwrap().kind {
                yaboc_hir::BlockKind::Parser => HeadDiscriminant::Parser,
                yaboc_hir::BlockKind::Inline => HeadDiscriminant::FunctionArgs,
            },
            MonoLayout::ArrayFillParser(Some((_, FuncLayoutKind::Parse)))
            | MonoLayout::ArrayParser(Some((_, Some((_, FuncLayoutKind::Parse))))) => {
                HeadDiscriminant::Parser
            }
            MonoLayout::ArrayFillParser(_) | MonoLayout::ArrayParser(_) => {
                HeadDiscriminant::FunctionArgs
            }
            MonoLayout::Single | MonoLayout::Regex(_) | MonoLayout::IfParser(_, _) => {
                HeadDiscriminant::Parser
            }
            MonoLayout::Nominal(_, _, _) => HeadDiscriminant::Nominal,
        }
    }

    pub fn unapply_nominal(
        &self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> (ILayout<'a>, IMonoLayout<'a>) {
        let mono = self.mono_layout();
        let MonoLayout::Nominal(pd, Some(from), to) = mono else {
            dbpanic!(
                ctx.db,
                "Expected nominal layout with from type, got {}",
                &self.inner()
            );
        };
        let to_layout = ctx.dcx.intern(Layout::Mono(MonoLayout::NominalParser(
            *pd,
            to.clone(),
            FuncLayoutKind::Parse,
        )));
        let to_mono = IMonoLayout(to_layout);
        (*from, to_mono)
    }

    pub fn make_thunk(
        id: hir::ParserDefId,
        fields: &FxHashMap<Arg, ILayout<'a>>,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> SResult<Self> {
        let parserdef = id.lookup(ctx.db)?;
        let from = fields.get(&Arg::From).copied();
        let mut args = Vec::new();
        for arg in parserdef.args.into_iter().flatten() {
            let layout = fields[&Arg::Named(arg.0)];
            args.push(layout);
        }
        let new_layout = Layout::Mono(MonoLayout::Nominal(id, from, args));
        Ok(IMonoLayout(ctx.dcx.intern(new_layout)))
    }
}

impl<'a, 'l> IntoIterator for &'l ILayout<'a> {
    type Item = IMonoLayout<'a>;
    type IntoIter =
        std::iter::Map<std::slice::Iter<'l, ILayout<'a>>, fn(&'l ILayout<'a>) -> IMonoLayout<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        match &self.layout.1 {
            Layout::Mono(_) => std::slice::from_ref(self),
            Layout::Multi(l) => l.layouts.as_slice(),
            Layout::None => &[],
        }
        .iter()
        .map(|l| match l.layout.1 {
            Layout::Mono(_) => IMonoLayout(*l),
            _ => unreachable!(),
        })
    }
}

impl<'a> ILayout<'a> {
    pub fn maybe_mono(self) -> Option<IMonoLayout<'a>> {
        match self.layout.1 {
            Layout::None => None,
            Layout::Mono(_) => Some(IMonoLayout(self)),
            Layout::Multi(_) => None,
        }
    }

    pub fn contains_deref(&self) -> bool {
        self.into_iter()
            .any(|x| matches!(x.mono_layout(), MonoLayout::Nominal(_, _, _)))
    }

    pub fn is_multi(self) -> bool {
        matches!(self.layout.1, Layout::Multi(_))
    }

    pub fn is_int(self) -> bool {
        matches!(
            self.layout.1,
            Layout::Mono(MonoLayout::Primitive(PrimitiveType::Int))
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
                Layout::Mono(_) => {
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
                Layout::Mono(_) => {
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

    fn get_captured(
        self,
        ctx: &AbsIntCtx<'a, ILayout<'a>>,
        id: DefId,
    ) -> Result<Option<ILayout<'a>>, LayoutError> {
        Ok(match match &self.layout.1 {
            Layout::Mono(m) => m,
            Layout::Multi(_) => {
                dbpanic!(
                    ctx.db,
                    "Attempting to get captured variable inside multi layout block {}",
                    &self
                )
            }
            Layout::None => {
                panic!("Attempting to get captured variable inside empty layout")
            }
        } {
            MonoLayout::BlockParser(_, captures, ..) => captures,
            MonoLayout::Lambda(lambda_id, captures, args, ..) => {
                if id.parent(ctx.db) == Some(lambda_id.0) {
                    return Ok(ctx.db.lambda_arg_index(*lambda_id, id)?.map(|i| args[i]));
                } else {
                    captures
                }
            }
            MonoLayout::Nominal(pd, _, args) | MonoLayout::NominalParser(pd, args, _) => {
                return Ok(ctx.db.parserdef_arg_index(*pd, id)?.map(|i| args[i]));
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
        self.try_map(ctx, |layout, ctx| match layout.mono_layout() {
            MonoLayout::SlicePtr => Ok(ctx.dcx.intern(Layout::Mono(MonoLayout::Ptr))),
            MonoLayout::Range => Ok(ctx.dcx.int()),
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
        self.evaluate(ctx)?
            .0
            .try_map(ctx, |layout, ctx| match layout.mono_layout() {
                MonoLayout::NominalParser(pd, args, FuncLayoutKind::Parse) => {
                    let ret = ctx.apply_thunk_arg(*pd, from, args)?;
                    Ok(ret)
                }
                MonoLayout::BlockParser(block_id, _) => {
                    let res = ctx
                        .eval_block(*block_id, layout.inner(), Some(from))
                        .ok_or_else(|| SilencedError::new().into());
                    if from.is_int() {
                        Ok(ctx.dcx.intern(Layout::None))
                    } else {
                        res
                    }
                }
                MonoLayout::Single => from.array_primitive(ctx),
                MonoLayout::Regex(..) => Ok(from),
                MonoLayout::IfParser(inner, ..) => inner.apply_arg(ctx, from),
                MonoLayout::ArrayParser(Some((parser, Some(_))))
                | MonoLayout::ArrayFillParser(Some((parser, _))) => {
                    if from.is_int() {
                        return Ok(ctx.dcx.intern(Layout::None));
                    }
                    if let Layout::Mono(MonoLayout::Single) = parser.layout.1 {
                        return Ok(from);
                    }
                    Ok(ctx.dcx.intern(Layout::Mono(MonoLayout::Array {
                        parser: *parser,
                        slice: from,
                    })))
                }
                _ => dbpanic!(
                    ctx.db,
                    "Attempting to apply argument to non-parser layout {}",
                    &layout
                ),
            })
    }

    pub fn eval_fun(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> Result<ILayout<'a>, LayoutError> {
        self.try_map(ctx, |layout, ctx| {
            let (pd, present_args) =
                match layout.mono_layout() {
                    MonoLayout::NominalParser(pd, present_args, FuncLayoutKind::Fun) => {
                        (pd, present_args)
                    }
                    MonoLayout::BlockParser(block_id, _) => {
                        return ctx
                            .eval_block(*block_id, layout.inner(), None)
                            .ok_or_else(|| SilencedError::new().into());
                    }
                    MonoLayout::Lambda(lambda_id, _, _) => {
                        return ctx
                            .eval_lambda(*lambda_id, layout.inner())
                            .ok_or_else(|| SilencedError::new().into());
                    }
                    MonoLayout::ArrayParser(Some((parser, Some((int, FuncLayoutKind::Fun))))) => {
                        return Ok(ctx.dcx.intern(Layout::Mono(MonoLayout::ArrayParser(Some((
                            *parser,
                            Some((*int, FuncLayoutKind::Parse)),
                        ))))));
                    }
                    MonoLayout::ArrayFillParser(Some((parser, FuncLayoutKind::Fun))) => {
                        return Ok(ctx.dcx.intern(Layout::Mono(MonoLayout::ArrayFillParser(
                            Some((*parser, FuncLayoutKind::Parse)),
                        ))));
                    }
                    _ => {
                        dbpanic!(
                            ctx.db,
                            "Attempting to apply function to non-function layout {}",
                            &layout
                        );
                    }
                };
            let parserdef = pd.lookup(ctx.db)?;
            if parserdef.from.is_some() {
                Ok(ctx.dcx.intern(Layout::Mono(MonoLayout::NominalParser(
                    *pd,
                    present_args.clone(),
                    FuncLayoutKind::Parse,
                ))))
            } else {
                Ok(ctx.dcx.intern(Layout::Mono(MonoLayout::Nominal(
                    *pd,
                    None,
                    present_args.clone(),
                ))))
            }
        })
    }

    pub fn apply_fun(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        args: impl Iterator<Item = ILayout<'a>> + Clone,
    ) -> Result<ILayout<'a>, LayoutError> {
        let fun = self.evaluate(ctx)?.0;
        fun.try_map(ctx, |layout, ctx| {
            let evaluated_args = args
                .clone()
                .map(|arg| arg.evaluate(ctx).map(|x| x.0))
                .collect::<Result<Vec<_>, _>>()?;
            match layout.mono_layout() {
                MonoLayout::NominalParser(pd, present_args, FuncLayoutKind::Fun) => {
                    let present_args = present_args
                        .iter()
                        .chain(evaluated_args.iter())
                        .copied()
                        .collect();
                    let layout = MonoLayout::NominalParser(*pd, present_args, FuncLayoutKind::Fun);
                    let res = ctx.dcx.intern(Layout::Mono(layout));
                    Ok(res)
                }
                MonoLayout::Lambda(lambda_id, captures, present_args) => {
                    let present_args = present_args
                        .iter()
                        .chain(evaluated_args.iter())
                        .copied()
                        .collect();
                    let layout = MonoLayout::Lambda(*lambda_id, captures.clone(), present_args);
                    let res = ctx.dcx.intern(Layout::Mono(layout));
                    Ok(res)
                }
                MonoLayout::ArrayParser(args) => {
                    let new_args = match (args, evaluated_args.as_slice()) {
                        (None, []) => None,
                        (None, [parser]) | (Some((parser, None)), []) => Some((*parser, None)),
                        (None, [parser, arg])
                        | (Some((parser, None)), [arg])
                        | (Some((parser, Some((arg, _)))), []) => {
                            Some((*parser, Some((*arg, FuncLayoutKind::Fun))))
                        }
                        _ => panic!("Invalid number of arguments for array parser"),
                    };
                    let layout = MonoLayout::ArrayParser(new_args);
                    let res = ctx.dcx.intern(Layout::Mono(layout));
                    Ok(res)
                }
                MonoLayout::ArrayFillParser(arg) => {
                    let new_arg = match (arg, evaluated_args.as_slice()) {
                        (None, []) => None,
                        (None, [parser]) | (Some((parser, _)), []) => {
                            Some((*parser, FuncLayoutKind::Fun))
                        }
                        _ => panic!("Invalid number of arguments for array fill parser"),
                    };
                    let layout = MonoLayout::ArrayFillParser(new_arg);
                    let res = ctx.dcx.intern(Layout::Mono(layout));
                    Ok(res)
                }
                _ => dbpanic!(
                    ctx.db,
                    "Attempting to apply function to non-function layout {}",
                    &layout
                ),
            }
        })
    }

    pub fn access_field(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        field: FieldName,
    ) -> Result<ILayout<'a>, LayoutError> {
        let block = self.evaluate(ctx)?.0;
        block.try_map(ctx, |layout, ctx| {
            Ok(layout.0.map(ctx, |layout, _| match layout.mono_layout() {
                MonoLayout::Block(_, fields) => fields[&field],
                _ => panic!("Field access on non-block {layout:?}"),
            }))
        })
    }

    fn access_front(self, ctx: &mut AbsIntCtx<'a, ILayout<'a>>) -> ILayout<'a> {
        self.map(ctx, |layout, _| match layout.mono_layout() {
            MonoLayout::Nominal(_, from, _) => from
                .expect("Attempting to get 'front' field from non-from field containing nominal"),
            MonoLayout::IfParser(inner, _) => *inner,
            _ => dbpanic!(
                ctx.db,
                "Attempting to get 'front' field from non-nominal or if-parser {}",
                &self
            ),
        })
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
            Layout::Mono(MonoLayout::Block(id, fields)) => {
                self.block_manifestation(ctx, *id, fields)?.size
            }
            Layout::Mono(MonoLayout::BlockParser(_, captures, ..)) => {
                self.block_parser_manifestation(ctx, captures)?.size
            }
            Layout::Mono(MonoLayout::Lambda(id, captures, args)) => {
                self.lambda_manifestation(ctx, *id, captures, args)?.size
            }
            Layout::Mono(
                MonoLayout::IfParser(inner, _)
                | MonoLayout::ArrayFillParser(Some((inner, _)))
                | MonoLayout::ArrayParser(Some((inner, None))),
            ) => SizeAlign::ZST.cat(inner.size_align(ctx)?),
            Layout::Mono(
                MonoLayout::ArrayParser(Some((parser, Some((l, _)))))
                | MonoLayout::Array { parser, slice: l },
            ) => SizeAlign::ZST
                .cat(parser.size_align(ctx)?)
                .cat(l.size_align(ctx)?),
            Layout::Mono(MonoLayout::Nominal(id, from, args)) => {
                self.nominal_manifestation(ctx, *id, *from, args)?.size
            }
            Layout::Mono(MonoLayout::NominalParser(id, args, _)) => {
                self.nominal_parser_manifestation(ctx, *id, args)?.size
            }
            Layout::Mono(MonoLayout::SlicePtr) => <&Zst>::tsize(data).array(2),
            Layout::Mono(MonoLayout::Range) => i64::tsize(data).array(2),
            Layout::Mono(MonoLayout::Primitive(PrimitiveType::Bit)) => bool::tsize(data),
            Layout::Mono(MonoLayout::Primitive(PrimitiveType::Char)) => char::tsize(data),
            Layout::Mono(MonoLayout::Primitive(PrimitiveType::Int)) => i64::tsize(data),
            Layout::Mono(MonoLayout::Primitive(PrimitiveType::Unit)) => Zst::tsize(data),
            Layout::Mono(MonoLayout::Ptr) => <&Zst>::tsize(data),
            Layout::Mono(
                MonoLayout::Single
                | MonoLayout::Regex(_)
                | MonoLayout::ArrayParser(None)
                | MonoLayout::ArrayFillParser(None),
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
            manifest.add_field(Some(field_id), field_size);
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
            manifest.add_field(Some(*capture), sa);
        }
        let manifest = Arc::new(manifest.finalize(Default::default()));
        ctx.dcx.manifestations.insert(self, manifest.clone());
        Ok(manifest)
    }

    pub fn lambda_manifestation(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        id: hir::LambdaId,
        captures: &BTreeMap<DefId, ILayout<'a>>,
        args: &[ILayout<'a>],
    ) -> SResult<Arc<StructManifestation>> {
        if let Some(man) = ctx.dcx.manifestations.get(&self) {
            return Ok(man.clone());
        }
        let mut manifest = UnfinishedManifestation::new();
        for (capture, layout) in captures.iter() {
            let sa = layout.size_align(ctx)?;
            manifest.add_field(Some(*capture), sa);
        }
        let arg_ids = id.lookup(ctx.db)?.args;
        for (arg, id) in args.iter().zip(arg_ids.iter()) {
            let sa = arg.size_align(ctx)?;
            manifest.add_field(Some(id.0), sa);
        }
        let manifest = Arc::new(manifest.finalize(Default::default()));
        ctx.dcx.manifestations.insert(self, manifest.clone());
        Ok(manifest)
    }

    pub fn nominal_parser_manifestation(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        id: hir::ParserDefId,
        args: &[ILayout<'a>],
    ) -> SResult<Arc<StructManifestation>> {
        if let Some(man) = ctx.dcx.manifestations.get(&self) {
            return Ok(man.clone());
        }
        let mut manifest = UnfinishedManifestation::new();
        let arg_ids = id.lookup(ctx.db)?.args.unwrap_or_default();
        for (arg, id) in args.iter().zip(arg_ids.iter()) {
            let sa = arg.size_align(ctx)?;
            manifest.add_field(Some(id.0), sa);
        }
        let manifest = Arc::new(manifest.finalize(Default::default()));
        ctx.dcx.manifestations.insert(self, manifest.clone());
        Ok(manifest)
    }

    pub fn nominal_manifestation(
        self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        id: hir::ParserDefId,
        from: Option<ILayout<'a>>,
        args: &[ILayout<'a>],
    ) -> SResult<Arc<StructManifestation>> {
        if let Some(man) = ctx.dcx.manifestations.get(&self) {
            return Ok(man.clone());
        }
        let mut manifest = UnfinishedManifestation::new();
        let parserdef = id.lookup(ctx.db)?;
        if let Some(from) = from {
            let sa = from.size_align(ctx)?;
            manifest.add_field(Some(parserdef.from.unwrap().0), sa);
        }
        let arg_ids = id.lookup(ctx.db)?.args.unwrap_or_default();
        for (arg, id) in args.iter().zip(arg_ids.iter()) {
            let sa = arg.size_align(ctx)?;
            manifest.add_field(Some(id.0), sa);
        }
        let manifest = Arc::new(manifest.finalize(Default::default()));
        ctx.dcx.manifestations.insert(self, manifest.clone());
        Ok(manifest)
    }
}

pub fn pd_parser<'a, 'b>(
    ctx: &'b mut AbsIntCtx<'a, ILayout<'a>>,
    pd: ParserDefId,
) -> Result<ILayout<'a>, LayoutError> {
    let parserdef = pd.lookup(ctx.db)?;
    let kind = if parserdef.args.is_some() || parserdef.kind == DefKind::Static {
        FuncLayoutKind::Fun
    } else {
        FuncLayoutKind::Parse
    };

    Ok(ctx.dcx.intern(Layout::Mono(MonoLayout::NominalParser(
        pd,
        Default::default(),
        kind,
    ))))
}

pub fn canon_layout<'a, 'b>(
    ctx: &'b mut AbsIntCtx<'a, ILayout<'a>>,
    ty: TypeId,
) -> Result<ILayout<'a>, LayoutError> {
    let typ = ctx.db.lookup_intern_type(ty);
    let make_layout = |ctx: &'b mut AbsIntCtx<'a, ILayout<'a>>, x| ctx.dcx.intern(Layout::Mono(x));
    match typ {
        Type::Primitive(x) => Ok(make_layout(ctx, MonoLayout::Primitive(x))),
        Type::Loop(_, inner_ty) => {
            let inner_type = ctx.db.lookup_intern_type(inner_ty);
            match inner_type {
                Type::Primitive(PrimitiveType::Int) => Ok(make_layout(ctx, MonoLayout::SlicePtr)),
                _ => Err(LayoutError::LayoutError),
            }
        }
        Type::Block(..) | Type::ParserArg { .. } | Type::FunctionArg(_, _) => {
            Err(LayoutError::LayoutError)
        }
        Type::TypeVarRef(_) | Type::Unknown => Err(LayoutError::LayoutError),
    }
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

    pub fn int(&mut self) -> ILayout<'a> {
        self.intern(Layout::Mono(MonoLayout::Primitive(PrimitiveType::Int)))
    }

    pub fn primitive(&mut self, prim: PrimitiveType) -> ILayout<'a> {
        self.intern(Layout::Mono(MonoLayout::Primitive(prim)))
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
        fields: &FxHashMap<FieldName, Self>,
    ) -> Result<Self, Self::Err> {
        let new_layout = ctx.dcx.intern(Layout::Mono(MonoLayout::Block(
            id,
            fields.iter().map(|(a, b)| (*a, *b)).collect(),
        )));
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
            (Layout::Mono(_), Layout::Mono(_)) => vec![self, other],
            (Layout::Multi(first), Layout::Mono(_)) => match first.layouts.binary_search(&other) {
                Ok(_) => return Ok((self, false)),
                Err(i) => {
                    let mut new_layouts = first.layouts.clone();
                    new_layouts.insert(i, other);
                    new_layouts
                }
            },
            (Layout::Mono(_), Layout::Multi(second)) => match second.layouts.binary_search(&self) {
                Ok(_) => return Ok((other, true)),
                Err(i) => {
                    let mut new_layouts = second.layouts.clone();
                    new_layouts.insert(i, self);
                    new_layouts
                }
            },
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
        expr: ExprHead<Resolved, &Self>,
    ) -> Result<Self, Self::Err> {
        let mut make_layout = |x| ctx.dcx.intern(Layout::Mono(x));
        let res = match expr {
            ExprHead::Niladic(n) => match n {
                ResolvedAtom::Char(_) => make_layout(MonoLayout::Primitive(PrimitiveType::Char)),
                ResolvedAtom::Number(_) => make_layout(MonoLayout::Primitive(PrimitiveType::Int)),
                ResolvedAtom::Bool(_) => make_layout(MonoLayout::Primitive(PrimitiveType::Bit)),
                ResolvedAtom::Val(id) => ctx.var_by_id(id)?,
                ResolvedAtom::Single => make_layout(MonoLayout::Single),
                ResolvedAtom::Array => make_layout(MonoLayout::ArrayParser(None)),
                ResolvedAtom::ArrayFill => make_layout(MonoLayout::ArrayFillParser(None)),
                ResolvedAtom::Span(..) => {
                    ctx.active_ambience().expect("Span in non-parser context")
                }
                ResolvedAtom::String(_) => make_layout(MonoLayout::SlicePtr),
                ResolvedAtom::Regex(r) => make_layout(MonoLayout::Regex(r)),
                ResolvedAtom::ParserDef(pd) => {
                    let parserdef = pd.lookup(ctx.db)?;
                    let kind = if parserdef.args.is_some() || parserdef.kind == DefKind::Static {
                        FuncLayoutKind::Fun
                    } else {
                        FuncLayoutKind::Parse
                    };
                    make_layout(MonoLayout::NominalParser(pd, Vec::new(), kind))
                }
                ResolvedAtom::Global(pd) => ctx.dcx.globals[&pd].1,
                ResolvedAtom::Block(block_id, _) => {
                    let mut captures = BTreeMap::new();
                    let capture_ids = ctx.db.captures(block_id);
                    for capture in capture_ids.iter() {
                        // if the containing environment also captured the
                        // variable, we need to fetch it from the captures
                        // of the containing environment
                        let capture_value = ctx.active_env().get_captured(ctx, *capture)?;
                        let capture_value = match capture_value {
                            Some(x) => x,
                            // otherwise we get it from the current environment
                            None => ctx.var_by_id(*capture)?,
                        };
                        captures.insert(*capture, capture_value);
                    }
                    let res = ctx
                        .dcx
                        .intern(Layout::Mono(MonoLayout::BlockParser(block_id, captures)));
                    res
                }
                ResolvedAtom::Lambda(lambda_id) => {
                    let mut captures = BTreeMap::new();
                    let capture_ids = ctx.db.lambda_captures(lambda_id);
                    for capture in capture_ids.iter() {
                        let capture_value = ctx.active_env().get_captured(ctx, *capture)?;
                        let capture_value = match capture_value {
                            Some(x) => x,
                            // otherwise we get it from the current environment
                            None => ctx.var_by_id(*capture)?,
                        };
                        captures.insert(*capture, capture_value);
                    }
                    ctx.dcx.intern(Layout::Mono(MonoLayout::Lambda(
                        lambda_id,
                        captures,
                        Vec::new(),
                    )))
                }
                ResolvedAtom::Captured(capture) => {
                    ctx.active_env().get_captured(ctx, capture)?.unwrap()
                }
            },
            ExprHead::Monadic(op, inner) => match op {
                ValUnOp::Not | ValUnOp::Neg | ValUnOp::Size => {
                    make_layout(MonoLayout::Primitive(PrimitiveType::Int))
                }
                ValUnOp::Wiggle(cid, WiggleKind::Is) => {
                    let eval_inner = inner.evaluate(ctx)?.0;
                    ctx.dcx
                        .intern(Layout::Mono(MonoLayout::IfParser(eval_inner, cid)))
                }
                ValUnOp::Wiggle(_, WiggleKind::If | WiggleKind::Expect) => *inner,
                ValUnOp::Dot(a, ..) => inner.access_field(ctx, a)?,
                ValUnOp::EvalFun(_) => inner.eval_fun(ctx)?,
                ValUnOp::GetAddr => IMonoLayout::u8_array(ctx).inner(),
            },
            ExprHead::Dyadic(op, [lhs, rhs]) => match op {
                ValBinOp::ParserApply(_) => {
                    let arg = lhs.evaluate(ctx)?.0;
                    rhs.apply_arg(ctx, arg)?
                }
                ValBinOp::Else => lhs.join(ctx, *rhs)?.0,
                ValBinOp::Then => *rhs,
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
            ExprHead::Variadic(ValVarOp::PartialApply(_), inner) => {
                inner[0].apply_fun(ctx, inner[1..].iter().copied().copied())?
            }
        };
        Ok(res)
    }

    fn evaluate(self, ctx: &mut AbsIntCtx<'a, Self>) -> Result<(Self, bool), Self::Err> {
        let mut changed = false;
        let res = self.try_map(ctx, |layout, ctx| match layout.mono_layout() {
            MonoLayout::Ptr => {
                changed = true;
                Ok(ctx.dcx.int())
            }
            MonoLayout::Nominal(pd, _, _) => {
                changed = true;
                Ok(ctx
                    .eval_pd(layout.inner(), *pd)
                    .ok_or_else(SilencedError::new)?
                    .evaluate(ctx)?
                    .0)
            }
            _ => Ok(layout.inner()),
        });
        res.map(|x| (x, changed))
    }

    fn normalize(self, ctx: &mut AbsIntCtx<'a, Self>) -> Result<(Self, bool), Self::Err> {
        let mut changed = false;
        let res = self.try_map(ctx, |layout, ctx| match layout.mono_layout() {
            MonoLayout::Nominal(pd, _, _) => {
                if yaboc_hir::DefKind::Def == pd.lookup(ctx.db)?.kind {
                    return Ok(layout.inner());
                }
                changed = true;
                Ok(ctx
                    .eval_pd(layout.inner(), *pd)
                    .ok_or_else(SilencedError::new)?
                    .normalize(ctx)?
                    .0)
            }
            _ => Ok(layout.inner()),
        });
        res.map(|x| (x, changed))
    }

    fn get_arg(
        self,
        ctx: &mut AbsIntCtx<'a, Self>,
        arg: yaboc_absint::Arg,
    ) -> Result<Self, Self::Err> {
        self.try_map(ctx, |l, _| match l.mono_layout() {
            MonoLayout::Nominal(pd, from, args) => match arg {
                Arg::Named(def) => {
                    let idx = ctx
                        .db
                        .parserdef_arg_index(*pd, def)?
                        .expect("arg not found");
                    Ok(args[idx])
                }
                Arg::From => Ok(from.expect(&dbformat!(ctx.db, "{} did not have from arg", &l))),
            },
            _ => panic!("get_arg called on non-nominal layout"),
        })
    }

    fn make_thunk(
        ctx: &mut AbsIntCtx<'a, Self>,
        id: hir::ParserDefId,
        fields: &FxHashMap<Arg, Self>,
    ) -> Result<Self, Self::Err> {
        Ok(IMonoLayout::make_thunk(id, fields, ctx)?.inner())
    }

    fn bottom(ctx: &mut Self::DomainContext) -> Self {
        ctx.intern(Layout::None)
    }

    fn unit(ctx: &mut Self::DomainContext) -> Self {
        ctx.intern(Layout::Mono(MonoLayout::Primitive(PrimitiveType::Unit)))
    }
}

#[cfg(test)]
mod tests {
    use bumpalo::Bump;
    use yaboc_absint::AbsIntDatabase;
    use yaboc_ast::{AstDatabase, import::Import};
    use yaboc_base::dbeprintln;
    use yaboc_base::{
        Context, config::ConfigDatabase, interner::InternerDatabase, source::FileDatabase,
    };
    use yaboc_constraint::ConstraintDatabase;
    use yaboc_dependents::DependentsDatabase;
    use yaboc_hir::{HirDatabase, Parser};
    use yaboc_hir_types::HirTypesDatabase;
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
    def first = ~
    fun ~second = ~
    def main = {
        a: ~
        b: ~
        c: {
          case
          | let c = first
          | let c = second
          \
        }
        d: c.c
    }
            ",
        );
        let bump = Bump::new();
        let layout_ctx = LayoutContext::new(&bump, yaboc_target::layout::POINTER64);
        let mut outlayer = AbsIntCtx::<ILayout>::new(&ctx.db, layout_ctx);
        let main = ctx.parser("main");
        collected_layouts(&mut outlayer, &[main]).unwrap();
        let from = IMonoLayout::u8_array(&mut outlayer);
        let hash = outlayer.dcx.layout_hash(&ctx.db, from.inner());
        let canon_2004 = pd_parser(&mut outlayer, main)
            .unwrap()
            .apply_arg(&mut outlayer, from.inner())
            .unwrap();
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
                "main$466398b15b97e804$parse_9dcf97a184f32623_lb_worker"
            );
        }
        let main_block = outlayer.pd_result(&canon_2004).unwrap().returned;
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
                "block_6c872ebf06064930$ca296cd9ca93c0c5$parse_9dcf97a184f32623_vb"
            );
        }
        let field = |name| FieldName::Ident(ctx.id(name));
        assert_eq!(
            dbformat!(
                &ctx.db,
                "{}",
                &main_block.access_field(&mut outlayer, field("a")).unwrap(),
            ),
            "ptr"
        );
        assert_eq!(
            dbformat!(
                &ctx.db,
                "{}",
                &main_block.access_field(&mut outlayer, field("b")).unwrap(),
            ),
            "ptr"
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
            [
                "nominal-parser[file[_].second]() | nominal-parser[file[_].first]()",
                "nominal-parser[file[_].first]() | nominal-parser[file[_].second]()"
            ]
            .contains(&out.as_str())
        );
        let res = dbformat!(
            &ctx.db,
            "{}",
            &main_block.access_field(&mut outlayer, field("d")).unwrap(),
        );
        assert!(
            [
                "ptr | nominal[file[_].first](from: sliceptr)",
                "nominal[file[_].first](from: sliceptr) | ptr"
            ]
            .contains(&res.as_str())
        );
    }
    #[test]
    fn tailsize() {
        let ctx = Context::<LayoutTestDatabase>::mock(
            r"
export
def test = [5][3][2]
            ",
        );
        let bump = Bump::new();
        let layout_ctx = LayoutContext::new(&bump, yaboc_target::layout::POINTER64);
        let mut outlayer = AbsIntCtx::<ILayout>::new(&ctx.db, layout_ctx);
        let test = ctx.parser("test");
        let layouts = collected_layouts(&mut outlayer, &[test]).unwrap();
        let canon = pd_parser(&mut outlayer, test).unwrap();
        let slice = outlayer.dcx.intern(Layout::Mono(MonoLayout::SlicePtr));
        dbeprintln!(&ctx.db, "layout: {} -> {}", &slice, &canon);
        for ((from, to), sa) in layouts.tail_sa {
            dbeprintln!(&ctx.db, "tail: {} -> {}", &from, &to);
            eprintln!("{:?}", sa);
        }
    }
}
