mod represent;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};

use fxhash::{FxHashMap, FxHashSet};

use crate::absint::{AbsInt, AbsIntCtx, AbstractDomain, Arg};
use crate::error::{IsSilenced, SilencedError};
use crate::expr::{self, Atom, ExpressionHead, ValBinOp, ValUnOp};
use crate::hir;
use crate::hir_types::NominalId;
use crate::interner::{FieldName, HirId};
use crate::low_effort_interner::{Interner, Uniq};
use crate::order::expr::ResolvedAtom;
use crate::order::ResolvedExpr;
use crate::types::{PrimitiveType, Type, TypeId};

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
    Nominal(hir::ParserDefId, BTreeMap<Arg, Inner>),
    NominalParser(hir::ParserDefId),
    Block(hir::BlockId, BTreeMap<FieldName, Inner>),
    BlockParser(hir::BlockId, BTreeMap<HirId, Inner>),
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

#[derive(Clone, Copy)]
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
}

impl<'a> Uniq<InternerLayout<'a>> {
    fn flat_layout_list(&'a self) -> Vec<IMonoLayout<'a>> {
        match &self.layout {
            Layout::Mono(_, _) => vec![IMonoLayout(self)],
            Layout::Multi(l) => {
                let mut res = vec![];
                for l in &l.layouts {
                    match &l.layout {
                        Layout::Mono(_, _) => res.push(IMonoLayout(l)),
                        Layout::Multi(_) => panic!("MultiLayout inside MultiLayout not supported"),
                        Layout::None => panic!("Empty layout inside MultiLayout not supported"),
                    }
                }
                res
            }
            Layout::None => vec![],
        }
    }

    fn map(
        &'a self,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
        mut f: impl FnMut(IMonoLayout<'a>, &mut AbsIntCtx<'a, ILayout<'a>>) -> ILayout<'a>,
    ) -> ILayout<'a> {
        let mut acc = BTreeSet::new();
        let layouts = self.flat_layout_list();
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
            [] => ctx.dcx.intern(InternerLayout {
                layout: Layout::None,
            }),
            _ => ctx.dcx.intern(InternerLayout {
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
        let layouts = self.flat_layout_list();
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
            [] => ctx.dcx.intern(InternerLayout {
                layout: Layout::None,
            }),
            _ => ctx.dcx.intern(InternerLayout {
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

    fn get_captured(&'a self, id: HirId) -> Option<ILayout<'a>> {
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
                ctx.dcx.intern(InternerLayout {
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
                Type::ParserArg { result, arg  } => (result, arg),
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

    fn access_field(&'a self, ctx: &mut AbsIntCtx<'a, ILayout<'a>>, field: Atom) -> ILayout<'a> {
        self.map(ctx, |layout, _| match field {
            expr::Atom::Field(f) => match layout.mono_layout().0 {
                MonoLayout::Block(_, fields) => fields[&f],
                _ => panic!("Field access on non-block"),
            },
            expr::Atom::Number(_) | expr::Atom::Char(_) => panic!("Invalid field name"),
        })
    }
}

pub fn canon_layout<'a, 'b>(
    ctx: &'b mut AbsIntCtx<'a, ILayout<'a>>,
    ty: TypeId,
) -> Result<ILayout<'a>, LayoutError> {
    let typ = ctx.db.lookup_intern_type(ty);
    let make_layout = |ctx: &'b mut AbsIntCtx<'a, ILayout<'a>>, x| {
        ctx.dcx.intern(InternerLayout {
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
            let mut args = BTreeMap::new();
            if let Some(f) = from {
                args.insert(Arg::From, f);
            }
            Ok(make_layout(ctx, MonoLayout::Nominal(def_id, args)))
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

impl<'a> AbstractDomain<'a> for ILayout<'a> {
    type Err = LayoutError;
    type DomainContext = Interner<'a, InternerLayout<'a>>;

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
        Ok(ctx.dcx.intern(new_layout))
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
        Ok((ctx.dcx.intern(new_layout), true))
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
            ctx.dcx.intern(InternerLayout {
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
                    ctx.dcx.intern(InternerLayout {
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
            MonoLayout::Nominal(_, args) => &args[&arg],
            _ => panic!("get_arg called on non-nominal layout"),
        }))
    }

    fn make_thunk(
        ctx: &mut AbsIntCtx<'a, Self>,
        id: hir::ParserDefId,
        ty: TypeId,
        fields: &FxHashMap<Arg, Self>,
    ) -> Result<Self, Self::Err> {
        let new_layout = InternerLayout {
            layout: Layout::Mono(
                MonoLayout::Nominal(id, fields.iter().map(|(a, b)| (a.clone(), *b)).collect()),
                ty,
            ),
        };
        Ok(ctx.dcx.intern(new_layout))
    }

    fn bottom(ctx: &mut Self::DomainContext) -> Self {
        ctx.intern(InternerLayout {
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
        let mut outlayer = AbsIntCtx::<ILayout>::new(&ctx.db, intern);
        let main = ctx.parser("main");
        let main_ty = ctx.db.parser_args(main).unwrap().thunk;
        instantiate(&mut outlayer, &[main_ty]).unwrap();
        let canon_2006 = canon_layout(&mut outlayer, main_ty).unwrap();
        let main_block = outlayer.pd_result()[canon_2006].as_ref().unwrap().returned;
        let field = |name| Atom::Field(FieldName::Ident(ctx.id(name)));
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
