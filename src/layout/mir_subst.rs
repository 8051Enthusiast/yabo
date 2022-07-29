use fxhash::FxHashMap;

use crate::{
    absint::{AbsIntCtx, AbstractDomain},
    error::SilencedError,
    expr::ExprIter,
    hir::{ExprId, HirIdWrapper, ParserDefId},
    interner::DefId,
    mir::{Function, PdArgKind, Place, PlaceOrigin, PlaceRef, StackRef},
    source::SpanIndex,
};

use super::{ILayout, IMonoLayout, InternerLayout, Layout, LayoutError, MonoLayout};

pub struct FunctionSubstitute<'a> {
    pub f: Function,
    stack_layouts: Vec<ILayout<'a>>,
    place_layouts: Vec<ILayout<'a>>,
}

impl<'a> FunctionSubstitute<'a> {
    pub fn new_from_block(
        f: Function,
        from: ILayout<'a>,
        block: IMonoLayout<'a>,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> Result<Self, LayoutError> {
        let (_, captures) = if let MonoLayout::BlockParser(def, captures) = block.mono_layout().0 {
            (def, captures)
        } else {
            panic!("non-block-parser as argument")
        };
        let evaluated = ctx.block_result()[&(from, block.0)]
            .as_ref()
            .ok_or(SilencedError)?;
        let mut expr_map = FxHashMap::default();
        for (id, expr) in evaluated.expr_vals.iter() {
            for r in ExprIter::new(&expr) {
                let (layout, span) = *r.0.root_data();
                expr_map.insert((*id, span), layout);
            }
        }
        let mut vals = evaluated.vals.clone();
        vals.extend(captures.iter());
        let sub_info = SubInfo {
            fun: block.0,
            arg: from,
            ret: evaluated.returned,
            expr: expr_map,
            vals,
        };
        let stack_layouts = sub_info.stack_layouts(&f);
        let place_layouts = sub_info.place_layouts(&f, &stack_layouts, ctx)?;
        Ok(FunctionSubstitute {
            f,
            stack_layouts,
            place_layouts,
        })
    }
    pub fn new_from_pd(
        f: Function,
        from: ILayout<'a>,
        fun: ILayout<'a>,
        pd: ParserDefId,
        arg_kind: PdArgKind,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> Result<Self, LayoutError> {
        let evaluated = ctx.pd_result()[&from].as_ref().ok_or(SilencedError)?;
        let expr_id = pd.lookup(ctx.db)?.to;
        let mut expr_map = FxHashMap::default();
        if let Some(expr) = evaluated.expr_vals.as_ref() {
            for r in ExprIter::new(&expr) {
                let (layout, span) = *r.0.root_data();
                expr_map.insert((expr_id, span), layout);
            }
        }
        let vals = FxHashMap::default();
        let ret = evaluated.returned;
        let (fun, from) = match arg_kind {
            PdArgKind::Parse => (fun, from),
            PdArgKind::Thunk => {
                let ty = f.place(f.cap()).ty;
                let thunk = ctx.dcx.intern.intern(InternerLayout {
                    layout: Layout::Mono(MonoLayout::Nominal(pd, Some(from)), ty),
                });
                let none = ctx.dcx.intern.intern(InternerLayout {
                    layout: Layout::None,
                });
                (thunk, none)
            }
        };
        let sub_info = SubInfo {
            fun,
            arg: from,
            ret,
            expr: expr_map,
            vals,
        };
        let stack_layouts = sub_info.stack_layouts(&f);
        let place_layouts = sub_info.place_layouts(&f, &stack_layouts, ctx)?;
        Ok(FunctionSubstitute {
            f,
            stack_layouts,
            place_layouts,
        })
    }
    pub fn place(&self, place: PlaceRef) -> ILayout<'a> {
        self.place_layouts[place.as_index()]
    }
    pub fn stack(&self, stack: StackRef) -> ILayout<'a> {
        self.stack_layouts[stack.as_index()]
    }
}

struct SubInfo<T> {
    fun: T,
    arg: T,
    ret: T,
    expr: FxHashMap<(ExprId, SpanIndex), T>,
    vals: FxHashMap<DefId, T>,
}

impl<'a> SubInfo<ILayout<'a>> {
    fn stack_layouts(&self, f: &Function) -> Vec<ILayout<'a>> {
        f.iter_stack()
            .map(|(_, place_origin)| match place_origin {
                PlaceOrigin::Node(id) => self.vals[&id],
                PlaceOrigin::Ambient(_, _) => self.ret,
                PlaceOrigin::Expr(e, idx) => self.expr[&(e, idx)],
            })
            .collect()
    }

    fn place_layouts(
        &self,
        f: &Function,
        stack_layouts: &[ILayout<'a>],
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> Result<Vec<ILayout<'a>>, LayoutError> {
        let mut place_layouts: Vec<ILayout<'a>> = Vec::new();
        for (_, place_info) in f.iter_places() {
            let layout = match place_info.place {
                Place::Captures => self.fun,
                Place::Arg => self.arg,
                Place::Return => self.ret,
                Place::Stack(idx) => stack_layouts[idx.as_index()],
                Place::Field(place, field) => {
                    let field = ctx
                        .db
                        .def_name(field)
                        .expect("accessing non-field id as field");
                    place_layouts[place.as_index()].access_field(ctx, field)
                }
                Place::DupleField(place, duple_field) => {
                    place_layouts[place.as_index()].access_duple(ctx, duple_field)
                }
                Place::From(place) => place_layouts[place.as_index()].access_from(ctx),
            };
            let cast_layout = layout.typecast(ctx, place_info.ty)?.0;
            place_layouts.push(cast_layout);
        }
        Ok(place_layouts)
    }
}
