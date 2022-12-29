use std::sync::Arc;

use fxhash::FxHashMap;

use yaboc_absint::{AbsIntCtx, AbstractExprInfo};
use yaboc_ast::expr::ExprIter;
use yaboc_base::{error::SilencedError, interner::DefId, source::SpanIndex};
use yaboc_hir::{walk::ChildIter, ExprId, HirIdWrapper, HirNode, HirNodeKind, ParserDefId};
use yaboc_mir::{Function, Place, PlaceOrigin, PlaceRef, StackRef, Strictness};
use yaboc_types::{PrimitiveType, Type, TypeId};

use super::{ILayout, IMonoLayout, Layout, LayoutError, MonoLayout};

pub struct FunctionSubstitute<'a> {
    pub f: Function,
    pub stack_layouts: Vec<ILayout<'a>>,
    pub place_layouts: Vec<(ILayout<'a>, TypeId, Strictness)>,
}

impl<'a> FunctionSubstitute<'a> {
    pub fn new_from_block(
        f: Function,
        strictness: &[Strictness],
        from: ILayout<'a>,
        block: IMonoLayout<'a>,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> Result<Self, LayoutError> {
        let (def, captures) =
            if let MonoLayout::BlockParser(def, captures, _) = block.mono_layout().0 {
                (def, captures)
            } else {
                panic!("non-block-parser as argument")
            };
        let evaluated = ctx.block_result()[&(from, block.0)]
            .as_ref()
            .ok_or_else(SilencedError::new)?;
        let subst = Some(evaluated.typesubst.clone());
        let mut expr_map = FxHashMap::default();
        for (id, expr) in evaluated.expr_vals.iter() {
            for r in ExprIter::new(expr) {
                let AbstractExprInfo { val, span, .. } = *r.0.root_data();
                expr_map.insert((*id, span), val);
            }
        }
        let ret = evaluated.returned;
        let mut vals = evaluated.vals.clone();
        vals.extend(captures.iter());
        let root_context = def.lookup(ctx.db)?.root_context.0;
        for node in ChildIter::new(root_context, ctx.db).without_kinds(HirNodeKind::Block) {
            if let HirNode::Choice(choice) = node {
                let int_ty = ctx.db.intern_type(Type::Primitive(PrimitiveType::Int));
                let int_layout = ctx.dcx.intern(Layout::Mono(
                    MonoLayout::Primitive(PrimitiveType::Int),
                    int_ty,
                ));
                vals.insert(choice.id.0, int_layout);
            }
        }
        vals.insert(def.0, ret);
        let sub_info = SubInfo {
            fun: block.0,
            arg: from,
            ret,
            expr: expr_map,
            vals,
            subst,
        };
        let mut stack_layouts = sub_info.stack_layouts(&f);
        let place_layouts = sub_info.place_layouts(&f, &mut stack_layouts, strictness, ctx)?;
        Ok(FunctionSubstitute {
            f,
            stack_layouts,
            place_layouts,
        })
    }

    pub fn new_from_pd(
        f: Function,
        strictness: &[Strictness],
        from: ILayout<'a>,
        fun: ILayout<'a>,
        pd: ParserDefId,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> Result<Self, LayoutError> {
        let lookup_layout = fun.apply_arg(ctx, from)?;
        let evaluated = ctx.pd_result()[&lookup_layout]
            .as_ref()
            .ok_or_else(SilencedError::new)?;
        let subst = Some(evaluated.typesubst.clone());
        let expr_id = pd.lookup(ctx.db)?.to;
        let mut expr_map = FxHashMap::default();
        if let Some(expr) = evaluated.expr_vals.as_ref() {
            for r in ExprIter::new(expr) {
                let AbstractExprInfo { val, span, .. } = *r.0.root_data();
                expr_map.insert((expr_id, span), val);
            }
        }
        let vals = FxHashMap::default();
        let sub_info = SubInfo {
            fun,
            arg: from,
            ret: evaluated.returned,
            expr: expr_map,
            vals,
            subst,
        };
        let mut stack_layouts = sub_info.stack_layouts(&f);
        let place_layouts = sub_info.place_layouts(&f, &mut stack_layouts, strictness, ctx)?;
        Ok(FunctionSubstitute {
            f,
            stack_layouts,
            place_layouts,
        })
    }

    pub fn new_from_if(
        f: Function,
        strictness: &[Strictness],
        from: ILayout<'a>,
        fun: IMonoLayout<'a>,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> Result<Self, LayoutError> {
        let MonoLayout::IfParser(inner, _, _) = fun.mono_layout().0 else {
            panic!("non-if-parser as argument")
        };
        let result = inner.apply_arg(ctx, from)?;
        let exprs = FxHashMap::default();
        let vals = FxHashMap::default();
        let sub_info = SubInfo {
            fun: fun.inner(),
            arg: from,
            ret: result,
            expr: exprs,
            vals,
            subst: None,
        };
        let mut stack_layouts = sub_info.stack_layouts(&f);
        let place_layouts = sub_info.place_layouts(&f, &mut stack_layouts, strictness, ctx)?;
        Ok(FunctionSubstitute {
            f,
            stack_layouts,
            place_layouts,
        })
    }

    pub fn place(&self, place: PlaceRef) -> ILayout<'a> {
        self.place_layouts[place.as_index()].0
    }

    pub fn place_type(&self, place: PlaceRef) -> TypeId {
        self.place_layouts[place.as_index()].1
    }

    pub fn place_strictness(&self, place: PlaceRef) -> Strictness {
        self.place_layouts[place.as_index()].2
    }

    pub fn stack(&self, stack: StackRef) -> ILayout<'a> {
        self.stack_layouts[stack.as_index()]
    }
}

#[derive(Debug)]
struct SubInfo<T> {
    fun: T,
    arg: T,
    ret: T,
    expr: FxHashMap<(ExprId, SpanIndex), T>,
    vals: FxHashMap<DefId, T>,
    subst: Option<Arc<Vec<TypeId>>>,
}

impl<'a> SubInfo<ILayout<'a>> {
    fn stack_layouts(&self, f: &Function) -> Vec<ILayout<'a>> {
        f.iter_stack()
            .map(|(_, place_origin)| match place_origin {
                PlaceOrigin::Node(id) => self.vals[&id],
                PlaceOrigin::Ambient(_, _) => self.arg,
                PlaceOrigin::Expr(e, idx) => self.expr[&(e, idx)],
                PlaceOrigin::Ret => self.ret,
                PlaceOrigin::Arg => self.arg,
            })
            .collect()
    }

    fn place_layouts(
        &self,
        f: &Function,
        stack_layouts: &mut [ILayout<'a>],
        strictness: &[Strictness],
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> Result<Vec<(ILayout<'a>, TypeId, Strictness)>, LayoutError> {
        let mut place_layouts: Vec<(ILayout<'a>, TypeId, Strictness)> = Vec::new();
        for (p, place_info) in f.iter_places() {
            let layout = match place_info.place {
                Place::Captures => self.fun,
                Place::Arg | Place::ReturnLen => self.arg,
                Place::Return => self.ret,
                Place::Stack(idx) => stack_layouts[idx.as_index()],
                Place::Field(place, field) => {
                    let field_name = ctx
                        .db
                        .def_name(field)
                        .expect("accessing non-field id as field");
                    place_layouts[place.as_index()]
                        .0
                        .access_field(ctx, field_name)?
                }
                Place::Captured(place, field) => place_layouts[place.as_index()]
                    .0
                    .get_captured(ctx, field)?
                    .unwrap(),
                Place::DupleField(place, duple_field) => place_layouts[place.as_index()]
                    .0
                    .access_duple(ctx, duple_field),
                Place::Front(place) => place_layouts[place.as_index()].0.access_front(ctx),
                Place::ModifiedBy(ins_ref) => match f.ins_at(ins_ref) {
                    yaboc_mir::MirInstr::ParseCall(_, _, _, front, _, _) => {
                        place_layouts[front.as_index()].0
                    }
                    _ => unreachable!(),
                },
            };

            // we don't want any type variables to appear here in general because we want a defined
            // deref level, but for the case of if-parsers, type variables cannot actually occur in a
            // position that would be relevant for the deref level, so we can just ignore them
            let ty = self
                .subst
                .as_ref()
                .map(|subst| ctx.db.substitute_typevar(place_info.ty, subst.clone()))
                .unwrap_or(place_info.ty);

            let strict = if let Type::TypeVarRef(..) = ctx.db.lookup_intern_type(place_info.ty) {
                Strictness::Static(ctx.db.deref_level(ty)?)
            } else {
                strictness[p.as_index()]
            };

            let cast_layout = if let Strictness::Static(level) = strict {
                layout.deref_to_level(ctx, level)?.0
            } else {
                layout
            };
            // we need to make sure that the stack layouts are casted to the right type, but
            // we didn't have that information when we created the stack layouts, so we cast
            // it if we encounter a stack place here
            if let Place::Stack(idx) = place_info.place {
                stack_layouts[idx.as_index()] = cast_layout;
            }
            place_layouts.push((cast_layout, ty, strict));
        }
        Ok(place_layouts)
    }
}
