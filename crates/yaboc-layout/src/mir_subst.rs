use fxhash::FxHashMap;

use yaboc_absint::{AbsIntCtx, AbstractDomain, PdEvaluated};
use yaboc_base::{error::SilencedError, interner::DefId};
use yaboc_expr::{IndexExpr, ShapedData};
use yaboc_hir::{
    walk::ChildIter, ExprId, HirIdWrapper, HirNode, HirNodeKind, LambdaId, ParserDefId,
};
use yaboc_mir::{FunKind, Function, MirKind, Place, PlaceOrigin, PlaceRef, StackRef, Strictness};
use yaboc_resolve::expr::Resolved;
use yaboc_types::PrimitiveType;

use super::{ILayout, IMonoLayout, Layout, LayoutError, MonoLayout};

pub struct FunctionSubstitute<'a> {
    pub f: Function,
    pub stack_layouts: Vec<ILayout<'a>>,
    pub place_layouts: Vec<(ILayout<'a>, Strictness)>,
}

impl<'a> FunctionSubstitute<'a> {
    pub fn new_from_block(
        f: Function,
        strictness: &[Strictness],
        from: Option<ILayout<'a>>,
        block: IMonoLayout<'a>,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> Result<Self, LayoutError> {
        let (def, captures) = if let MonoLayout::BlockParser(def, captures, _) = block.mono_layout()
        {
            (def, captures)
        } else {
            panic!("non-block-parser as argument")
        };
        let evaluated = ctx
            .block_result(&(from, block.0))
            .ok_or_else(SilencedError::new)?;
        let mut expr_map = FxHashMap::default();
        for (id, expr) in evaluated.expr_vals.iter() {
            expr_map.insert(*id, expr.clone());
        }
        let ret = evaluated.returned;
        let mut vals = evaluated.vals.clone();
        vals.extend(captures.iter());
        let root_context = def.lookup(ctx.db)?.root_context.0;
        for node in ChildIter::new(root_context, ctx.db)
            .without_kinds(HirNodeKind::Block | HirNodeKind::Lambda)
        {
            if let HirNode::Choice(choice) = node {
                let int_layout = ctx
                    .dcx
                    .intern(Layout::Mono(MonoLayout::Primitive(PrimitiveType::Int)));
                vals.insert(choice.id.0, int_layout);
            }
        }
        vals.insert(def.0, ret);
        let sub_info = SubInfo {
            fun: block.0,
            arg: from,
            ret,
            int: ctx.dcx.int(),
            expr: expr_map,
            vals,
        };
        let mut stack_layouts = sub_info.stack_layouts(&f);
        let place_layouts = sub_info.place_layouts(&f, &mut stack_layouts, strictness, ctx)?;
        Ok(FunctionSubstitute {
            f,
            stack_layouts,
            place_layouts,
        })
    }

    fn new_from_fun(
        f: Function,
        strictness: &[Strictness],
        from: Option<ILayout<'a>>,
        fun: IMonoLayout<'a>,
        evaluated: PdEvaluated<ILayout<'a>>,
        expr_id: ExprId,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> Result<FunctionSubstitute<'a>, LayoutError> {
        let mut expr_map = FxHashMap::default();
        if let Some(expr) = evaluated.expr_vals.as_ref() {
            expr_map.insert(expr_id, expr.clone());
        }
        let vals = FxHashMap::default();
        let sub_info = SubInfo {
            fun: fun.inner(),
            arg: from,
            ret: evaluated.returned,
            int: ctx.dcx.int(),
            expr: expr_map,
            vals,
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
        from: Option<ILayout<'a>>,
        fun: IMonoLayout<'a>,
        pd: ParserDefId,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> Result<Self, LayoutError> {
        let MonoLayout::NominalParser(..) = fun.mono_layout() else {
            panic!("non-nominal-parser as argument")
        };
        let expr_id = pd.lookup(ctx.db)?.to;
        let lookup_layout = if let Some(from) = from {
            fun.inner().apply_arg(ctx, from)?
        } else {
            fun.inner().eval_fun(ctx)?
        };
        let evaluated = ctx
            .pd_result(&lookup_layout)
            .ok_or_else(SilencedError::new)?
            .clone();
        Self::new_from_fun(f, strictness, from, fun, evaluated, expr_id, ctx)
    }

    pub fn new_from_lambda(
        f: Function,
        strictness: &[Strictness],
        fun: IMonoLayout<'a>,
        lambda: LambdaId,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> Result<Self, LayoutError> {
        let MonoLayout::Lambda(..) = fun.mono_layout() else {
            panic!("non-lambda-parser as argument")
        };
        let expr_id = lambda.lookup(ctx.db)?.expr;
        let evaluated = ctx
            .lambda_result(&fun.0)
            .ok_or_else(SilencedError::new)?
            .clone();
        Self::new_from_fun(f, strictness, None, fun, evaluated.into(), expr_id, ctx)
    }

    pub fn new_from_if(
        f: Function,
        strictness: &[Strictness],
        from: ILayout<'a>,
        fun: IMonoLayout<'a>,
        ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
    ) -> Result<Self, LayoutError> {
        let MonoLayout::IfParser(inner, _, _) = fun.mono_layout() else {
            panic!("non-if-parser as argument")
        };
        let result = inner.apply_arg(ctx, from)?;
        let exprs = FxHashMap::default();
        let vals = FxHashMap::default();
        let sub_info = SubInfo {
            fun: fun.inner(),
            arg: Some(from),
            ret: result,
            expr: exprs,
            int: ctx.dcx.int(),
            vals,
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

    pub fn place_strictness(&self, place: PlaceRef) -> Strictness {
        self.place_layouts[place.as_index()].1
    }

    pub fn stack(&self, stack: StackRef) -> ILayout<'a> {
        self.stack_layouts[stack.as_index()]
    }
}

pub fn function_substitute<'a>(
    fun_info: FunKind,
    req: MirKind,
    from: Option<ILayout<'a>>,
    fun: IMonoLayout<'a>,
    ctx: &mut AbsIntCtx<'a, ILayout<'a>>,
) -> Result<FunctionSubstitute<'a>, LayoutError> {
    let mir = ctx.db.mir(fun_info, req)?;
    let strictness = ctx.db.strictness(fun_info, req)?;
    match fun_info {
        FunKind::Block(_) => FunctionSubstitute::new_from_block(mir, &strictness, from, fun, ctx),
        FunKind::ParserDef(pd) => {
            FunctionSubstitute::new_from_pd(mir, &strictness, from, fun, pd, ctx)
        }
        FunKind::Lambda(lambda_id) => {
            FunctionSubstitute::new_from_lambda(mir, &strictness, fun, lambda_id, ctx)
        }
        FunKind::If(_, _) => {
            FunctionSubstitute::new_from_if(mir, &strictness, from.unwrap(), fun, ctx)
        }
    }
}

#[derive(Debug)]
struct SubInfo<T> {
    fun: T,
    arg: Option<T>,
    ret: T,
    int: T,
    expr: FxHashMap<ExprId, ShapedData<Vec<T>, Resolved>>,
    vals: FxHashMap<DefId, T>,
}

impl<'intern> SubInfo<ILayout<'intern>> {
    fn stack_layouts(&self, f: &Function) -> Vec<ILayout<'intern>> {
        f.iter_stack()
            .map(|(_, place_origin)| match place_origin {
                PlaceOrigin::Node(id) => self.vals[&id],
                PlaceOrigin::Ambient(_, _) => self.arg.expect("used arg in non-parser"),
                PlaceOrigin::Expr(e, idx) => *self.expr[&e].index_expr(idx),
                PlaceOrigin::PolyLen => self.int,
                PlaceOrigin::Ret => self.ret,
                PlaceOrigin::Arg => self.arg.expect("used arg in non-parser"),
            })
            .collect()
    }

    fn place_layouts(
        &self,
        f: &Function,
        stack_layouts: &mut [ILayout<'intern>],
        strictness: &[Strictness],
        ctx: &mut AbsIntCtx<'intern, ILayout<'intern>>,
    ) -> Result<Vec<(ILayout<'intern>, Strictness)>, LayoutError> {
        let mut place_layouts: Vec<(ILayout<'intern>, Strictness)> = Vec::new();
        for (p, place_info) in f.iter_places() {
            let layout = match place_info.place {
                Place::Captures => self.fun,
                Place::Arg | Place::ReturnLen => self.arg.expect("used arg in non-parser"),
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
                Place::Front(place) => place_layouts[place.as_index()].0.access_front(ctx),
                Place::ModifiedBy(ins_ref) => match f.ins_at(ins_ref) {
                    yaboc_mir::MirInstr::ParseCall(_, _, _, front, _, _) => {
                        place_layouts[front.as_index()].0
                    }
                    _ => unreachable!(),
                },
                Place::Global(pd) => ctx.dcx.globals[&pd].1,
                Place::Undefined => ctx.dcx.intern(Layout::None),
            };

            let strict = strictness[p.as_index()];

            let cast_layout = if let Strictness::Strict = strict {
                layout.evaluate(ctx)?.0
            } else {
                layout
            };
            // we need to make sure that the stack layouts are casted to the right type, but
            // we didn't have that information when we created the stack layouts, so we cast
            // it if we encounter a stack place here
            if let Place::Stack(idx) = place_info.place {
                stack_layouts[idx.as_index()] = cast_layout;
            }
            place_layouts.push((cast_layout, strict));
        }
        Ok(place_layouts)
    }
}
