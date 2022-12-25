use std::{collections::hash_map::Entry, sync::Arc};

use fxhash::{FxHashMap, FxHashSet};
use petgraph::unionfind::UnionFind;

use yaboc_absint::{AbstractDomain, AbstractExpression, Arg, BlockEvaluated};
use yaboc_ast::expr::{Dyadic, ExprIter, ExpressionHead, OpWithData, ValBinOp, ValVarOp, Variadic};
use yaboc_base::error::Silencable;
use yaboc_dependents::{NeededBy, RequirementSet};
use yaboc_hir::{HirIdWrapper, HirNode, ParserDefId};
use yaboc_types::{PrimitiveType, Type, TypeId};

use super::{
    canon_layout, flat_layouts, prop::PSize, AbsLayoutCtx, ILayout, IMonoLayout, Layout,
    LayoutError, MonoLayout,
};

type LayoutSet<'a> = FxHashSet<IMonoLayout<'a>>;

pub fn pd_len_req() -> RequirementSet {
    NeededBy::Len.into()
}
pub fn pd_val_req() -> RequirementSet {
    NeededBy::Val.into()
}
pub fn root_req() -> RequirementSet {
    NeededBy::Val | NeededBy::Len | NeededBy::Backtrack
}
pub fn apply_req() -> RequirementSet {
    NeededBy::Val | NeededBy::Backtrack
}

#[derive(Debug)]
pub struct LayoutCollection<'a> {
    pub root: Vec<(IMonoLayout<'a>, PSize)>,
    pub arrays: LayoutSet<'a>,
    pub blocks: LayoutSet<'a>,
    pub nominals: LayoutSet<'a>,
    pub parsers: LayoutSet<'a>,
    pub functions: LayoutSet<'a>,
    pub primitives: LayoutSet<'a>,
    pub parser_slots: CallSlotResult<'a, (ILayout<'a>, RequirementSet)>,
    pub funcall_slots: CallSlotResult<'a, ILayout<'a>>,
}

pub struct LayoutCollector<'a, 'b> {
    ctx: &'b mut AbsLayoutCtx<'a>,
    parses: CallInfo<'a, (ILayout<'a>, RequirementSet)>,
    funcalls: CallInfo<'a, ILayout<'a>>,
    arrays: LayoutSet<'a>,
    blocks: LayoutSet<'a>,
    nominals: LayoutSet<'a>,
    parsers: LayoutSet<'a>,
    functions: LayoutSet<'a>,
    root: Vec<(ILayout<'a>, IMonoLayout<'a>)>,
    processed_calls: FxHashSet<(ILayout<'a>, IMonoLayout<'a>, RequirementSet)>,
    unprocessed: Vec<UnprocessedCall<'a>>,
}

pub enum UnprocessedCall<'a> {
    NominalParser(ILayout<'a>, IMonoLayout<'a>, RequirementSet),
    Block(ILayout<'a>, IMonoLayout<'a>, RequirementSet),
}

impl<'a, 'b> LayoutCollector<'a, 'b> {
    pub fn new(ctx: &'b mut AbsLayoutCtx<'a>) -> Self {
        LayoutCollector {
            ctx,
            parses: Default::default(),
            funcalls: Default::default(),
            arrays: Default::default(),
            blocks: Default::default(),
            nominals: Default::default(),
            parsers: Default::default(),
            functions: Default::default(),
            processed_calls: Default::default(),
            unprocessed: Default::default(),
            root: Default::default(),
        }
    }
    fn register_layouts(&mut self, layout: ILayout<'a>) {
        for mono in flat_layouts(&layout) {
            match &mono.mono_layout().0 {
                MonoLayout::SlicePtr => {
                    self.arrays.insert(mono);
                }
                MonoLayout::Nominal(_, _, _) => {
                    if self.nominals.insert(mono) {
                        let (arg, parser) = mono.unapply_nominal(self.ctx);
                        // the original parser may have been backtracking, so we need to register
                        // the corresponding non-backtracking parser as well
                        self.register_layouts(parser);
                        self.register_parse(arg, parser, pd_val_req());
                        self.register_parse(arg, parser, pd_len_req());
                    }
                }
                MonoLayout::Block(_, _) => {
                    self.blocks.insert(mono);
                }
                MonoLayout::NominalParser(pd, args, bt) => {
                    let argnum = self.ctx.db.argnum(*pd).unwrap().unwrap_or(0);
                    if args.len() == argnum {
                        self.parsers.insert(mono);
                        if *bt {
                            let nbt_layout = mono.remove_backtracking(self.ctx);
                            self.parsers.insert(nbt_layout);
                        }
                    } else {
                        self.functions.insert(mono);
                        if *bt {
                            let nbt_layout = mono.remove_backtracking(self.ctx);
                            self.functions.insert(nbt_layout);
                        }
                    }
                }
                MonoLayout::BlockParser(_, _, _)
                | MonoLayout::ComposedParser(_, _, _, _)
                | MonoLayout::Single
                | MonoLayout::Nil => {
                    self.parsers.insert(mono);
                    self.parsers.insert(mono.remove_backtracking(self.ctx));
                }
                MonoLayout::Primitive(_) => {}
                MonoLayout::Tuple(_) => panic!("tuples not supported yet"),
            }
        }
    }
    fn register_parse(&mut self, left: ILayout<'a>, right: ILayout<'a>, req: RequirementSet) {
        if req.is_empty() {
            return;
        }
        for mono in flat_layouts(&right) {
            match mono.mono_layout().0 {
                MonoLayout::BlockParser(_, _, _) => {
                    if self.processed_calls.insert((left, mono, req)) {
                        self.unprocessed
                            .push(UnprocessedCall::Block(left, mono, req));
                    }
                }
                MonoLayout::NominalParser(_, _, _) => {
                    if self.processed_calls.insert((left, mono, req)) {
                        self.unprocessed
                            .push(UnprocessedCall::NominalParser(left, mono, req));
                    }
                }
                _ => {}
            }
        }
        self.parses.add_call((left, req), right)
    }
    fn register_funcall(
        &mut self,
        fun: ILayout<'a>,
        args: Vec<ILayout<'a>>,
        fun_ty: TypeId,
    ) -> Result<(), LayoutError> {
        let any_ty = self.ctx.db.intern_type(Type::Any);
        let Type::FunctionArg(_, arg_tys) = self.ctx.db.lookup_intern_type(fun_ty) else {
            panic!("register_funcall called with non-function type");
        };
        let casted_args = args
            .iter()
            .zip(arg_tys.iter())
            .map(|(l, ty)| Ok(l.typecast(self.ctx, *ty)?.0))
            .collect::<Result<_, LayoutError>>()?;
        // hack: this is just a placeholder for the argument bundle
        let arg_tuple = self
            .ctx
            .dcx
            .intern(Layout::Mono(MonoLayout::Tuple(casted_args), any_ty));
        self.funcalls.add_call(arg_tuple, fun);
        Ok(())
    }
    fn collect_expr(&mut self, expr: &AbstractExpression<ILayout<'a>>) -> Result<(), LayoutError> {
        for part in ExprIter::new(expr) {
            let dom = part.0.root_data().val;
            self.register_layouts(dom);
            match &part.0 {
                ExpressionHead::Dyadic(Dyadic {
                    op:
                        OpWithData {
                            inner: ValBinOp::ParserApply,
                            ..
                        },
                    inner: [left, right],
                }) => self.register_parse(
                    left.0.root_data().val,
                    right.0.root_data().val,
                    apply_req(),
                ),
                ExpressionHead::Variadic(Variadic {
                    op:
                        OpWithData {
                            inner: ValVarOp::Call,
                            ..
                        },
                    inner: args,
                }) => self.register_funcall(
                    args[0].0.root_data().val,
                    args[1..].iter().map(|x| x.0.root_data().val).collect(),
                    args[0].0.root_data().ty,
                )?,
                _ => (),
            }
        }
        Ok(())
    }
    fn collect_block(
        &mut self,
        block: &BlockEvaluated<ILayout<'a>>,
        req: RequirementSet,
    ) -> Result<(), LayoutError> {
        for expr in block.expr_vals.values() {
            self.collect_expr(expr)?;
        }
        let requirements = self
            .ctx
            .db
            .block_serialization(block.id)
            .silence()?
            .parse_requirements;
        for (&node, &value) in block.vals.iter() {
            if let HirNode::Parse(p) = self.ctx.db.hir_node(node)? {
                let expr_val = block.expr_vals[&p.expr].0.root_data().val;
                let parse_req = requirements[&p.id.0] * req;
                self.register_parse(block.from, expr_val, parse_req);
            }
            self.register_layouts(value)
        }
        Ok(())
    }
    fn collect_nominal_parse(
        &mut self,
        arg: ILayout<'a>,
        parser: IMonoLayout<'a>,
        mut req: RequirementSet,
    ) -> Result<(), LayoutError> {
        let (MonoLayout::NominalParser(id, thunk_args, bt), ty) = parser.mono_layout() else {
            panic!("unexpected non-nominal-parser layout");
        };
        if !*bt {
            req &= !NeededBy::Backtrack
        }
        let mut args = FxHashMap::default();
        let thunk_ty = self.ctx.db.parser_result(ty).unwrap();
        args.insert(Arg::From, arg);
        let arg_ids = id.lookup(self.ctx.db).unwrap().args.unwrap_or_default();
        for (id, arg) in arg_ids.iter().zip(thunk_args.iter()) {
            args.insert(Arg::Named(id.0), arg.0);
        }
        let thunk = ILayout::make_thunk(self.ctx, *id, thunk_ty, &args).unwrap();
        self.register_layouts(thunk);
        if let Some(pd_eval) = self.ctx.pd_result()[&thunk].clone() {
            if let Some(val) = &pd_eval.expr_vals {
                self.collect_expr(val)?;
                let root = val.0.root_data().val;
                self.register_parse(pd_eval.from, root, req);
                // the branch taken after the thunk is copied as-is and no value is needed
                self.register_parse(pd_eval.from, root, req & !NeededBy::Val);
            }
            self.register_layouts(pd_eval.returned)
        }
        Ok(())
    }
    fn proc_list(&mut self) -> Result<(), LayoutError> {
        while let Some(mono) = self.unprocessed.pop() {
            match mono {
                UnprocessedCall::Block(arg, parser, mut req) => {
                    let MonoLayout::BlockParser(_, _, bt) = parser.mono_layout().0 else {
                        panic!("unexpected non-block-parser layout");
                    };
                    if !*bt {
                        req &= !NeededBy::Backtrack
                    }
                    if let Some(block) = &self.ctx.block_result()[&(arg, parser.0)].clone() {
                        self.collect_block(block, req)?
                    }
                }
                UnprocessedCall::NominalParser(arg, parser, req) => {
                    self.collect_nominal_parse(arg, parser, req)?
                }
            }
        }
        Ok(())
    }
    pub fn collect(&mut self, pds: &[ParserDefId]) -> Result<(), LayoutError> {
        for pd in pds {
            let sig = self.ctx.db.parser_args(*pd)?;
            let thunk_ty = sig.thunk;
            let thunk_layout = canon_layout(self.ctx, thunk_ty)?;
            if let Some(from) = sig.from {
                let from_layout = canon_layout(self.ctx, from)?;
                self.register_layouts(from_layout);
                let parser_ty = self.ctx.db.intern_type(yaboc_types::Type::ParserArg {
                    result: thunk_ty,
                    arg: from,
                });
                let parser_layout = self.ctx.dcx.intern(Layout::Mono(
                    MonoLayout::NominalParser(*pd, Default::default(), true),
                    parser_ty,
                ));
                self.register_layouts(parser_layout);
                for mono in flat_layouts(&parser_layout) {
                    self.root.push((from_layout, mono));
                }
                self.register_parse(from_layout, parser_layout, root_req());
            }
            self.register_layouts(thunk_layout);
        }
        self.proc_list()?;
        Ok(())
    }
    pub fn into_results(self) -> LayoutCollection<'a> {
        let parser_slots = self.parses.into_layout_vtable_offsets();
        let funcall_slots = self.funcalls.into_layout_vtable_offsets();
        let mut primitives = FxHashSet::default();

        for x in [
            PrimitiveType::Int,
            PrimitiveType::Bit,
            PrimitiveType::Char,
            PrimitiveType::Unit,
        ] {
            let primitive_type = self.ctx.db.intern_type(Type::Primitive(x));
            let layout = self
                .ctx
                .dcx
                .intern(Layout::Mono(MonoLayout::Primitive(x), primitive_type));
            primitives.insert(IMonoLayout(layout));
        }

        let root = self
            .root
            .into_iter()
            .map(|(from, mono)| {
                let key = ((from, root_req()), mono.0);
                let slot = parser_slots.layout_vtable_offsets[&key];
                (mono, slot)
            })
            .collect();

        LayoutCollection {
            root,
            arrays: self.arrays,
            blocks: self.blocks,
            nominals: self.nominals,
            parsers: self.parsers,
            functions: self.functions,
            primitives,
            parser_slots,
            funcall_slots,
        }
    }
}

pub struct CallInfo<'a, Arg: std::hash::Hash + Eq + Copy> {
    map: FxHashMap<Arg, FxHashSet<ILayout<'a>>>,
}

impl<'a, Arg: std::hash::Hash + Eq + Copy> Default for CallInfo<'a, Arg> {
    fn default() -> Self {
        Self {
            map: FxHashMap::default(),
        }
    }
}

#[derive(Debug)]
pub struct CallSlotResult<'a, Arg> {
    pub layout_vtable_offsets: FxHashMap<(Arg, ILayout<'a>), PSize>,
    pub occupied_entries: FxHashMap<IMonoLayout<'a>, Arc<FxHashMap<PSize, Arg>>>,
}

impl<'a, Arg: std::hash::Hash + Eq + Copy> CallInfo<'a, Arg> {
    pub fn add_call(&mut self, arg: Arg, parser: ILayout<'a>) {
        self.map.entry(arg).or_default().insert(parser);
    }
    pub fn into_layout_vtable_offsets(mut self) -> CallSlotResult<'a, Arg> {
        let mut vecs = Vec::new();
        let mut id_info = FxHashMap::default();
        for (arg_layout, parser_set) in self.map.drain() {
            let mut pog = ParserOffsetGroups::new(parser_set);
            let mut layout_map = pog.layout_map();
            let mut sorted_vecs = pog.get_sets();
            for (index, set) in sorted_vecs.drain() {
                let parser_layouts = match layout_map.remove(&index) {
                    Some(x) => x,
                    None => continue,
                };
                id_info.insert(vecs.len(), (arg_layout, parser_layouts));
                vecs.push((set, vecs.len()));
            }
        }
        vecs.sort_unstable_by_key(|x| x.0.len());
        let mut slot_sets: Vec<ParserSlotStatus> = Vec::new();
        for vec in vecs.iter().rev() {
            if !slot_sets
                .iter_mut()
                .any(|present| present.try_insert(&vec.0, vec.1))
            {
                slot_sets.push(ParserSlotStatus::new(vec.0.clone(), vec.1))
            }
        }
        let mut layout_vtable_offsets: FxHashMap<(Arg, ILayout<'a>), PSize> = FxHashMap::default();
        for (index, slot) in slot_sets.into_iter().enumerate() {
            for id in slot.contained_ids {
                let (arg, parsers) = match id_info.remove(&id) {
                    Some(x) => x,
                    None => continue,
                };
                for parser in parsers {
                    layout_vtable_offsets.insert((arg, parser), index as PSize);
                }
            }
        }
        let mut parser_occupied_entries: FxHashMap<IMonoLayout, FxHashMap<PSize, Arg>> =
            FxHashMap::default();
        for ((from, parsers), &slot) in layout_vtable_offsets.iter() {
            for parser in flat_layouts(parsers) {
                parser_occupied_entries
                    .entry(parser)
                    .or_default()
                    .insert(slot, *from);
            }
        }
        let arc_parser_occupied_entries = parser_occupied_entries
            .into_iter()
            .map(|(k, v)| (k, Arc::new(v)))
            .collect();
        CallSlotResult {
            layout_vtable_offsets,
            occupied_entries: arc_parser_occupied_entries,
        }
    }
}

struct ParserSlotStatus<'a> {
    used_parsers: LayoutSet<'a>,
    contained_ids: Vec<usize>,
}

impl<'a> ParserSlotStatus<'a> {
    fn new(used_parsers: LayoutSet<'a>, id: usize) -> Self {
        Self {
            used_parsers,
            contained_ids: vec![id],
        }
    }
    fn try_insert(&mut self, parser_set: &LayoutSet<'a>, id: usize) -> bool {
        if !is_disjoint(&self.used_parsers, parser_set) {
            return false;
        }
        self.used_parsers.extend(parser_set.iter());
        self.contained_ids.push(id);
        true
    }
}

fn is_disjoint<'a, T: std::hash::Hash + Eq>(
    mut a: &'a FxHashSet<T>,
    mut b: &'a FxHashSet<T>,
) -> bool {
    if a.len() > b.len() {
        std::mem::swap(&mut a, &mut b);
    }
    for element in a.iter() {
        if b.contains(element) {
            return false;
        }
    }
    true
}

struct ParserOffsetGroups<'a> {
    parser_set: FxHashSet<ILayout<'a>>,
    mono_parsers: FxHashMap<IMonoLayout<'a>, u32>,
    union_find: UnionFind<u32>,
}

impl<'a> ParserOffsetGroups<'a> {
    pub fn new(parser_set: FxHashSet<ILayout<'a>>) -> Self {
        let mut mono_parsers = FxHashMap::default();
        let mut current_index: u32 = 0;
        for mono_parser_layout in parser_set.iter().flat_map(flat_layouts) {
            if let Entry::Vacant(entry) = mono_parsers.entry(mono_parser_layout) {
                entry.insert(current_index);
                current_index = current_index
                    .checked_add(1)
                    .expect("overflowed number of mono layouts");
            }
        }
        let union_find = UnionFind::new(current_index as usize);
        let mut res = ParserOffsetGroups {
            parser_set,
            mono_parsers,
            union_find,
        };
        res.init_union_find();
        res
    }
    fn init_union_find(&mut self) {
        for parser_layout in self.parser_set.iter() {
            let flat_id_iter = flat_layouts(parser_layout).map(|x| self.mono_parsers[&x]);
            for (second, first) in flat_id_iter.clone().skip(1).zip(flat_id_iter) {
                self.union_find.union(first, second);
            }
        }
    }
    pub fn layout_map(&mut self) -> FxHashMap<u32, Vec<ILayout<'a>>> {
        let mut res: FxHashMap<u32, Vec<_>> = FxHashMap::default();
        for &layout in self.parser_set.iter() {
            if let Some(x) = flat_layouts(&layout).next() {
                let layout_index = self.union_find.find_mut(self.mono_parsers[&x]);
                res.entry(layout_index).or_default().push(layout);
            }
        }
        res
    }
    pub fn get_sets(&mut self) -> FxHashMap<u32, LayoutSet<'a>> {
        let mut res: FxHashMap<u32, LayoutSet> = FxHashMap::default();
        for (mono, index) in self.mono_parsers.iter() {
            let repr = self.union_find.find_mut(*index);
            res.entry(repr).or_default().insert(*mono);
        }
        res
    }
}
