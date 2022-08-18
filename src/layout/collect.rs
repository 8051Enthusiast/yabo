use std::collections::hash_map::Entry;

use fxhash::{FxHashMap, FxHashSet};
use petgraph::unionfind::UnionFind;

use crate::{
    absint::{AbstractExpression, BlockEvaluated, PdEvaluated},
    error::SResult,
    expr::{Dyadic, ExprIter, ExpressionHead, OpWithData, ValBinOp},
    hir::{HirNode, ParserDefId},
    types::{PrimitiveType, Type},
};

use super::{
    canon_layout, flat_layouts, prop::PSize, AbsLayoutCtx, ILayout, IMonoLayout, InternerLayout,
    Layout, LayoutError, MonoLayout,
};

type LayoutSet<'a> = FxHashSet<IMonoLayout<'a>>;

pub struct LayoutCollection<'a> {
    pub arrays: LayoutSet<'a>,
    pub blocks: LayoutSet<'a>,
    pub nominals: LayoutSet<'a>,
    pub parsers: LayoutSet<'a>,
    pub primitives: LayoutSet<'a>,
    pub call_slots: FxHashMap<(ILayout<'a>, ILayout<'a>), PSize>,
    pub parser_occupied_entries: FxHashMap<IMonoLayout<'a>, FxHashMap<PSize, ILayout<'a>>>,
}

pub struct LayoutCollector<'a, 'b> {
    ctx: &'b mut AbsLayoutCtx<'a>,
    calls: CallInfo<'a>,
    arrays: LayoutSet<'a>,
    blocks: LayoutSet<'a>,
    nominals: LayoutSet<'a>,
    parsers: LayoutSet<'a>,
    block_calls: FxHashSet<(ILayout<'a>, IMonoLayout<'a>)>,
    unprocessed: Vec<UnprocessedCall<'a>>,
}

pub enum UnprocessedCall<'a> {
    Nominal(IMonoLayout<'a>),
    Block(ILayout<'a>, IMonoLayout<'a>),
}

impl<'a, 'b> LayoutCollector<'a, 'b> {
    pub fn new(ctx: &'b mut AbsLayoutCtx<'a>) -> Self {
        LayoutCollector {
            ctx,
            calls: Default::default(),
            arrays: Default::default(),
            blocks: Default::default(),
            nominals: Default::default(),
            parsers: Default::default(),
            block_calls: Default::default(),
            unprocessed: Default::default(),
        }
    }
    fn register_layouts(&mut self, layout: ILayout<'a>) {
        for mono in flat_layouts(&layout) {
            match &mono.mono_layout().0 {
                MonoLayout::Pointer => {
                    self.arrays.insert(mono);
                }
                MonoLayout::Nominal(_, _) => {
                    if self.nominals.insert(mono) {
                        self.unprocessed.push(UnprocessedCall::Nominal(mono));
                    }
                }
                MonoLayout::Block(_, _) => {
                    self.blocks.insert(mono);
                }
                MonoLayout::NominalParser(_)
                | MonoLayout::BlockParser(_, _)
                | MonoLayout::ComposedParser(_, _, _)
                | MonoLayout::Single => {
                    self.parsers.insert(mono);
                }
                MonoLayout::Primitive(_) => {}
            }
        }
    }
    fn register_call(&mut self, left: ILayout<'a>, right: ILayout<'a>) {
        for mono in flat_layouts(&right) {
            if let MonoLayout::BlockParser(_, _) = mono.mono_layout().0 {
                if self.block_calls.insert((left, mono)) {
                    self.unprocessed.push(UnprocessedCall::Block(left, mono));
                }
            }
        }
        self.calls.add_call(left, right)
    }
    fn collect_expr(&mut self, expr: &AbstractExpression<ILayout<'a>>) {
        for part in ExprIter::new(expr) {
            let dom = part.0.root_data().val;
            self.register_layouts(dom);
            if let ExpressionHead::Dyadic(Dyadic {
                op:
                    OpWithData {
                        inner: ValBinOp::ParserApply,
                        ..
                    },
                inner: [left, right],
            }) = &part.0
            {
                self.register_call(&left.0.root_data().val, &right.0.root_data().val)
            }
        }
    }
    fn collect_block(&mut self, block: &BlockEvaluated<ILayout<'a>>) -> SResult<()> {
        for expr in block.expr_vals.values() {
            self.collect_expr(expr);
        }
        for (&node, &value) in block.vals.iter() {
            if let HirNode::Parse(p) = self.ctx.db.hir_node(node)? {
                let expr_val = block.expr_vals[&p.expr].0.root_data().val;
                self.register_call(block.from, expr_val);
            }
            self.register_layouts(value)
        }
        Ok(())
    }
    fn collect_pd(&mut self, pd: &PdEvaluated<ILayout<'a>>) {
        if let Some(val) = &pd.expr_vals {
            self.collect_expr(val);
            let root = val.0.root_data().val;
            self.register_call(pd.from, root);
        }
        self.register_layouts(pd.returned)
    }
    fn proc_list(&mut self) -> SResult<()> {
        while let Some(mono) = self.unprocessed.pop() {
            match mono {
                UnprocessedCall::Nominal(mono) => {
                    if let Some(pd) = &self.ctx.pd_result()[&mono.0].clone() {
                        self.collect_pd(&pd)
                    }
                }
                UnprocessedCall::Block(arg, parser) => {
                    if let Some(block) = &self.ctx.block_result()[&(arg, parser.0)].clone() {
                        self.collect_block(block)?
                    }
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
                let parser_ty = self.ctx.db.intern_type(crate::types::Type::ParserArg {
                    result: thunk_ty,
                    arg: from,
                });
                let parser_layout = self.ctx.dcx.intern.intern(InternerLayout {
                    layout: Layout::Mono(MonoLayout::NominalParser(*pd), parser_ty),
                });
                self.register_layouts(parser_layout);
                self.register_call(from_layout, parser_layout);
            }
            self.register_layouts(thunk_layout);
        }
        self.proc_list()?;
        Ok(())
    }
    pub fn into_results(self) -> LayoutCollection<'a> {
        let call_slots = self.calls.into_layout_vtable_offsets();
        let mut parser_occupied_entries: FxHashMap<IMonoLayout, FxHashMap<PSize, ILayout>> =
            FxHashMap::default();
        for ((from, parsers), &slot) in call_slots.iter() {
            for parser in flat_layouts(&parsers) {
                parser_occupied_entries
                    .entry(parser)
                    .or_default()
                    .insert(slot, from);
            }
        }
        let mut primitives = FxHashSet::default();

        for x in [PrimitiveType::Int, PrimitiveType::Bit, PrimitiveType::Char] {
            let primitive_type = self.ctx.db.intern_type(Type::Primitive(x));
            let layout = self.ctx.dcx.intern.intern(InternerLayout {
                layout: Layout::Mono(MonoLayout::Primitive(x), primitive_type),
            });
            primitives.insert(IMonoLayout(layout));
        }
        LayoutCollection {
            arrays: self.arrays,
            blocks: self.blocks,
            nominals: self.nominals,
            parsers: self.parsers,
            primitives,
            call_slots,
            parser_occupied_entries,
        }
    }
}

#[derive(Default)]
pub struct CallInfo<'a> {
    map: FxHashMap<ILayout<'a>, FxHashSet<ILayout<'a>>>,
}

impl<'a> CallInfo<'a> {
    pub fn add_call(&mut self, arg: ILayout<'a>, parser: ILayout<'a>) {
        self.map.entry(arg).or_default().insert(parser);
    }
    pub fn into_layout_vtable_offsets(mut self) -> FxHashMap<(ILayout<'a>, ILayout<'a>), PSize> {
        let mut vecs = Vec::new();
        let mut id_info = FxHashMap::default();
        for (arg_layout, parser_set) in self.map.drain() {
            let mut pog = ParserOffsetGroups::new(parser_set);
            let mut layout_map = pog.layout_map();
            let mut sorted_vecs = pog.to_sets();
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
        let mut res: FxHashMap<(ILayout<'a>, ILayout<'a>), PSize> = FxHashMap::default();
        for (index, slot) in slot_sets.into_iter().enumerate() {
            for id in slot.contained_ids {
                let (arg, parsers) = match id_info.remove(&id) {
                    Some(x) => x,
                    None => continue,
                };
                for parser in parsers {
                    res.insert((arg, parser), index as PSize);
                }
            }
        }
        res
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
            let flat_id_iter = flat_layouts(&parser_layout).map(|x| self.mono_parsers[&x]);
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
    pub fn to_sets(&mut self) -> FxHashMap<u32, LayoutSet<'a>> {
        let mut res: FxHashMap<u32, LayoutSet> = FxHashMap::default();
        for (mono, index) in self.mono_parsers.iter() {
            let repr = self.union_find.find_mut(*index);
            res.entry(repr).or_default().insert(*mono);
        }
        res
    }
}
