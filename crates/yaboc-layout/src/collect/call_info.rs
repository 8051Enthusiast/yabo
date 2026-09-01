use super::LayoutSet;
use crate::AbsLayoutCtx;
use crate::ILayout;
use crate::IMonoLayout;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use petgraph::unionfind::UnionFind;
use std::cmp::Ordering;
use std::collections::hash_map::Entry;
use std::rc::Rc;
use yaboc_target::layout::PSize;

pub struct CallInfo<'a, Arg: std::hash::Hash + Eq + Copy> {
    pub(crate) map: FxHashMap<Arg, FxHashSet<ILayout<'a>>>,
}

impl<Arg: std::hash::Hash + Eq + Copy> Default for CallInfo<'_, Arg> {
    fn default() -> Self {
        Self {
            map: FxHashMap::default(),
        }
    }
}

#[derive(Debug)]
pub struct CallSlotResult<'a, Arg> {
    pub layout_vtable_offsets: FxHashMap<(Arg, ILayout<'a>), PSize>,
    pub(crate) call_args: FxHashMap<IMonoLayout<'a>, Rc<Vec<(Arg, Option<PSize>)>>>,
}

impl<'a, Arg: std::hash::Hash + Eq + Copy> CallSlotResult<'a, Arg> {
    pub fn calls_from_layout(&self, layout: IMonoLayout<'a>) -> Rc<Vec<(Arg, Option<PSize>)>> {
        self.call_args.get(&layout).cloned().unwrap_or_default()
    }
}

fn layout_set_hash<'a>(ctx: &mut AbsLayoutCtx<'a>, set: &LayoutSet<'a>) -> [u8; 32] {
    let sorted = super::LayoutCollector::sorted_layouts(ctx, set)
        .into_iter()
        .map(|x| x.inner())
        .collect::<Vec<_>>();
    ctx.dcx.full_layout_slice_hash(ctx.db, &sorted)
}

impl<'a, Arg: std::hash::Hash + Eq + Copy + std::fmt::Debug> CallInfo<'a, Arg> {
    pub fn add_call(&mut self, arg: Arg, parser: ILayout<'a>) {
        self.map.entry(arg).or_default().insert(parser);
    }

    fn get_unslotted_call_args(&self) -> FxHashMap<IMonoLayout<'a>, FxHashMap<Arg, Option<PSize>>> {
        let mut ret: FxHashMap<IMonoLayout<'a>, FxHashMap<Arg, Option<PSize>>> =
            FxHashMap::default();
        for (arg, layouts) in self.map.iter() {
            for layout in layouts.iter().flatten() {
                ret.entry(layout).or_default().insert(*arg, None);
            }
        }
        ret
    }

    pub fn into_layout_vtable_offsets(
        mut self,
        ctx: &mut AbsLayoutCtx<'a>,
        mut cmp: impl FnMut(&mut AbsLayoutCtx<'a>, &Arg, &Arg) -> Ordering,
    ) -> CallSlotResult<'a, Arg> {
        let mut sorted_vecs = Vec::new();
        let mut layout_set_hashes = Vec::new();
        let mut id_info = FxHashMap::default();
        let mut call_args = self.get_unslotted_call_args();
        for (arg_layout, parser_set) in self.map.drain() {
            let mut pog = ParserOffsetGroups::new(parser_set);
            let mut layout_map = pog.layout_map();
            let mut vecs = pog.get_sets();
            for (index, set) in vecs.drain() {
                let Some(parser_layouts) = layout_map.remove(&index) else {
                    continue;
                };
                id_info.insert(sorted_vecs.len(), (arg_layout, parser_layouts));
                layout_set_hashes.push(layout_set_hash(ctx, &set));
                sorted_vecs.push((set, sorted_vecs.len()));
            }
        }
        sorted_vecs.sort_unstable_by(|x, y| {
            x.0.len()
                .cmp(&y.0.len())
                .then_with(|| layout_set_hashes[x.1].cmp(&layout_set_hashes[y.1]))
                .then_with(|| cmp(ctx, &id_info[&x.1].0, &id_info[&y.1].0))
        });
        let mut slot_sets: Vec<ParserSlotStatus> = Vec::new();
        for vec in sorted_vecs.iter().rev() {
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
                let Some((arg, parsers)) = id_info.remove(&id) else {
                    continue;
                };
                for parser in parsers {
                    layout_vtable_offsets.insert((arg, parser), index as PSize);
                }
            }
        }
        for ((from, parsers), &slot) in layout_vtable_offsets.iter() {
            for parser in parsers {
                call_args
                    .entry(parser)
                    .or_default()
                    .insert(*from, Some(slot));
            }
        }
        let arc_call_args = call_args
            .into_iter()
            .map(|(k, v)| {
                let mut vec = v.into_iter().collect::<Vec<_>>();
                vec.sort_unstable_by(|(lhs, _), (rhs, _)| cmp(ctx, lhs, rhs));
                (k, Rc::new(vec))
            })
            .collect();
        CallSlotResult {
            layout_vtable_offsets,
            call_args: arc_call_args,
        }
    }
}

#[derive(Debug)]
pub(crate) struct ParserSlotStatus<'a> {
    pub(crate) used_parsers: LayoutSet<'a>,
    pub(crate) contained_ids: Vec<usize>,
}

impl<'a> ParserSlotStatus<'a> {
    pub(crate) fn new(used_parsers: LayoutSet<'a>, id: usize) -> Self {
        Self {
            used_parsers,
            contained_ids: vec![id],
        }
    }
    pub(crate) fn try_insert(&mut self, parser_set: &LayoutSet<'a>, id: usize) -> bool {
        if !self.used_parsers.is_disjoint(&parser_set) {
            return false;
        }
        self.used_parsers.extend(parser_set.iter());
        self.contained_ids.push(id);
        true
    }
}

pub(crate) struct ParserOffsetGroups<'a> {
    pub(crate) parser_set: FxHashSet<ILayout<'a>>,
    pub(crate) mono_parsers: FxHashMap<IMonoLayout<'a>, u32>,
    pub(crate) union_find: UnionFind<u32>,
}

impl<'a> ParserOffsetGroups<'a> {
    pub fn new(parser_set: FxHashSet<ILayout<'a>>) -> Self {
        let mut mono_parsers = FxHashMap::default();
        let mut current_index: u32 = 0;
        for layout in parser_set.iter() {
            if !layout.is_multi() {
                continue;
            }
            for mono_parser_layout in layout {
                if let Entry::Vacant(entry) = mono_parsers.entry(mono_parser_layout) {
                    entry.insert(current_index);
                    current_index = current_index
                        .checked_add(1)
                        .expect("overflowed number of mono layouts");
                }
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

    pub(crate) fn init_union_find(&mut self) {
        for parser_layout in self.parser_set.iter() {
            let flat_id_iter = parser_layout.into_iter().map(|x| self.mono_parsers[&x]);
            for (second, first) in flat_id_iter.clone().skip(1).zip(flat_id_iter) {
                self.union_find.union(first, second);
            }
        }
    }

    pub fn layout_map(&mut self) -> FxHashMap<u32, Vec<ILayout<'a>>> {
        let mut res: FxHashMap<u32, Vec<_>> = FxHashMap::default();
        for &layout in self.parser_set.iter() {
            if layout.is_multi()
                && let Some(x) = layout.into_iter().next()
            {
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
