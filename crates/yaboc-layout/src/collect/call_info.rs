use super::LayoutSet;
use crate::ILayout;
use crate::IMonoLayout;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use petgraph::unionfind::UnionFind;
use std::collections::hash_map::Entry;
use std::rc::Rc;
use yaboc_target::layout::PSize;

pub struct CallInfo<'a, Arg: std::hash::Hash + Eq + Copy> {
    pub(crate) map: FxHashMap<Arg, FxHashSet<ILayout<'a>>>,
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
    pub occupied_entries: FxHashMap<IMonoLayout<'a>, Rc<FxHashMap<PSize, Arg>>>,
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
                let Some(parser_layouts) = layout_map.remove(&index) else {
                    continue;
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
                let Some((arg, parsers)) = id_info.remove(&id) else {
                    continue;
                };
                for parser in parsers {
                    layout_vtable_offsets.insert((arg, parser), index as PSize);
                }
            }
        }
        let mut parser_occupied_entries: FxHashMap<IMonoLayout, FxHashMap<PSize, Arg>> =
            FxHashMap::default();
        for ((from, parsers), &slot) in layout_vtable_offsets.iter() {
            for parser in parsers {
                parser_occupied_entries
                    .entry(parser)
                    .or_default()
                    .insert(slot, *from);
            }
        }
        let arc_parser_occupied_entries = parser_occupied_entries
            .into_iter()
            .map(|(k, v)| (k, Rc::new(v)))
            .collect();
        CallSlotResult {
            layout_vtable_offsets,
            occupied_entries: arc_parser_occupied_entries,
        }
    }
}

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
        if !is_disjoint(&self.used_parsers, parser_set) {
            return false;
        }
        self.used_parsers.extend(parser_set.iter());
        self.contained_ids.push(id);
        true
    }
}

pub(crate) fn is_disjoint<'a, T: std::hash::Hash + Eq>(
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

pub(crate) struct ParserOffsetGroups<'a> {
    pub(crate) parser_set: FxHashSet<ILayout<'a>>,
    pub(crate) mono_parsers: FxHashMap<IMonoLayout<'a>, u32>,
    pub(crate) union_find: UnionFind<u32>,
}

impl<'a> ParserOffsetGroups<'a> {
    pub fn new(parser_set: FxHashSet<ILayout<'a>>) -> Self {
        let mut mono_parsers = FxHashMap::default();
        let mut current_index: u32 = 0;
        for mono_parser_layout in parser_set.iter().flatten() {
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
            if let Some(x) = layout.into_iter().next() {
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
