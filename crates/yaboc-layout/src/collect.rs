mod tailsize;
use std::{collections::hash_map::Entry, sync::Arc};

use fxhash::{FxHashMap, FxHashSet};
use petgraph::unionfind::UnionFind;

use yaboc_absint::{AbstractDomain, Arg};
use yaboc_dependents::{NeededBy, RequirementSet};
use yaboc_hir::{HirIdWrapper, ParserDefId};
use yaboc_hir_types::DerefLevel;
use yaboc_mir::{CallMeta, MirInstr};
use yaboc_types::{PrimitiveType, Type, TypeId};

use crate::{
    mir_subst::{function_substitute, FunctionSubstitute},
    prop::SizeAlign,
};

use self::tailsize::{CallSite, TailCollector};

use super::{
    canon_layout, flat_layouts, prop::PSize, AbsLayoutCtx, ILayout, IMonoLayout, Layout,
    LayoutError, MonoLayout,
};

type LayoutSet<'a> = FxHashSet<IMonoLayout<'a>>;

pub fn pd_len_req() -> CallMeta {
    CallMeta::new(NeededBy::Len.into(), false)
}
pub fn pd_val_req() -> CallMeta {
    CallMeta::new(NeededBy::Val.into(), false)
}
pub fn root_req() -> CallMeta {
    CallMeta::new(NeededBy::Val | NeededBy::Len | NeededBy::Backtrack, false)
}
pub fn apply_req() -> CallMeta {
    CallMeta::new(NeededBy::Val | NeededBy::Backtrack, false)
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
    pub parser_slots: CallSlotResult<'a, (ILayout<'a>, CallMeta)>,
    pub funcall_slots: CallSlotResult<'a, ILayout<'a>>,
    pub tail_sa: FxHashMap<(ILayout<'a>, IMonoLayout<'a>), Option<SizeAlign>>,
}

pub struct LayoutCollector<'a, 'b> {
    ctx: &'b mut AbsLayoutCtx<'a>,
    parses: CallInfo<'a, (ILayout<'a>, CallMeta)>,
    funcalls: CallInfo<'a, ILayout<'a>>,
    arrays: LayoutSet<'a>,
    blocks: LayoutSet<'a>,
    nominals: LayoutSet<'a>,
    parsers: LayoutSet<'a>,
    functions: LayoutSet<'a>,
    root: Vec<(ILayout<'a>, IMonoLayout<'a>)>,
    processed_calls: FxHashSet<(ILayout<'a>, IMonoLayout<'a>, CallMeta)>,
    unprocessed: Vec<UnprocessedCall<'a>>,
}

pub enum UnprocessedCall<'a> {
    NominalParser(ILayout<'a>, IMonoLayout<'a>, RequirementSet),
    Block(ILayout<'a>, IMonoLayout<'a>, RequirementSet),
    IfParser(ILayout<'a>, IMonoLayout<'a>, RequirementSet),
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
        for mono in &layout {
            match &mono.mono_layout().0 {
                MonoLayout::SlicePtr => {
                    self.arrays.insert(mono);
                }
                MonoLayout::Nominal(_, _, _) => {
                    if self.nominals.insert(mono) {
                        let (arg, parser) = mono.unapply_nominal(self.ctx);
                        let parser = parser.inner();
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
                MonoLayout::BlockParser(..)
                | MonoLayout::ArrayParser(Some(_))
                | MonoLayout::Single
                | MonoLayout::Nil
                | MonoLayout::IfParser(..)
                | MonoLayout::Regex(..) => {
                    self.parsers.insert(mono);
                    self.parsers.insert(mono.remove_backtracking(self.ctx));
                }
                MonoLayout::ArrayParser(None) => {
                    self.functions.insert(mono);
                    self.functions.insert(mono.remove_backtracking(self.ctx));
                }
                MonoLayout::Primitive(_) => {}
                MonoLayout::Tuple(_) => panic!("tuples not supported yet"),
            }
        }
    }

    fn register_parse(&mut self, arg: ILayout<'a>, parser: ILayout<'a>, info: CallMeta) {
        if info.req.is_empty() {
            return;
        }
        let parser = parser
            .deref_to_level(self.ctx, DerefLevel::zero())
            .unwrap()
            .0;
        for mono in &parser {
            match mono.mono_layout().0 {
                MonoLayout::BlockParser(..) => {
                    if self.processed_calls.insert((arg, mono, info)) {
                        self.unprocessed
                            .push(UnprocessedCall::Block(arg, mono, info.req));
                    }
                }
                MonoLayout::NominalParser(..) => {
                    if self.processed_calls.insert((arg, mono, info)) {
                        self.unprocessed
                            .push(UnprocessedCall::NominalParser(arg, mono, info.req));
                    }
                }
                MonoLayout::Regex(..) => {
                    let single = IMonoLayout::int_single(self.ctx);
                    self.register_layouts(single.inner());
                    self.parses.add_call(
                        (arg, CallMeta::new(RequirementSet::all(), false)),
                        single.inner(),
                    );
                }
                MonoLayout::IfParser(..) => {
                    if self.processed_calls.insert((arg, mono, info)) {
                        self.unprocessed
                            .push(UnprocessedCall::IfParser(arg, mono, info.req));
                    }
                }
                _ => {}
            }
        }
        self.parses.add_call((arg, info), parser)
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

    fn collect_ins(
        &mut self,
        mir: &FunctionSubstitute<'a>,
        ins: MirInstr,
    ) -> Result<(), LayoutError> {
        match ins {
            MirInstr::ApplyArgs(_, fun, args, _, _) => {
                let fun_layout = mir.place(fun);
                let args = args.iter().map(|arg| mir.place(*arg)).collect::<Vec<_>>();
                let ty = mir.place_type(fun);
                self.register_funcall(fun_layout, args, ty)?;
                Ok(())
            }
            MirInstr::ParseCall(_, _, meta, arg, fun, _) => {
                let fun_layout = mir.place(fun);
                let arg_layout = mir.place(arg);
                self.register_parse(arg_layout, fun_layout, meta);
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn collect_mir(&mut self, mir: &FunctionSubstitute<'a>) -> Result<(), LayoutError> {
        for layout in mir.stack_layouts.iter() {
            self.register_layouts(*layout);
        }
        for ins in mir.f.iter_bb().flat_map(|(_, block)| block.ins()) {
            self.collect_ins(mir, ins)?;
        }
        Ok(())
    }

    fn collect_block(
        &mut self,
        arg: ILayout<'a>,
        parser: IMonoLayout<'a>,
        mut info: RequirementSet,
    ) -> Result<(), LayoutError> {
        let MonoLayout::BlockParser(id, _, bt) = parser.mono_layout().0 else {
            panic!("unexpected non-block-parser layout");
        };
        if !*bt {
            info &= !NeededBy::Backtrack
        };
        let fsub =
            function_substitute(yaboc_mir::FunKind::Block(*id), info, arg, parser, self.ctx)?;
        self.collect_mir(&fsub)?;
        Ok(())
    }

    fn collect_nominal_parse(
        &mut self,
        arg: ILayout<'a>,
        parser: IMonoLayout<'a>,
        mut info: RequirementSet,
    ) -> Result<(), LayoutError> {
        let (MonoLayout::NominalParser(pd, thunk_args, bt), ty) = parser.mono_layout() else {
            panic!("unexpected non-nominal-parser layout");
        };
        let Type::ParserArg { arg: arg_ty, .. } = self.ctx.db.lookup_intern_type(ty) else {
            panic!("unexpected non-parserarg type");
        };
        if !*bt {
            info &= !NeededBy::Backtrack
        }
        let parserdef = pd.lookup(self.ctx.db).unwrap();
        let mut args = FxHashMap::default();
        let thunk_ty = self.ctx.db.parser_result(ty).unwrap();
        args.insert(Arg::From, (arg, arg_ty));
        let arg_ids = parserdef.args.unwrap_or_default();
        for (id, arg) in arg_ids.iter().zip(thunk_args.iter()) {
            args.insert(Arg::Named(id.0), *arg);
        }
        let thunk = IMonoLayout::make_thunk(*pd, thunk_ty, &args, self.ctx).unwrap();
        if parserdef.thunky {
            self.register_layouts(thunk.inner());
        }
        let (arg_layout, parser_layout) = thunk.unapply_nominal(self.ctx);
        let val_info = if info.contains(NeededBy::Val) {
            Some(info & !NeededBy::Val)
        } else {
            None
        };
        for info in std::iter::once(info).chain(val_info) {
            let fsub = function_substitute(
                yaboc_mir::FunKind::ParserDef(*pd),
                info,
                arg_layout,
                parser_layout,
                self.ctx,
            )?;
            self.collect_mir(&fsub)?;
        }
        Ok(())
    }

    fn proc_list(&mut self) -> Result<(), LayoutError> {
        while let Some(mono) = self.unprocessed.pop() {
            match mono {
                UnprocessedCall::Block(arg, parser, info) => {
                    self.collect_block(arg, parser, info)?
                }
                UnprocessedCall::NominalParser(arg, parser, info) => {
                    self.collect_nominal_parse(arg, parser, info)?
                }
                UnprocessedCall::IfParser(from, inner, info) => {
                    let MonoLayout::IfParser(inner, c, _) = inner.mono_layout().0 else {
                        panic!("unexpected non-if-parser layout");
                    };
                    self.register_layouts(*inner);
                    let mut first_req = info | NeededBy::Val;
                    if self.ctx.db.lookup_intern_hir_constraint(*c).has_no_eof {
                        first_req |= NeededBy::Len
                    }
                    self.register_parse(from, *inner, CallMeta::new(first_req, false));
                    self.register_parse(from, *inner, CallMeta::new(info & NeededBy::Val, false));
                }
            }
        }
        Ok(())
    }

    pub fn collect(&mut self, pds: &[ParserDefId]) -> Result<(), LayoutError> {
        for pd in pds {
            let sig = self.ctx.db.parser_args(*pd)?;
            let thunk_ty = self.ctx.db.intern_type(Type::Nominal(sig.thunk));
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

    pub fn into_results(self) -> Result<LayoutCollection<'a>, LayoutError> {
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

        let mut tail_sa = FxHashMap::default();
        let mut tail_collector = TailCollector::new(self.ctx);
        for (parser, froms) in parser_slots.occupied_entries.iter() {
            for (_, (from, _)) in froms.iter() {
                let sa = tail_collector.size(CallSite(*from, *parser))?;
                tail_sa.insert((*from, *parser), sa);
            }
        }

        Ok(LayoutCollection {
            root,
            arrays: self.arrays,
            blocks: self.blocks,
            nominals: self.nominals,
            parsers: self.parsers,
            functions: self.functions,
            primitives,
            parser_slots,
            funcall_slots,
            tail_sa,
        })
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
