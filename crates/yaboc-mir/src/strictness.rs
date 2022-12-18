use fxhash::{FxHashMap, FxHashSet};
use yaboc_base::error::SResult;
use yaboc_hir_types::DerefLevel;
use yaboc_types::Type;

use crate::{BBRef, BasicBlock, Function, MirInstr, Mirs, Place, PlaceRef};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Strictness {
    Static(DerefLevel),
    Return,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Use {
    pub strictness: Strictness,
    pub always: bool,
}

impl Use {
    fn sometimes(self) -> Self {
        Use {
            strictness: self.strictness,
            always: false,
        }
    }
    fn new_always(strictness: Strictness) -> Self {
        Use {
            strictness,
            always: true,
        }
    }
    fn new_static(deref_level: DerefLevel) -> Self {
        Use {
            strictness: Strictness::Static(deref_level),
            always: true,
        }
    }
}

impl Ord for Strictness {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Strictness::Static(a), Strictness::Static(b)) => a.cmp(b),
            (Strictness::Return, Strictness::Return) => std::cmp::Ordering::Equal,
            (Strictness::Static(a), Strictness::Return) => {
                if a == &DerefLevel::zero() {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Greater
                }
            }
            (Strictness::Return, Strictness::Static(a)) => {
                if a == &DerefLevel::zero() {
                    std::cmp::Ordering::Greater
                } else {
                    std::cmp::Ordering::Less
                }
            }
        }
    }
}

impl PartialOrd for Strictness {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl std::ops::BitOr for Use {
    type Output = Self;
    fn bitor(self, other: Self) -> Self {
        Use {
            strictness: self.strictness.max(other.strictness),
            always: self.always && other.always,
        }
    }
}

impl std::ops::BitAnd for Use {
    type Output = Self;
    fn bitand(self, other: Self) -> Self {
        Use {
            strictness: self.strictness.max(other.strictness),
            always: self.always || other.always,
        }
    }
}

#[derive(Default, Clone, PartialEq, Eq, Debug)]
pub struct PlaceStrictness {
    map: FxHashMap<PlaceRef, Use>,
}

impl PlaceStrictness {
    fn combine(&self, other: &Self) -> Self {
        let mut map = self.map.clone();
        for (place, branch_use) in &other.map {
            if let Some(old) = map.get_mut(place) {
                *old = *old | *branch_use;
            } else {
                map.insert(*place, branch_use.sometimes());
            }
        }
        for (place, branch_use) in self.map.iter() {
            if !other.map.contains_key(place) {
                map.insert(*place, branch_use.sometimes());
            }
        }
        PlaceStrictness { map }
    }
    fn collect(&mut self, other: &Self) {
        for (place, other_use) in &other.map {
            if let Some(old) = self.map.get_mut(place) {
                *old = *old | *other_use;
            } else {
                self.map.insert(*place, *other_use);
            }
        }
    }
    fn insert(&mut self, place: PlaceRef, new_use: Use) {
        if let Some(old) = self.map.get_mut(&place) {
            *old = *old & new_use;
        } else {
            self.map.insert(place, new_use);
        }
    }
}

pub struct StrictnessCtx<'a> {
    fun: &'a Function,
    db: &'a dyn Mirs,
    pred: Vec<Vec<BBRef>>,
    block_in: Vec<PlaceStrictness>,
    kill: Vec<PlaceStrictness>,
    fixed: PlaceStrictness,
    included: Vec<bool>,
    worklist: Vec<BBRef>,
    active: FxHashSet<BBRef>,
    ran_before: FxHashSet<BBRef>,
}

impl<'a> StrictnessCtx<'a> {
    pub fn new(fun: &'a Function, db: &'a dyn Mirs) -> SResult<Self> {
        let pred = fun.preds();
        let num_blocks = fun.iter_bb().count();
        let block_in = vec![PlaceStrictness::default(); num_blocks];
        let kill = vec![PlaceStrictness::default(); num_blocks];
        let mut fixed = PlaceStrictness::default();
        for (place, info) in fun.iter_places() {
            let deref_level = db.deref_level(info.ty)?;
            //dbeprintln!(&(fun, db), "deref {} place {}", &deref_level, &place);
            if deref_level == DerefLevel::zero() {
                fixed.map.insert(place, Use::new_static(deref_level));
                continue;
            }
            match info.place {
                Place::Return => fixed.map.insert(place, Use::new_always(Strictness::Return)),
                Place::Stack(_) => continue,
                _ => {
                    let deref_level = db.deref_level(info.ty)?;
                    fixed.map.insert(place, Use::new_static(deref_level))
                }
            };
        }
        let worklist = fun.success_returns().to_vec();
        let active = worklist.iter().cloned().collect();
        let mut included_worklist = worklist.clone();
        let mut included = vec![false; num_blocks];
        while let Some(bb) = included_worklist.pop() {
            included[bb.as_index()] = true;
            for pred in &pred[bb.as_index()] {
                if !included[pred.as_index()] {
                    included_worklist.push(*pred);
                }
            }
        }
        Ok(StrictnessCtx {
            fun,
            db,
            pred,
            block_in,
            kill,
            fixed,
            included,
            worklist,
            active,
            ran_before: Default::default(),
        })
    }

    fn block_out(&self, block: &BasicBlock) -> (PlaceStrictness, Option<PlaceStrictness>) {
        let Some(sucs) = block.ins.last().unwrap().control_flow() else {
            return (PlaceStrictness::default(), None);
        };
        let next = sucs.next;
        let out = self.block_in[next.as_index()].clone();
        let mut fallible_out: Option<PlaceStrictness> = None;
        for suc in sucs
            .successors()
            .filter(|x| self.included[x.as_index()] && *x != next)
        {
            if let Some(o) = fallible_out.as_mut() {
                fallible_out = Some(o.combine(&self.block_in[suc.as_index()]));
            } else {
                fallible_out = Some(self.block_in[suc.as_index()].clone());
            }
        }
        (out, fallible_out)
    }

    fn ins_kills(&self, kill: &mut PlaceStrictness, out: &mut PlaceStrictness, ins: &MirInstr) {
        let (a, b) = match ins {
            MirInstr::IntBin(ret, _, _, _)
            | MirInstr::IntUn(ret, _, _)
            | MirInstr::Comp(ret, _, _, _)
            | MirInstr::StoreVal(ret, _)
            | MirInstr::ApplyArgs(ret, _, _, _, _)
            | MirInstr::Copy(ret, _, _)
            | MirInstr::Field(ret, _, _, _) => (Some(*ret), None),
            MirInstr::ParseCall(ret, retlen, _, _, _, _) => (*ret, *retlen),
            MirInstr::AssertVal(_, _, _)
            | MirInstr::Branch(_)
            | MirInstr::Return(_)
            | MirInstr::SetDiscriminant(_, _, _) => (None, None),
        };
        for place in a.into_iter().chain(b) {
            if self.fixed.map.contains_key(&place) {
                continue;
            }
            if let Some(strictness) = out.map.remove(&place) {
                kill.insert(place, strictness);
            }
        }
    }

    fn ins_refs(&self, out: &mut PlaceStrictness, ins: &MirInstr) -> SResult<()> {
        let mut insert = |place, strictness| {
            if self.fixed.map.contains_key(&place) {
                return;
            }
            out.insert(place, strictness);
        };
        match ins {
            MirInstr::IntBin(_, _, a, b)
            | MirInstr::Comp(_, _, a, b)
            | MirInstr::ParseCall(_, _, _, a, b, _) => {
                insert(*a, Use::new_static(DerefLevel::zero()));
                insert(*b, Use::new_static(DerefLevel::zero()));
            }
            MirInstr::IntUn(_, _, a)
            | MirInstr::AssertVal(a, _, _)
            | MirInstr::Field(_, a, _, _) => {
                insert(*a, Use::new_static(DerefLevel::zero()));
            }
            MirInstr::ApplyArgs(_, fun, args, _, _) => {
                insert(*fun, Use::new_static(DerefLevel::zero()));
                let ty = self.fun.place(*fun).ty;
                let Type::FunctionArg(_, arg_tys) = self.db.lookup_intern_type(ty) else {
                    panic!("Expected function type");
                };
                for (place, ty) in args.iter().zip(arg_tys.iter()) {
                    let deref_level = self.db.deref_level(*ty)?;
                    insert(*place, Use::new_static(deref_level));
                }
            }
            MirInstr::Copy(f, g, _) => {
                if self.fixed.map.contains_key(g) {
                    return Ok(());
                }
                let strictness = if let Some(strictness) = self.fixed.map.get(f) {
                    *strictness
                } else if let Some(strictness) = out.map.get(f) {
                    *strictness
                } else {
                    return Ok(());
                };
                out.insert(*g, strictness);
            }
            MirInstr::Branch(_)
            | MirInstr::Return(_)
            | MirInstr::StoreVal(_, _)
            | MirInstr::SetDiscriminant(_, _, _) => {}
        }
        Ok(())
    }

    fn collect_strictness(&self) -> SResult<Vec<Strictness>> {
        let mut kills = PlaceStrictness::default();
        for k in self.kill.iter() {
            kills.collect(k);
        }
        let mut strictness = Vec::new();
        for (place, info) in self.fun.iter_places() {
            let overall_use = self
                .fixed
                .map
                .get(&place)
                .or_else(|| kills.map.get(&place))
                .filter(|x| x.always)
                .map(|x| x.strictness);
            let strict = if let Some(strict) = overall_use {
                strict
            } else {
                let deref_level = self.db.deref_level(info.ty)?;
                Strictness::Static(deref_level)
            };
            strictness.push(strict);
        }
        Ok(strictness)
    }

    pub fn run(&mut self) -> SResult<Vec<Strictness>> {
        while let Some(bb) = self.worklist.pop() {
            self.active.remove(&bb);
            let block = self.fun.bb(bb);
            let (mut out, fallible_out) = self.block_out(block);
            let mut kills = PlaceStrictness::default();
            for (i, ins) in block.ins.iter().rev().enumerate() {
                self.ins_refs(&mut out, ins)?;
                self.ins_kills(&mut kills, &mut out, ins);
                if i == 0 {
                    if let Some(fout) = &fallible_out {
                        out = out.combine(fout)
                    }
                }
            }
            let in_ = self.block_in[bb.as_index()].clone();
            //eprintln!("{}: {:?} -> {:?} (kill {:?})", bb, in_, out, kills);
            self.kill[bb.as_index()] = kills;
            if in_ != out || self.ran_before.insert(bb) {
                self.block_in[bb.as_index()] = out;
                for pred in &self.pred[bb.as_index()] {
                    if self.active.insert(*pred) {
                        self.worklist.push(*pred);
                    }
                }
            }
        }
        self.collect_strictness()
    }
}
