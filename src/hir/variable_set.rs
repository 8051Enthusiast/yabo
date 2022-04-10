use std::collections::btree_map::Entry;

use crate::interner::FieldName;

use std::collections::BTreeMap;

use std::fmt::Debug;

use std::hash::Hash;

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct VariableSet<T: Clone + Hash + Eq + Debug> {
    pub(crate) set: BTreeMap<FieldName, VarStatus<T>>,
}

impl<T: Clone + Hash + Eq + Debug> VariableSet<T> {
    pub fn new() -> Self {
        VariableSet {
            set: BTreeMap::new(),
        }
    }
    pub fn singular(id: FieldName, data: T) -> Self {
        let mut set = BTreeMap::new();
        set.insert(id, VarStatus::Always(data));
        VariableSet { set }
    }
    pub fn merge_product(&self, other: &Self) -> (Self, Vec<FieldName>) {
        let mut set = self.set.clone();
        let mut doubled = Vec::new();
        for (k, v) in other.set.iter() {
            match set.entry(*k) {
                Entry::Vacant(entry) => {
                    entry.insert(v.clone());
                }
                Entry::Occupied(_) => doubled.push(*k),
            }
        }
        (VariableSet { set }, doubled)
    }
    pub fn without_data(&self) -> VariableSet<()> {
        let mut set = BTreeMap::new();
        for (k, v) in self.set.iter() {
            set.insert(*k, v.without_data());
        }
        VariableSet { set }
    }
    pub fn map<S: Clone + Eq + Debug + Hash>(
        &self,
        mut f: impl FnMut(FieldName, &T) -> S,
    ) -> VariableSet<S> {
        let mut set = BTreeMap::new();
        for (k, v) in self.set.iter() {
            set.insert(*k, v.map(|x| f(*k, x)));
        }
        VariableSet { set }
    }
    pub fn get(&self, idx: FieldName) -> Option<&VarStatus<T>> {
        self.set.get(&idx)
    }
    pub fn iter(&self) -> impl Iterator<Item = (&FieldName, &VarStatus<T>)> {
        self.set.iter()
    }
}

impl VariableSet<()> {
    pub fn merge_sum<T: Hash + Eq + Clone + Debug>(&self, other: &VariableSet<T>) -> Self {
        let mut new = BTreeMap::new();
        new.extend(self.set.iter().map(|(k, v)| {
            (*k, {
                VarStatus::<()>::from_accessibility(v.is_accessible() && other.set.contains_key(k))
            })
        }));
        for (k, v) in other.set.iter() {
            let other_entry = v.is_accessible();
            new.entry(*k)
                .and_modify(|e| {
                    *e = VarStatus::<()>::from_accessibility(e.is_accessible() && other_entry)
                })
                .or_insert(VarStatus::Maybe(()));
        }
        VariableSet { set: new }
    }
}

impl<T: Copy + Hash + Eq + Debug> Default for VariableSet<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum VarStatus<T: Clone + Eq + Debug + Hash> {
    Always(T),
    Maybe(T),
}

impl<T: Clone + Eq + Debug + Hash> VarStatus<T> {
    pub fn is_accessible(&self) -> bool {
        matches!(self, Self::Always(_))
    }
    pub(crate) fn without_data(&self) -> VarStatus<()> {
        match self {
            VarStatus::Always(_) => VarStatus::Always(()),
            VarStatus::Maybe(_) => VarStatus::Maybe(()),
        }
    }
    pub(crate) fn map<S: Clone + Eq + Debug + Hash>(&self, f: impl FnOnce(&T) -> S) -> VarStatus<S> {
        match self {
            VarStatus::Always(a) => VarStatus::Always(f(a)),
            VarStatus::Maybe(a) => VarStatus::Always(f(a)),
        }
    }
    pub(crate) fn from_accessibility(access: bool) -> VarStatus<()> {
        if access {
            VarStatus::Always(())
        } else {
            VarStatus::Maybe(())
        }
    }
    pub fn inner(&self) -> &T {
        match self {
            VarStatus::Always(a) | VarStatus::Maybe(a) => a,
        }
    }
}
