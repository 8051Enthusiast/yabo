use std::slice;

use fxhash::{FxHashMap, FxHashSet};
use yaboc_base::{
    error::{SResult, Silencable, SilencedError},
    interner::DefId,
};

use crate::{
    inference::{
        InfTypeId, InferenceContext, InferenceType, InternedNomHead, NominalInfHead, TypeResolver,
    },
    NominalKind, PrimitiveType, TypeConvError, TypeVarRef,
};

enum State {
    InProgress,
    Finished,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum DerefData {
    Base,
    TypeVar(TypeVarRef),
    U8,
    Nominal { def: DefId, level: u32 },
    Error(SilencedError),
}

impl DerefData {
    fn deref_of(&self, def: DefId) -> Self {
        match self {
            DerefData::Nominal { level, .. } => DerefData::Nominal {
                def,
                level: level + 1,
            },
            DerefData::U8 => DerefData::Nominal { def, level: 2 },
            DerefData::Base => DerefData::Nominal { def, level: 1 },
            // type vars cannot be dereffed to
            DerefData::TypeVar(_) => panic!("type var cannot be dereffed"),
            DerefData::Error(s) => DerefData::Error(s.clone()),
        }
    }

    fn level(&self) -> u32 {
        match self {
            DerefData::Nominal { level, .. } => *level,
            DerefData::U8 => 1,
            DerefData::Base | DerefData::TypeVar(_) | DerefData::Error(_) => 0,
        }
    }
}

pub struct DerefCache {
    deref_cache: FxHashMap<DefId, DerefData>,
    local_deref: FxHashMap<DefId, State>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum DerefInfoTargetResponse {
    Target(DerefData),
    Unknown,
    NonDeref,
}

impl DerefCache {
    fn deref_data_to_level<'a>(
        &'a self,
        mut data: &'a DerefData,
        target_level: u32,
    ) -> &'a DerefData {
        assert!(data.level() >= target_level);
        while let DerefData::Nominal { def, .. } = data {
            let deref = &self.deref_cache[def];
            // this is the first one where the level is strictly smaller than
            // the target level, so the current one has the correct level
            if deref.level() < target_level {
                break;
            }
            data = deref;
        }
        match data {
            DerefData::U8 => {
                assert!(target_level <= 1);
                if target_level == 1 {
                    &DerefData::U8
                } else {
                    &DerefData::Base
                }
            }
            otherwise => otherwise,
        }
    }

    fn nominal_deref_info_impl<'intern, TR: TypeResolver<'intern>>(
        &mut self,
        head: &InternedNomHead<'intern>,
        ctx: &mut InferenceContext<'intern, TR>,
    ) -> Result<DerefInfoTargetResponse, TypeConvError> {
        let Some(res) = ctx.deref(head)? else {
            // returning None means that it cannot be dereffed
            return Ok(DerefInfoTargetResponse::NonDeref);
        };
        let Some(info) = self.deref_info(res, ctx)? else {
            // if we don't know the deref info of the result,
            // we cannot know the deref info of this one either
            return Ok(DerefInfoTargetResponse::Unknown);
        };
        match (head.kind, &info) {
            (NominalKind::Def, DerefData::TypeVar(v)) => {
                Err(TypeConvError::TypeVarReturn(head.def, *v))
            }
            _ => Ok(DerefInfoTargetResponse::Target(info)),
        }
    }

    fn nominal_deref_info<'intern, TR: TypeResolver<'intern>>(
        &mut self,
        head: &InternedNomHead<'intern>,
        ctx: &mut InferenceContext<'intern, TR>,
    ) -> Result<Option<DerefData>, TypeConvError> {
        if let Some(result) = self.deref_cache.get(&head.def) {
            return Ok(Some(result.deref_of(head.def)));
        }
        let is_local_def = ctx.tr.is_local(head.def);
        if is_local_def {
            let old_state = self.local_deref.insert(head.def, State::InProgress);
            // if we are already in progress, we have a cycle and
            // cannot return a concrete value
            if matches!(old_state, Some(State::InProgress)) {
                return Ok(None);
            }
        }
        let res = self.nominal_deref_info_impl(head, ctx);
        if is_local_def {
            self.local_deref.insert(head.def, State::Finished);
        }
        let res = match res {
            // we don't want to cache the `None` case as we may
            // actually get something from the same id if
            // we start from another id
            Ok(DerefInfoTargetResponse::Unknown) => return Ok(None),
            Ok(DerefInfoTargetResponse::Target(res)) => Ok(Some(res)),
            Ok(DerefInfoTargetResponse::NonDeref) => Ok(None),
            Err(e) => Err(e),
        };
        let cached_res = res
            .clone()
            .unwrap_or_else(|e| Some(DerefData::Error(e.clone().silence())));
        match head.kind {
            NominalKind::Def => {
                if let Some(c) = cached_res {
                    self.deref_cache.insert(head.def, c);
                }
                match res {
                    Ok(None) => Ok(Some(DerefData::Base)),
                    Ok(Some(target)) => Ok(Some(target.deref_of(head.def))),
                    Err(e) => Err(e),
                }
            }
            NominalKind::Fun | NominalKind::Static => match res {
                Ok(None) => Ok(Some(DerefData::Base)),
                Ok(Some(target)) => Ok(Some(target)),
                Err(e) => Err(e),
            },
            NominalKind::Block => unreachable!(),
        }
    }

    fn var_deref_info<'intern, TR: TypeResolver<'intern>>(
        &mut self,
        types: &[InfTypeId<'intern>],
        ctx: &mut InferenceContext<'intern, TR>,
    ) -> Result<Option<DerefData>, TypeConvError> {
        let mut elements = Vec::with_capacity(types.len());
        for ty in types {
            match ty.value() {
                InferenceType::Var(_) => continue,
                _ => elements.extend(self.deref_info(*ty, ctx)?),
            }
        }
        let mut min_deref = None;
        for element in elements.iter() {
            let level = match element {
                DerefData::Nominal { level, .. } => *level,
                DerefData::U8 => 1,
                DerefData::Base => 0,
                DerefData::TypeVar(v) => return Ok(Some(DerefData::TypeVar(*v))),
                DerefData::Error(s) => return Ok(Some(DerefData::Error(s.clone()))),
            };
            if Some(level) < min_deref || min_deref.is_none() {
                min_deref = Some(level)
            }
        }
        let Some(mut min_deref) = min_deref else {
            return Ok(None);
        };
        loop {
            for element in elements.iter_mut() {
                *element = self.deref_data_to_level(element, min_deref).clone()
            }
            if elements[1..]
                .iter()
                .zip(elements.iter())
                .all(|(a, b)| a == b)
            {
                return Ok(Some(elements[0].clone()));
            }
            let Some(new_deref) = min_deref.checked_sub(1) else {
                break;
            };
            min_deref = new_deref;
        }
        Ok(elements.into_iter().max())
    }

    fn deref_info<'intern, TR: TypeResolver<'intern>>(
        &mut self,
        ty: InfTypeId<'intern>,
        ctx: &mut InferenceContext<'intern, TR>,
    ) -> Result<Option<DerefData>, TypeConvError> {
        match ty.value() {
            InferenceType::Var(vid) => {
                let types = ctx.var_store.get(*vid).lower().to_vec();
                self.var_deref_info(&types, ctx)
            }
            InferenceType::Nominal(NominalInfHead {
                kind: NominalKind::Block,
                ..
            }) => Ok(Some(DerefData::Base)),
            InferenceType::Nominal(head) => self.nominal_deref_info(head, ctx),
            InferenceType::TypeVarRef(v) => Ok(Some(DerefData::TypeVar(*v))),
            InferenceType::Unknown => Ok(Some(DerefData::Error(SilencedError::new()))),
            InferenceType::Primitive(PrimitiveType::U8) => Ok(Some(DerefData::U8)),
            InferenceType::InferField(_, _)
            | InferenceType::InferIfResult(_, _, _)
            | InferenceType::SizeOf => panic!("virtual type cannot be dereffed"),
            _ => Ok(Some(DerefData::Base)),
        }
    }

    fn check_locals<'intern, TR: TypeResolver<'intern>>(
        &mut self,
        defs: &[InfTypeId<'intern>],
        ctx: &mut InferenceContext<'intern, TR>,
    ) -> Result<(), TypeConvError> {
        let mut unknown_defs = Vec::new();
        for def in defs {
            let InferenceType::Nominal(head) = def.value() else {
                panic!("local def type must be nominal")
            };
            let info = self.deref_info(*def, ctx)?;
            match info {
                Some(DerefData::Error(s)) => return Err(s.clone().into()),
                Some(DerefData::TypeVar(v)) if head.kind == NominalKind::Def => {
                    return Err(TypeConvError::TypeVarReturn(head.def, v))
                }
                None => unknown_defs.push(head.def),
                Some(_) => {}
            };
        }
        if !unknown_defs.is_empty() {
            return Err(TypeConvError::CyclicReturnThunks(unknown_defs));
        }
        Ok(())
    }

    pub fn new<'intern, TR: TypeResolver<'intern>>(
        ctx: &mut InferenceContext<'intern, TR>,
        local_defs: &[InfTypeId<'intern>],
    ) -> Result<Self, TypeConvError> {
        let mut cache = Self {
            deref_cache: FxHashMap::default(),
            local_deref: FxHashMap::default(),
        };
        cache.check_locals(local_defs, ctx)?;
        Ok(cache)
    }

    pub fn homogenize<'intern, TR: TypeResolver<'intern>>(
        &mut self,
        types: &[InfTypeId<'intern>],
        ctx: &mut InferenceContext<'intern, TR>,
    ) -> SResult<Vec<InfTypeId<'intern>>> {
        let flattened_types = types
            .iter()
            .flat_map(|ty| match ty.value() {
                InferenceType::Var(vid) => ctx.var_store.get(*vid).lower(),
                _ => slice::from_ref(ty),
            })
            .copied()
            .filter(|t| !matches!(t.value(), InferenceType::Var(_)))
            .collect::<Vec<_>>();
        let deref = match self.var_deref_info(&flattened_types, ctx) {
            Ok(Some(deref)) => deref,
            Ok(None) => return Ok(vec![]),
            Err(TypeConvError::Silenced(s)) => return Err(s.clone()),
            // any novel errors should have been checked by check_locals
            Err(_) => panic!("homogenize can only be called after new() finished"),
        };
        let mut worklist = flattened_types;
        let mut done = FxHashSet::default();
        let mut matches = Vec::new();
        while let Some(ty) = worklist.pop() {
            if !done.insert(ty) {
                continue;
            }
            match (ty.value(), &deref) {
                (_, DerefData::Error(s)) => return Err(s.clone()),
                (InferenceType::Var(v), _) => {
                    let types = ctx.var_store.get(*v).lower();
                    worklist.extend(
                        types
                            .iter()
                            .filter(|t| !matches!(t.value(), InferenceType::Var(_))),
                    );
                }
                (InferenceType::Primitive(PrimitiveType::U8), DerefData::Base) => {
                    let int = ctx.int();
                    worklist.push(int);
                }
                (InferenceType::Nominal(head), DerefData::Nominal { def, .. })
                    if head.def == *def =>
                {
                    matches.push(ty);
                }
                (InferenceType::Nominal(head), _) => {
                    if let Some(deref) = ctx.deref(head)? {
                        worklist.push(deref);
                    } else {
                        matches.push(ty);
                    }
                }
                _ => {
                    matches.push(ty);
                }
            }
        }
        Ok(matches)
    }

    pub fn deref_level<'intern, TR: TypeResolver<'intern>>(
        &mut self,
        ty: InfTypeId<'intern>,
        ctx: &mut InferenceContext<'intern, TR>,
    ) -> SResult<u32> {
        let info = match self.deref_info(ty, ctx) {
            Ok(Some(DerefData::Error(s))) | Err(TypeConvError::Silenced(s)) => {
                return Err(s.clone())
            }
            Ok(Some(info)) => info,
            Ok(None) => return Ok(0),
            // any novel errors should have been checked by check_locals
            Err(_) => panic!("homogenize can only be called after new() finished"),
        };
        Ok(info.level())
    }
}
