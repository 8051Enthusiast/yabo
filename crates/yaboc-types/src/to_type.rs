use std::{collections::HashMap, hash::Hash, sync::Arc};

use crate::{
    connections::ConnectionMap,
    inference::{BlockInfHead, TRACING_ENABLED},
    BlockTypeHead, TypeVarRef,
};
use yaboc_base::{
    dbeprintln, dbpanic,
    error::{SResult, Silencable, SilencedError},
    interner::DefId,
};

use super::{
    inference::{InfTypeId, InfTypeInterner, InferenceType},
    inference::{InferenceContext, TypeResolver},
    Type, TypeError, TypeId, TypeInterner,
};

pub struct TyVars<'a, 'intern> {
    pub(crate) cur: &'a [InfTypeId<'intern>],
}

impl<'a, 'intern> TyVars<'a, 'intern> {
    pub fn resolve(&'a self, index: u32) -> Option<InfTypeId<'intern>> {
        self.cur.get(index as usize).copied()
    }
}

#[derive(Debug)]
pub struct MemoRecursor<From: Copy + Eq + Hash, To: Copy> {
    process: HashMap<From, usize>,
    memo: HashMap<From, SResult<To>>,
}

impl<From: Copy + Eq + Hash, To: Copy> Default for MemoRecursor<From, To> {
    fn default() -> Self {
        Self {
            process: Default::default(),
            memo: Default::default(),
        }
    }
}

impl<From: Copy + Eq + Hash + std::fmt::Debug, To: Copy> MemoRecursor<From, To> {
    fn enter_fun(&mut self, from: From) -> Option<Result<To, TypeError>> {
        if let Some(t) = self.memo.get(&from) {
            return Some(t.clone().map_err(|e| e.clone().into()));
        }
        let depth = self.process.len();
        if self.process.insert(from, depth).is_some() {
            return Some(Err(TypeError::RecursiveType));
        }
        None
    }
    fn leave_fun(&mut self, from: From, to: To) -> Result<To, TypeError> {
        self.memo.insert(from, Ok(to));
        self.process.remove(&from);
        Ok(to)
    }
    fn leave_fun_err(&mut self, from: From, err: TypeError) -> Result<To, TypeError> {
        self.memo.insert(from, Err(err.clone().silence()));
        self.process.remove(&from);
        Err(err)
    }
}

pub struct TypeConvertMemo<'a, 'intern, TR: TypeResolver<'intern>> {
    convert: MemoRecursor<(InfTypeId<'intern>, DefId), TypeId>,
    id: DefId,
    map: ConnectionMap,
    pub ctx: &'a mut InferenceContext<'intern, TR>,
}

impl<'a, 'intern, TR: TypeResolver<'intern>> TypeConvertMemo<'a, 'intern, TR> {
    pub(crate) fn new(
        ctx: &'a mut InferenceContext<'intern, TR>,
        id: DefId,
        map: ConnectionMap,
    ) -> Self {
        TypeConvertMemo {
            convert: Default::default(),
            id,
            map,
            ctx,
        }
    }
    pub fn set_id(&mut self, id: DefId) {
        self.id = id;
    }
    fn canon(&mut self, ty: InfTypeId<'intern>) -> Result<InfTypeId<'intern>, TypeError> {
        match ty.value() {
            InferenceType::TypeVarRef(var @ TypeVarRef(def, _)) => {
                if *def == self.id {
                    return Ok(ty);
                }

                let Some(ours) = self.map.def_map(self.id, *var) else {
                    return Err(TypeError::NonInferTypeVar(*var));
                };

                Ok(self.ctx.intern_infty(InferenceType::TypeVarRef(ours)))
            }
            _ => Ok(self.ctx.canon(ty)),
        }
    }
    fn convert_arg_list(&mut self, args: InfTypeId<'intern>) -> Result<Vec<TypeId>, TypeError> {
        let mut result = vec![];
        let mut arg_list = self.canon(args)?;
        while let InferenceType::FunArgCons(head, tail) = arg_list.value() {
            let arg = self.convert_to_type(*head)?;
            result.push(arg);
            arg_list = self.canon(*tail)?;
        }
        match arg_list.value() {
            InferenceType::FunArgNil => Ok(result),
            InferenceType::Var(..) => Err(TypeError::NonInfer),
            InferenceType::Unknown => Err(TypeError::Silenced(SilencedError::new())),
            _ => dbpanic!(self.ctx.tr.db(), "Unexpected type {}", &arg_list),
        }
    }
    fn convert_to_type_impl(&mut self, infty: InfTypeId<'intern>) -> Result<TypeId, TypeError> {
        let infty = self.canon(infty)?;
        let res = match infty.value() {
            InferenceType::TypeVarRef(varref) => Type::TypeVarRef(*varref),
            InferenceType::Primitive(p) => Type::Primitive(*p),
            InferenceType::Var(..) => {
                return Err(TypeError::NonInfer);
            }
            InferenceType::Unknown => Type::Unknown,
            InferenceType::Block(BlockInfHead {
                def,
                parse_arg,
                ty_args,
                internal: _,
            }) => {
                let parse_arg = parse_arg.map(|x| self.convert_to_type(x)).transpose()?;
                let ty_args = ty_args
                    .iter()
                    .copied()
                    .map(|x| self.convert_to_type(x))
                    .collect::<Result<_, _>>()?;
                Type::Block(BlockTypeHead {
                    def: *def,
                    parse_arg,
                    ty_args: Arc::new(ty_args),
                })
            }
            InferenceType::Loop(kind, inner) => Type::Loop(*kind, self.convert_to_type(*inner)?),
            InferenceType::ParserArg { result, arg } => Type::ParserArg {
                result: self.convert_to_type(*result)?,
                arg: self.convert_to_type(*arg)?,
            },
            InferenceType::FunctionArgs { result, args } => {
                let args = self.convert_arg_list(*args)?;
                Type::FunctionArg(self.convert_to_type(*result)?, Arc::new(args))
            }
            InferenceType::FunArgCons(_, _) | InferenceType::FunArgNil => unreachable!(),
        };
        Ok(self.ctx.tr.db().intern_type(res))
    }
    pub fn convert_to_type(&mut self, infty: InfTypeId<'intern>) -> Result<TypeId, TypeError> {
        if let Some(x) = self.convert.enter_fun((infty, self.id)) {
            return x;
        }
        match self.convert_to_type_impl(infty) {
            Ok(x) => {
                if TRACING_ENABLED {
                    dbeprintln!(
                        self.ctx.tr.db(),
                        "[{}] Converted {} to {}",
                        &self.ctx.tr.name(),
                        &infty,
                        &x
                    );
                }
                self.convert.leave_fun((infty, self.id), x)
            }
            Err(e) => self.convert.leave_fun_err((infty, self.id), e),
        }
    }
}

impl<'intern, TR: TypeResolver<'intern>> InfTypeInterner<'intern>
    for TypeConvertMemo<'_, 'intern, TR>
{
    fn intern_infty(&mut self, infty: InferenceType<InfTypeId<'intern>>) -> InfTypeId<'intern> {
        InfTypeId(self.ctx.interner.intern(infty))
    }

    fn intern_infty_slice(
        &mut self,
        slice: &[InfTypeId<'intern>],
    ) -> super::inference::InfSlice<'intern> {
        self.ctx.slice_interner.intern_slice(slice)
    }
}
