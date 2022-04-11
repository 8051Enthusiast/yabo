use crate::{
    dbformat,
    error::{SResult, SilencedError},
    hir::recursion::FunctionSscId,
};

use super::*;

pub fn parser_returns(db: &dyn TyHirs, id: hir::ParserDefId) -> SResult<ParserDefType> {
    db.parser_returns_ssc(db.parser_ssc(id)?)
        .into_iter()
        .find(|x| x.id.0 == id.0)
        .ok_or(SilencedError)
}

pub fn parser_returns_ssc(db: &dyn TyHirs, id: FunctionSscId) -> Vec<ParserDefType> {
    let def_ids = db.lookup_intern_recursion_scc(id);
    let defs = def_ids
        .iter()
        .flat_map(|fun: &hir::ParserDefId| fun.lookup(db))
        .collect::<Vec<_>>();

    let resolver = ReturnResolver::new(db);
    let mut ctx = TypingContext::new(db, resolver);

    for def in def_ids.iter() {
        // in this loop we do not directly the inference variables we are creating yet
        let deref = ctx.infctx.var();
        let retinf = ParserDefTypeInf { id: *def, deref };
        ctx.infctx.tr.return_infs.insert(retinf.id.0, retinf);
    }
    let defs = defs
        .into_iter()
        .flat_map(|def| -> Result<hir::ParserDef, TypeError> {
            // in this separate loop we do the actual type inference
            let expr = def.to.lookup(db)?.expr;
            let mut context = TypingLocation {
                vars: TypeVarCollection::at_id(db, def.id)?,
                loc: db.hir_parent_module(def.id.0)?.0,
                pd: def.id,
            };
            let ty = ctx.val_expression_type(&mut context, &expr)?.root_type();
            let sig = db.parser_args(def.id)?;
            if let Some(from) = sig.from {
                let inffrom = ctx.infctx.from_type(from);
                let deref = ctx.infctx.tr.return_infs[&def.id.0].deref;
                let ret = ctx.infctx.parser_apply(ty, inffrom)?;
                ctx.infctx.constrain(ret, deref)?;
            };
            Ok(def)
        })
        .collect::<Vec<_>>();
    defs.into_iter()
        .flat_map(|def| -> Result<ParserDefType, TypeError> {
            // here we are finished with inference so we can convert to actual types
            let deref = ctx.inftype_to_concrete_type(ctx.infctx.tr.return_infs[&def.id.0].deref)?;
            Ok(ParserDefType { id: def.id, deref })
        })
        .collect()
}

pub struct ReturnResolver<'a> {
    db: &'a dyn TyHirs,
    return_infs: FxHashMap<HirId, ParserDefTypeInf>,
}

impl<'a> ReturnResolver<'a> {
    #[must_use]
    pub fn new(db: &'a dyn TyHirs) -> Self {
        Self {
            db,
            return_infs: Default::default(),
        }
    }
}

impl<'a> TypeResolver for ReturnResolver<'a> {
    type DB = dyn TyHirs + 'a;

    fn db(&self) -> &Self::DB {
        self.db
    }

    fn field_type(&self, _: &NominalInfHead, _: FieldName) -> Result<EitherType, TypeError> {
        Ok(self.db.intern_type(Type::Unknown).into())
    }

    fn deref(&self, ty: &NominalInfHead) -> Result<Option<TypeId>, TypeError> {
        if self.return_infs.get(&ty.def).is_some() {
            Ok(Some(self.db.intern_type(Type::Unknown)))
        } else {
            let id = match NominalId::from_nominal_inf_head(ty) {
                NominalId::Def(d) => d,
                NominalId::Block(_) => return Ok(None),
            };
            Ok(Some(self.db.parser_returns(id)?.deref))
        }
    }

    fn signature(&self, ty: &NominalInfHead) -> Result<Signature, TypeError> {
        get_signature(self.db, ty)
    }

    fn lookup(&self, context: HirId, name: FieldName) -> Result<EitherType, TypeError> {
        get_thunk(self.db, context, name)
    }

    fn name(&self) -> String {
        let mut ret = String::from("returns for (");
        for (i, id) in self.return_infs.keys().enumerate() {
            if i > 0 {
                ret.push_str(", ");
            }
            ret.push_str(&dbformat!(self.db, "{}, ", id));
        }
        ret.push(')');
        ret
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefType {
    pub id: hir::ParserDefId,
    pub deref: TypeId,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefTypeInf {
    pub id: hir::ParserDefId,
    pub deref: InfTypeId,
}

#[cfg(test)]
mod tests {
    use crate::{context::Context, databased_display::DatabasedDisplay};

    use super::*;
    #[test]
    fn return_types() {
        let ctx = Context::mock(
            r#"
def for['t] *> nil = {}
def nil *> expr1 = {}
def for[int] *> single = ~
            "#,
        );
        let return_type = |name| {
            let p = ctx.parser(name);
            ctx.db
                .parser_returns(p)
                .unwrap()
                .deref
                .to_db_string(&ctx.db)
        };
        assert_eq!(
            return_type("nil"),
            "<anonymous block for['t] &> file[_].nil.1.0>"
        );
        assert_eq!(
            return_type("expr1"),
            "<anonymous block for['1] &> file[_].nil &> file[_].expr1.1.0>"
        );
        assert_eq!(return_type("single"), "int");
    }
}