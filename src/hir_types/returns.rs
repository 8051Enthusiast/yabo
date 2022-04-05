use super::*;

pub fn parser_returns(db: &dyn TyHirs, id: ParserDefId) -> Result<ParserDefType, TypeError> {
    let rets = db.parser_returns_ssc(db.parser_ssc(id).map_err(|_| TypeError)?)?;
    for def in rets {
        if def.id.0 == id.0 {
            return Ok(def);
        }
    }
    panic!("Not included in ssc even though it should")
}

pub fn parser_returns_ssc(
    db: &dyn TyHirs,
    id: FunctionSscId,
) -> Result<Vec<ParserDefType>, TypeError> {
    let def_ids = db.lookup_intern_recursion_scc(id);
    let defs = def_ids
        .iter()
        .map(|fun: &ParserDefId| fun.lookup(db))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_| TypeError)?;

    let resolver = ReturnResolver::new(db);
    let mut ctx = TypingContext::new(db, resolver);

    for def in defs.iter() {
        // in this loop we do not directly the inference variables we are creating yet
        let deref = ctx.infctx.var();
        let retinf = ParserDefTypeInf { id: def.id, deref };
        ctx.infctx.tr.return_infs.insert(retinf.id.0, retinf);
    }
    for def in defs.iter() {
        // in this separate loop we do the actual type inference
        let expr = def.to.lookup(db).map_err(|_| TypeError)?.expr;
        let mut context = TypingLocation {
            vars: TypeVarCollection::at_id(db, def.id)?,
            loc: db.hir_parent_module(def.id.0).map_err(|_| TypeError)?.0,
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
    }
    let mut ret = Vec::new();
    for def in defs.iter() {
        // here we are finished with inference so we can convert to actual types
        let deref = ctx.inftype_to_concrete_type(ctx.infctx.tr.return_infs[&def.id.0].deref)?;
        ret.push(ParserDefType { id: def.id, deref })
    }
    Ok(ret)
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

    fn field_type(&self, _: &NominalInfHead, _: FieldName) -> Result<EitherType, ()> {
        Ok(self.db.intern_type(Type::Unknown).into())
    }

    fn deref(&self, ty: &NominalInfHead) -> Result<Option<TypeId>, ()> {
        if self.return_infs.get(&ty.def).is_some() {
            Ok(Some(self.db.intern_type(Type::Unknown)))
        } else {
            let id = match NominalId::from_nominal_inf_head(ty) {
                NominalId::Def(d) => d,
                NominalId::Block(_) => return Ok(None),
            };
            Ok(Some(self.db.parser_returns(id).map_err(|_| ())?.deref))
        }
    }

    fn signature(&self, ty: &NominalInfHead) -> Result<Signature, ()> {
        get_signature(self.db, ty)
    }

    fn lookup(&self, context: HirId, name: FieldName) -> Result<EitherType, ()> {
        get_thunk(self.db, context, name)
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefType {
    pub id: ParserDefId,
    pub deref: TypeId,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefTypeInf {
    pub id: ParserDefId,
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
        eprintln!("{}", return_type("nil"));
        eprintln!("{}", return_type("expr1"));
        eprintln!("{}", return_type("single"));
    }
}