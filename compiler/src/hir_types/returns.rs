use crate::{
    dbformat,
    error::{SResult, SilencedError},
    resolve::parserdef_ssc::FunctionSscId,
};

use super::{signature::get_parserdef, *};

pub fn deref_type(db: &dyn TyHirs, ty: TypeId) -> SResult<Option<TypeId>> {
    match db.lookup_intern_type(ty) {
        Type::ForAll(inner, vars) => {
            let inner_deref = match db.deref_type(inner)? {
                Some(t) => t,
                None => return Ok(None),
            };
            Ok(Some(db.intern_type(Type::ForAll(inner_deref, vars))))
        }
        Type::Nominal(nom) => {
            let id = match NominalId::from_nominal_head(&nom) {
                NominalId::Def(id) => id,
                NominalId::Block(_) => return Ok(None),
            };
            let deref_ty = db.parser_returns(id)?.deref;
            let subst_deref_ty = db.substitute_typevar(deref_ty, nom.ty_args);
            Ok(Some(subst_deref_ty))
        }
        _ => Ok(None),
    }
}

pub fn least_deref_type(db: &dyn TyHirs, mut ty: TypeId) -> SResult<TypeId> {
    Ok(loop {
        match db.deref_type(ty)? {
            Some(t) => ty = t,
            None => break ty,
        }
    })
}

pub fn parser_returns(db: &dyn TyHirs, id: hir::ParserDefId) -> SResult<ParserDefType> {
    db.parser_returns_ssc(db.parser_ssc(id)?)
        .into_iter()
        .flatten()
        .find(|x| x.id.0 == id.0)
        .ok_or(SilencedError)
}

pub fn parser_returns_ssc(
    db: &dyn TyHirs,
    id: FunctionSscId,
) -> Vec<Result<ParserDefType, SpannedTypeError>> {
    let def_ids = db.lookup_intern_recursion_scc(id);
    let defs = def_ids
        .iter()
        .flat_map(|fun: &hir::ParserDefId| fun.lookup(db))
        .collect::<Vec<_>>();

    let resolver = ReturnResolver::new(db);
    let bump = Bump::new();
    let placeholder_id = defs[0].id;
    let loc = match TypingLocation::at_id(db, placeholder_id.0) {
        Ok(loc) => loc,
        Err(e) => return vec![Err(e.into())],
    };
    let mut ctx = TypingContext::new(db, resolver, loc, &bump, false);

    let mut vars = FxHashMap::default();
    let defs: Vec<_> = defs
        .into_iter()
        .map(|def| {
            // in this loop we do not directly the inference variables we are creating yet
            ctx.initialize_vars_at(def.id.0, &mut vars).and(Ok(def))
        })
        .collect();
    ctx.inftypes = Rc::new(vars);
    ctx.infctx.tr.return_infs = ctx.inftypes.clone();
    let defs = defs
        .into_iter()
        .map(|def| -> Result<hir::ParserDef, SpannedTypeError> {
            def.and_then(|def| {
                // in this separate loop we do the actual type inference
                ctx.loc = TypingLocation::at_id(db, def.id.0)?;
                ctx.type_parserdef(def.id)?;
                Ok(def)
            })
        })
        .collect::<Vec<_>>();
    defs.into_iter()
        .map(|def| -> Result<ParserDefType, SpannedTypeError> {
            def.and_then(|def| {
                let spanned = |e| SpannedTypeError::new(e, IndirectSpan::default_span(def.to.0));
                // here we are finished with inference so we can convert to actual types
                let deref = ctx
                    .inftype_to_concrete_type(ctx.inftypes[&def.id.0])
                    .map_err(spanned)?;
                Ok(ParserDefType { id: def.id, deref })
            })
        })
        .collect()
}

pub struct ReturnResolver<'a> {
    db: &'a dyn TyHirs,
    return_infs: Rc<FxHashMap<DefId, InfTypeId<'a>>>,
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

impl<'a> TypeResolver<'a> for ReturnResolver<'a> {
    type DB = dyn TyHirs + 'a;

    fn db(&self) -> &Self::DB {
        self.db
    }

    fn field_type(
        &self,
        _: &NominalInfHead<'a>,
        _: FieldName,
    ) -> Result<EitherType<'a>, TypeError> {
        Ok(self.db.intern_type(Type::Unknown).into())
    }

    fn deref(&self, ty: &NominalInfHead<'a>) -> Result<Option<TypeId>, TypeError> {
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

    fn signature(&self, ty: &NominalInfHead<'a>) -> Result<Signature, TypeError> {
        get_signature(self.db, ty)
    }

    fn lookup(&self, val: DefId) -> Result<EitherType<'a>, TypeError> {
        self.return_infs
            .get(&val)
            .map_or_else(|| Err(SilencedError.into()), |inf| Ok((*inf).into()))
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

    fn parserdef(&self, pd: DefId) -> Result<EitherType<'a>, TypeError> {
        get_parserdef(self.db(), pd).map(|x| x.into())
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefType {
    pub id: hir::ParserDefId,
    pub deref: TypeId,
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
    #[test]
    fn block_with_return() {
        let ctx = Context::mock(
            r#"
def for[int] *> u16l = {
    low: ~,
    high: ~,
    let return: int = low + high * 256,
}
            "#,
        );
        let parser = ctx.parser("u16l");
        let ret = ctx.db.parser_returns(parser).unwrap().deref;
        assert_eq!(ret.to_db_string(&ctx.db), "int");
    }
}
