use super::*;

pub fn parser_args(db: &dyn TyHirs, id: ParserDefId) -> Result<Signature, TypeError> {
    let pd = id.lookup(db).map_err(|_| TypeError)?;
    let mut context = TypingLocation {
        vars: TypeVarCollection::new_empty(),
        loc: db.hir_parent_module(id.0).map_err(|_| TypeError)?.0,
        pd: id,
    };
    let arg_resolver = ArgResolver::new(db);
    let mut tcx = TypingContext::new(db, arg_resolver);
    let from_expr = pd.from.lookup(db).map_err(|_| TypeError)?;
    let from_infty = tcx.resolve_type_expr(&mut context, &from_expr.expr)?;
    let from_ty = tcx.inftype_to_concrete_type(from_infty)?;
    let args = Arc::new(vec![]);
    let thunk = db.intern_type(Type::Nominal(NominalTypeHead {
        kind: NominalKind::Def,
        def: id.0,
        parse_arg: Some(from_ty),
        fun_args: args.clone(),
        ty_args: Arc::new(vec![]),
    }));
    Ok(Signature {
        ty_args: Arc::new(context.vars.defs),
        from: Some(from_ty),
        args,
        thunk,
    })
}

pub struct ArgResolver<'a>(&'a dyn TyHirs);

impl<'a> ArgResolver<'a> {
    pub fn new(db: &'a dyn TyHirs) -> Self {
        ArgResolver(db)
    }
}

impl<'a> TypeResolver for ArgResolver<'a> {
    fn field_type(&self, _ty: &NominalInfHead, _name: FieldName) -> Result<EitherType, ()> {
        Ok(self.0.intern_type(Type::Unknown).into())
    }

    fn deref(&self, _ty: &NominalInfHead) -> Result<Option<TypeId>, ()> {
        Ok(Some(self.0.intern_type(Type::Unknown)))
    }

    fn signature(&self, ty: &NominalInfHead) -> Result<Signature, ()> {
        get_signature(self.0, ty)
    }

    fn lookup(&self, _context: HirId, _name: FieldName) -> Result<EitherType, ()> {
        Ok(self.0.intern_type(Type::Unknown).into())
    }

    type DB = dyn TyHirs + 'a;

    fn db(&self) -> &Self::DB {
        self.0
    }
}

pub fn get_signature(db: &dyn TyHirs, ty: &NominalInfHead) -> Result<Signature, ()> {
    let id = match NominalId::from_nominal_inf_head(ty) {
        NominalId::Def(d) => d,
        NominalId::Block(_) => panic!("attempted to extract signature directly from block"),
    };
    db.parser_args(id).map_err(|_| ())
}

pub fn get_thunk(db: &dyn TyHirs, context: HirId, name: FieldName) -> Result<EitherType, ()> {
    let pd = parserdef_ref(db, context, name).map_err(|_| ())?;
    let args = db.parser_args(pd).map_err(|_| ())?;
    let mut ret = args.thunk;
    if let Some(from) = args.from {
        ret = db.intern_type(Type::ParserArg {
            result: ret,
            arg: from,
        })
    }
    if !args.args.is_empty() {
        ret = db.intern_type(Type::FunctionArg(ret, args.args.clone()))
    }
    if !args.ty_args.is_empty() {
        ret = db.intern_type(Type::ForAll(ret, args.ty_args.clone()))
    }
    Ok(ret.into())
}

#[cfg(test)]
mod tests {
    use crate::{context::Context, databased_display::DatabasedDisplay};

    use super::*;
    #[test]
    fn arg_types() {
        let ctx = Context::mock(
            r#"
def for[int] *> expr1 = {}
def for[for[int] &> expr1] *> expr2 = {}
def for['x] *> expr3 = {}
def each[for[int] *> expr2] *> expr4 = {}
            "#,
        );
        let arg_type = |name| {
            let p = ctx.parser(name);
            ctx.db
                .parser_args(p)
                .unwrap()
                .from
                .unwrap()
                .to_db_string(&ctx.db)
        };
        assert_eq!("for[int]", arg_type("expr1"));
        assert_eq!("for[for[int] &> file[anonymous].expr1]", arg_type("expr2"));
        assert_eq!("for['x]", arg_type("expr3"));
        assert_eq!("each[for[int] *> file[anonymous].expr2]", arg_type("expr4"));
    }
}
