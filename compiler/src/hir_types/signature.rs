use crate::{error::SilencedError, hir::refs::parserdef_ref};

use super::*;

pub fn parser_args(db: &dyn TyHirs, id: hir::ParserDefId) -> SResult<Signature> {
    parser_args_impl(db, id).silence()
}

pub(super) fn parser_args_impl(db: &dyn TyHirs, id: hir::ParserDefId) -> Result<Signature, TypeError> {
    let pd = id.lookup(db)?;
    let mut context = TypingLocation {
        vars: TypeVarCollection::new_empty(),
        loc: db.hir_parent_module(id.0)?.0,
        pd: id,
    };
    let arg_resolver = ArgResolver::new(db);
    let mut tcx = TypingContext::new(db, arg_resolver);
    let from_expr = pd.from.lookup(db)?;
    let args = Arc::new(vec![]);
    let from_infty = tcx.resolve_type_expr(&mut context, &from_expr.expr)?;
    let (from_tys, count) =
        tcx.infctx
            .to_types_with_vars(&[from_infty][..], context.vars.defs.len() as u32, id.0)?;
    context.vars.fill_anon_vars(db, count);
    let ty_args = context.vars.var_types(db, id);
    let thunk = db.intern_type(Type::Nominal(NominalTypeHead {
        kind: NominalKind::Def,
        def: id.0,
        parse_arg: from_tys.last().copied(),
        fun_args: args.clone(),
        ty_args: Arc::new(ty_args),
    }));
    Ok(Signature {
        ty_args: Arc::new(context.vars.defs),
        from: from_tys.last().copied(),
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
    fn field_type(&self, _ty: &NominalInfHead, _name: FieldName) -> Result<EitherType, TypeError> {
        Ok(self.0.intern_type(Type::Unknown).into())
    }

    fn deref(&self, _ty: &NominalInfHead) -> Result<Option<TypeId>, TypeError> {
        Ok(Some(self.0.intern_type(Type::Unknown)))
    }

    fn signature(&self, ty: &NominalInfHead) -> Result<Signature, TypeError> {
        get_signature(self.0, ty)
    }

    fn lookup(&self, _context: DefId, _name: FieldName) -> Result<EitherType, TypeError> {
        Ok(self.0.intern_type(Type::Unknown).into())
    }

    type DB = dyn TyHirs + 'a;

    fn db(&self) -> &Self::DB {
        self.0
    }

    fn name(&self) -> String {
        String::from("signature")
    }
}

pub fn get_signature(db: &dyn TyHirs, ty: &NominalInfHead) -> Result<Signature, TypeError> {
    let id = match NominalId::from_nominal_inf_head(ty) {
        NominalId::Def(d) => d,
        NominalId::Block(_) => panic!("attempted to extract signature directly from block"),
    };
    Ok(db.parser_args(id)?)
}

pub fn get_thunk(
    db: &dyn TyHirs,
    context: DefId,
    name: FieldName,
) -> Result<EitherType, TypeError> {
    let pd = parserdef_ref(db, context, name)?.ok_or(SilencedError)?;
    let args = db.parser_args(pd)?;
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
    ret = attach_forall(db, ret, &args.ty_args);
    Ok(ret.into())
}

pub fn attach_forall(db: &dyn TyHirs, ty: TypeId, ty_args: &Arc<Vec<TypeVar>>) -> TypeId {
    if !db.type_contains_typevar(ty) {
        return ty;
    }
    db.intern_type(Type::ForAll(ty, ty_args.clone()))
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
def each[for[expr1] *> expr2] *> expr4 = {}
def each[expr3] *> expr5 = {}
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
        assert_eq!("for[for[int] &> file[_].expr1]", arg_type("expr2"));
        assert_eq!("for['x]", arg_type("expr3"));
        assert_eq!("each[for[for[int] &> file[_].expr1] *> for[for[int] &> file[_].expr1] &> file[_].expr2]", arg_type("expr4"));
        assert_eq!("each[for['1] &> file[_].expr3]", arg_type("expr5"));
    }
}
