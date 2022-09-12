use crate::{
    dbformat,
    error::{SResult, Silencable, SilencedError},
    expr::ExpressionHead,
};

use super::{signature::get_parserdef, *};

pub fn public_expr_type(db: &dyn TyHirs, loc: hir::ExprId) -> SResult<(TypedExpression, TypeId)> {
    public_expr_type_impl(db, loc).silence()
}

fn public_expr_type_impl(
    db: &dyn TyHirs,
    loc: hir::ExprId,
) -> Result<(TypedExpression, TypeId), SpannedTypeError> {
    let spanned = |x| SpannedTypeError::new(x, IndirectSpan::default_span(loc.0));
    let bump = Bump::new();
    let mut ctx = PublicResolver::new_typing_context_and_loc(db, loc.0, &bump)?;
    let mut typeloc = ctx.infctx.tr.tloc.clone();
    let parent = loc.0.parent(db);
    let surrounding_types = match db.hir_node(parent)? {
        hir::HirNode::ParserDef(pd) => ctx.parserdef_types(&pd)?,
        hir::HirNode::Let(l) => ctx.let_statement_types(&l)?,
        hir::HirNode::Parse(p) => ctx.parse_statement_types(&p)?,
        hir::HirNode::Array(_) => unimplemented!(),
        _ => panic!("expected parse statement, let statement or parser def"),
    };
    let resolved_expr = db.resolve_expr(loc)?;
    let expr = ctx.val_expression_type(&mut typeloc, &resolved_expr, loc)?;
    let root = expr.0.root_data().0;
    let into_ret = if let Some(ty) = surrounding_types.from_type {
        ctx.infctx.parser_apply(root, ty).map_err(spanned)?
    } else {
        root
    };
    let ret = if let Some(real_ret) = surrounding_types.root_type {
        ctx.infctx.constrain(into_ret, real_ret).map_err(spanned)?;
        real_ret
    } else {
        into_ret
    };
    let ret = ctx.inftype_to_concrete_type(ret).map_err(spanned)?;
    Ok((ctx.expr_to_concrete_type(&expr, loc)?, ret))
}

pub fn public_type(db: &dyn TyHirs, loc: DefId) -> SResult<TypeId> {
    public_type_impl(db, loc).silence()
}

fn public_type_impl(db: &dyn TyHirs, loc: DefId) -> Result<TypeId, TypeError> {
    let node = db.hir_node(loc)?;
    let ret = match node {
        hir::HirNode::Let(l) => public_expr_type(db, l.expr)?.1,
        hir::HirNode::Parse(p) => db.public_expr_type(p.expr)?.1,
        hir::HirNode::ChoiceIndirection(ind) => {
            let bump = Bump::new();
            let mut ctx =
                PublicResolver::new_typing_context_and_loc(db, ind.parent_context.0, &bump)?;
            let ret = ctx.infctx.var();
            for (_, choice_id) in ind.choices.iter() {
                // todo
                let choice_ty = db.public_type(*choice_id)?;
                let intfy = ctx.infctx.from_type(choice_ty);
                ctx.infctx.constrain(intfy, ret)?;
            }
            ctx.inftype_to_concrete_type(ret)?
        }
        _ => panic!("unexpected node type"),
    };
    Ok(ret)
}

/// finds the public ambient type (ie the type that gets applied as the from argument
/// to the parser) for the parser at loc, which is inside a block
pub fn ambient_type(db: &dyn TyHirs, loc: hir::ParseId) -> SResult<TypeId> {
    ambient_type_impl(db, loc).silence()
}

fn ambient_type_impl(db: &dyn TyHirs, loc: hir::ParseId) -> Result<TypeId, TypeError> {
    let block = loc
        .lookup(db)?
        .parent_context
        .lookup(db)?
        .block_id
        .lookup(db)?;
    let (typed_expr, _) = db.public_expr_type(block.enclosing_expr)?;
    let block_ty = ExprIter::new(&typed_expr)
        .find_map(|x| match &x.0 {
            ExpressionHead::Niladic(expr::OpWithData {
                inner: ResolvedAtom::Block(b),
                data,
            }) if *b == block.id => Some(data.0),
            _ => None,
        })
        .ok_or(SilencedError)?;
    let block_type = match db.lookup_intern_type(block_ty) {
        Type::ParserArg { result, .. } => result,
        _ => panic!("expected parser arg"),
    };
    match db.lookup_intern_type(block_type) {
        Type::Nominal(NominalTypeHead {
            kind: NominalKind::Block,
            parse_arg: Some(parse_ty),
            ..
        }) => Ok(parse_ty),
        _ => panic!("expected block"),
    }
}

impl<'a, 'intern> TypingContext<'a, 'intern, PublicResolver<'a>> {
    pub fn parse_statement_types(
        &mut self,
        parse: &ParseStatement,
    ) -> Result<ExpressionTypeConstraints<'intern>, SpannedTypeError> {
        let from = self.db.ambient_type(parse.id)?;
        let infty = self.infctx.from_type(from);
        Ok(ExpressionTypeConstraints {
            from_type: Some(infty),
            root_type: None,
        })
    }
    pub fn let_statement_types(
        &mut self,
        let_statement: &hir::LetStatement,
    ) -> Result<ExpressionTypeConstraints<'intern>, SpannedTypeError> {
        let ty = let_statement.ty;
        let ty_expr = ty.lookup(self.db)?.expr;
        let mut typeloc = self.infctx.tr.tloc.clone();
        let infty = self.resolve_type_expr(&mut typeloc, &ty_expr, ty)?;
        Ok(ExpressionTypeConstraints {
            root_type: Some(infty),
            from_type: None,
        })
    }
    pub fn parserdef_types(
        &mut self,
        parserdef: &hir::ParserDef,
    ) -> Result<ExpressionTypeConstraints<'intern>, SpannedTypeError> {
        let ty_expr = parserdef.from.lookup(self.db)?.expr;
        let mut typeloc = self.infctx.tr.tloc.clone();
        let from = self.resolve_type_expr(&mut typeloc, &ty_expr, parserdef.from)?;
        Ok(ExpressionTypeConstraints {
            root_type: None,
            from_type: Some(from),
        })
    }
}

pub struct PublicResolver<'a> {
    db: &'a dyn TyHirs,
    tloc: TypingLocation,
}

impl<'a> PublicResolver<'a> {
    pub fn new(db: &'a dyn TyHirs, tloc: TypingLocation) -> Self {
        Self { db, tloc }
    }
    pub fn new_typing_context_and_loc<'intern>(
        db: &'a dyn TyHirs,
        loc: DefId,
        bump: &'intern Bump,
    ) -> SResult<TypingContext<'a, 'intern, Self>> {
        let pd = db.hir_parent_parserdef(loc)?;
        let vars = TypeVarCollection::at_id(db, pd)?;
        let typeloc = TypingLocation { vars, loc, pd };
        let public_resolver = Self::new(db, typeloc);
        Ok(TypingContext::new(db, public_resolver, &bump))
    }
}

impl<'a, 'intern> TypeResolver<'intern> for PublicResolver<'a> {
    fn field_type(
        &self,
        _ty: &NominalInfHead<'intern>,
        _name: FieldName,
    ) -> Result<EitherType<'intern>, TypeError> {
        Ok(self.db.intern_type(Type::Unknown).into())
    }

    fn deref(&self, ty: &NominalInfHead<'intern>) -> Result<Option<TypeId>, TypeError> {
        let id = match NominalId::from_nominal_inf_head(ty) {
            NominalId::Def(d) => d,
            NominalId::Block(_) => return Ok(None),
        };
        Ok(Some(self.db.parser_returns(id)?.deref))
    }

    fn signature(&self, ty: &NominalInfHead<'intern>) -> Result<Signature, TypeError> {
        get_signature(self.db, ty)
    }

    fn lookup(&self, _val: DefId) -> Result<EitherType<'intern>, TypeError> {
        Err(SilencedError.into())
    }

    type DB = dyn TyHirs + 'a;

    fn db(&self) -> &Self::DB {
        self.db
    }

    fn name(&self) -> String {
        dbformat!(self.db, "public at {}", &self.tloc.pd.0)
    }

    fn parserdef(&self, pd: DefId) -> Result<EitherType<'intern>, TypeError> {
        get_parserdef(self.db(), pd).map(|x| x.into())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        context::Context, databased_display::DatabasedDisplay, interner::PathComponent,
        types::TypeInterner,
    };

    use super::*;
    #[test]
    fn public_types() {
        let ctx = Context::mock(
            r#"
def for['t] *> expr1 = {
    a: ~,
    b: ~,
    c: {
        let d: int = 2,
        e: ~,
        ;
        let d: int = 1,
    },
}
def each[int] *> expr2 = {
    x: expr1,
    (let y: int = 3,; y: ~,)
}
def for[int] *> expr3 = ~
def for[int] *> expr4 = {
    x: expr3,
    ;
    let x: int = 3,
}
def for[for[int]] *> expr5 = {
    x: ~ |> expr3,
}
            "#,
        );
        let public_type = |name: &str, fields: &[&str]| {
            let p = ctx.parser(name);
            let mut ret = ctx.db.parser_returns(p).unwrap().deref;
            for x in fields {
                let block = ctx.db.lookup_intern_type(ret);
                let def_id = match &block {
                    Type::Nominal(n) => n.def,
                    _ => panic!("expected nominal type"),
                };
                let block = hir::BlockId::extract(ctx.db.hir_node(def_id).unwrap());
                let root_context = block.root_context;
                let ident_field = ctx.id(x);
                let child = root_context
                    .0
                    .child(&ctx.db, PathComponent::Named(FieldName::Ident(ident_field)));
                ret = ctx.db.public_type(child).unwrap();
            }
            ret.to_db_string(&ctx.db)
        };
        assert_eq!("'t", public_type("expr1", &["a"]));
        assert_eq!("'t", public_type("expr1", &["b"]));
        assert_eq!(
            "<anonymous block for['t] &> file[_].expr1.1.0.0.c.0.0>",
            public_type("expr1", &["c"])
        );
        assert_eq!("int", public_type("expr1", &["c", "d"]));
        assert_eq!("'t", public_type("expr1", &["c", "e"]));
        assert_eq!("for[int] &> file[_].expr1", public_type("expr2", &["x"]));
        assert_eq!("int", public_type("expr2", &["y"]));
        assert_eq!("int", public_type("expr4", &["x"]));
        assert_eq!("for[int] &> file[_].expr3", public_type("expr5", &["x"]));
    }
}
