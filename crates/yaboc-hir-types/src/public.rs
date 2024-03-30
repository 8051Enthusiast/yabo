use yaboc_base::dbformat;
use yaboc_expr::{FetchExpr, FetchKindData};

use super::*;

impl<DB: TyHirs + ?Sized> FetchKindData<PubTypeId, ExprId, DB> for Resolved {
    type Data = Arc<ShapedData<Vec<TypeId>, Resolved>>;
    type Err = SilencedError;

    fn fetch_kind_data(db: &DB, id: ExprId) -> Result<Self::Data, Self::Err> {
        Ok(db.public_expr_type(id)?.0)
    }
}

pub fn public_expr_type(db: &dyn TyHirs, loc: hir::ExprId) -> SResult<(Arc<ExprTypeData>, TypeId)> {
    public_expr_type_impl(db, loc).silence()
}

fn public_expr_type_impl(
    db: &dyn TyHirs,
    loc: hir::ExprId,
) -> Result<(Arc<ExprTypeData>, TypeId), SpannedTypeError> {
    let spanned = |x| SpannedTypeError::new(x, IndirectSpan::default_span(loc.0));
    let bump = Bump::new();
    let mut ctx = PublicResolver::new_typing_context_and_loc(db, loc.0, &bump)?;
    let parent = loc.0.parent(db).unwrap();
    let (ambient, ret) = match db.hir_node(parent)? {
        hir::HirNode::Let(l) => (None, ctx.let_statement_types(&l)?),
        hir::HirNode::Parse(p) => (ctx.parse_statement_types(&p)?, None),
        hir::HirNode::ParserDef(pd) => {
            let ret = db.parser_returns(pd.id)?.deref;
            let ret = ctx.infctx.convert_type_into_inftype(ret);
            (ctx.parserdef_types(&pd)?, Some(ret))
        }
        _ => panic!("expected parse statement, let statement or parser def"),
    };
    let mut vars = FxHashMap::default();
    let parserdef = ctx.loc.pd;
    ctx.initialize_vars_at(loc.0, &mut vars)?;
    ctx.initialize_parserdef_args(parserdef, &mut vars)?;
    ctx.inftypes = Rc::new(vars);
    ctx.infctx.tr.inftys = ctx.inftypes.clone();
    let ret = ret.unwrap_or_else(|| ctx.infctx.var());
    ctx.set_ambient_type(ambient);
    let val_expr = loc.lookup(db)?;
    let expr_ret = ctx.type_expr(&val_expr)?;
    ctx.infctx.constrain(expr_ret, ret).map_err(spanned)?;
    let ret = ctx.inftype_to_concrete_type(ret).map_err(spanned)?;
    let expr = ctx.inf_expressions[&loc].clone();
    Ok((Arc::new(ctx.expr_to_concrete_type(&expr, loc)?), ret))
}

pub fn public_type(db: &dyn TyHirs, loc: DefId) -> SResult<TypeId> {
    public_type_impl(db, loc).silence()
}

fn public_type_impl(db: &dyn TyHirs, loc: DefId) -> Result<TypeId, TypeError> {
    let node = db.hir_node(loc)?;
    let ret = match node {
        hir::HirNode::Let(l) => db.public_expr_type(l.expr)?.1,
        hir::HirNode::Parse(p) => db.public_expr_type(p.expr)?.1,
        hir::HirNode::ChoiceIndirection(ind) => {
            let bump = Bump::new();
            let mut ctx =
                PublicResolver::new_typing_context_and_loc(db, ind.parent_context.0, &bump)?;
            let ret = ctx.infctx.var();
            for (_, choice_id) in ind.choices.iter() {
                // todo
                let choice_ty = db.public_type(*choice_id)?;
                let intfy = ctx.infctx.convert_type_into_inftype(choice_ty);
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
    let expr = Resolved::expr_with_data::<(SpanIndex, PubTypeId)>(db, block.enclosing_expr)?;
    let block_ty = expr
        .take_ref()
        .iter_parts()
        .find_map(|(head, (_, ty))| match head {
            ExprHead::Niladic(ResolvedAtom::Block(b, _)) if b == block.id => Some(*ty),
            _ => None,
        })
        .ok_or_else(SilencedError::new)?;
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

impl<'a, 'intern> TypingContext<'a, 'intern, PublicResolver<'a, 'intern>> {
    pub fn parse_statement_types(
        &mut self,
        parse: &ParseStatement,
    ) -> Result<Option<InfTypeId<'intern>>, SpannedTypeError> {
        let from = self.db.ambient_type(parse.id)?;
        let infty = self.infctx.convert_type_into_inftype(from);
        Ok(Some(infty))
    }
    pub fn parserdef_types(
        &mut self,
        parserdef: &hir::ParserDef,
    ) -> Result<Option<InfTypeId<'intern>>, SpannedTypeError> {
        let Some(from_expr) = parserdef.from else {
            return Ok(None);
        };
        let ty_expr = from_expr.lookup(self.db)?.expr;
        let from = self.resolve_type_expr(ty_expr.take_ref(), from_expr)?;
        Ok(Some(from))
    }
}

pub struct PublicResolver<'a, 'intern> {
    db: &'a dyn TyHirs,
    inftys: Rc<FxHashMap<DefId, InfTypeId<'intern>>>,
    name: String,
}

impl<'a, 'intern> PublicResolver<'a, 'intern> {
    pub fn new(db: &'a dyn TyHirs, name: String) -> Self {
        Self {
            db,
            name,
            inftys: Default::default(),
        }
    }
    pub fn new_typing_context_and_loc(
        db: &'a dyn TyHirs,
        loc: DefId,
        bump: &'intern Bump,
    ) -> SResult<TypingContext<'a, 'intern, Self>> {
        let pd = db.hir_parent_parserdef(loc)?;
        let vars = TypeVarCollection::at_id(db, pd)?;
        let typeloc = TypingLocation { vars, loc, pd };
        let name = dbformat!(db, "public at {}", &loc);
        let public_resolver = Self::new(db, name);
        Ok(TypingContext::new(
            db,
            public_resolver,
            typeloc,
            bump,
            false,
        ))
    }
}

impl<'a, 'intern> TypeResolver<'intern> for PublicResolver<'a, 'intern> {
    fn field_type(
        &self,
        _ty: &NominalInfHead<'intern>,
        _name: FieldName,
    ) -> Result<EitherType<'intern>, TypeError> {
        Ok(self.db.intern_type(Type::Unknown).into())
    }

    fn deref(
        &self,
        ty: &NominalInfHead<'intern>,
    ) -> Result<Option<EitherType<'intern>>, TypeError> {
        let id = match NominalId::from_nominal_inf_head(ty) {
            NominalId::Def(d) => d,
            NominalId::Block(_) => return Ok(None),
        };
        Ok(Some(EitherType::Regular(self.db.parser_returns(id)?.deref)))
    }

    fn signature(&self, id: DefId) -> Result<Signature, TypeError> {
        get_signature(self.db, id)
    }

    fn lookup(&self, val: DefId) -> Result<EitherType<'intern>, TypeError> {
        self.inftys
            .get(&val)
            .map_or_else(|| Err(SilencedError::new().into()), |inf| Ok((*inf).into()))
    }

    type DB = dyn TyHirs + 'a;

    fn db(&self) -> &Self::DB {
        self.db
    }

    fn name(&self) -> String {
        self.name.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::HirTypesTestDatabase;
    use hir::Parser;
    use yaboc_ast::import::Import;
    use yaboc_base::databased_display::DatabasedDisplay;
    use yaboc_base::interner::PathComponent;
    use yaboc_base::Context;
    use yaboc_types::TypeInterner;

    use super::*;
    #[test]
    fn public_types() {
        let ctx = Context::<HirTypesTestDatabase>::mock(
            r#"
def ['t] *> expr1 = {
    a: ~
    b: ~
    c: {
      | let d: int = 2
        e: ~
      | let d: int = 1
    }
}
def *expr2 = {
    x: expr1
    | let y: int = 3
    | y: ~
}
def *expr3 = ~
def *expr4 = {
    | x: expr3
    | let x: int = 3
}
def [[u8]] *> expr5 = {
    x: ~ |> expr3
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
        assert_eq!("'0", public_type("expr1", &["a"]));
        assert_eq!("'0", public_type("expr1", &["b"]));
        assert_eq!(
            "<anonymous block ['0] &> file[_].expr1.1.0.0.c.0.0>",
            public_type("expr1", &["c"])
        );
        assert_eq!("int", public_type("expr1", &["c", "d"]));
        assert_eq!("'0", public_type("expr1", &["c", "e"]));
        assert_eq!("[u8] &> file[_].expr1", public_type("expr2", &["x"]));
        assert_eq!("int", public_type("expr2", &["y"]));
        assert_eq!("int", public_type("expr4", &["x"]));
        //assert_eq!("[u8] &> file[_].expr3", public_type("expr5", &["x"]));
    }
}
