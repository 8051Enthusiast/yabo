use crate::{
    dbformat, dbpanic,
    error::{SResult, Silencable, SilencedError},
    expr::{ExpressionHead, OpWithData},
    hir::{walk::ChildIter, Block, ChoiceIndirection, StructChoice, StructCtx, ValExpression},
    interner::PathComponent,
    types::inference::{InfTypeId, NominalInfHead, TypeResolver},
};

use super::{signature::get_parserdef, *};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParserFullTypes {
    pub id: hir::ParserDefId,
    pub types: Arc<BTreeMap<DefId, TypeId>>,
    pub exprs: Arc<BTreeMap<hir::ExprId, TypedExpression>>,
}

pub fn parser_full_types(
    db: &dyn TyHirs,
    id: hir::ParserDefId,
) -> Result<Arc<ParserFullTypes>, SpannedTypeError> {
    let resolver = FullResolver::new(db, id.0)?;
    let bump = Bump::new();
    let mut ctx = TypingContext::new(db, resolver, &bump);
    ctx.initialize_vars()?;
    ctx.type_parserdef(id)?;
    let mut types = BTreeMap::new();
    let mut exprs = BTreeMap::new();
    for (&id, &infty) in ctx.infctx.tr.inftypes.clone().iter() {
        let ty = ctx
            .inftype_to_concrete_type(infty)
            .map_err(|e| SpannedTypeError::new(e, IndirectSpan::default_span(id)))?;
        types.insert(id, ty);
    }
    for (&id, expr) in ctx.infctx.tr.inf_expressions.clone().iter() {
        let expr = ctx.expr_to_concrete_type(expr, id)?;
        exprs.insert(id, expr);
    }
    Ok(Arc::new(ParserFullTypes {
        id,
        types: Arc::new(types),
        exprs: Arc::new(exprs),
    }))
}

pub fn parser_type_at(db: &dyn TyHirs, id: DefId) -> SResult<TypeId> {
    let parent_pd = db.hir_parent_parserdef(id)?;
    let types = db.parser_full_types(parent_pd).silence()?;
    let res = types.types.get(&id).copied().ok_or(SilencedError);
    res
}

pub fn parser_expr_at(db: &dyn TyHirs, id: hir::ExprId) -> SResult<TypedExpression> {
    let parent_pd = db.hir_parent_parserdef(id.0)?;
    let types = db.parser_full_types(parent_pd).silence()?;
    types.exprs.get(&id).cloned().ok_or(SilencedError)
}

impl<'a, 'intern> TypingContext<'a, 'intern, FullResolver<'a, 'intern>> {
    fn set_current_loc(&mut self, loc: DefId) {
        self.infctx.tr.loc.loc = loc;
    }
    fn infty_at(&mut self, id: DefId) -> InfTypeId<'intern> {
        self.infctx.tr.inftypes[&id]
    }
    fn let_statement_types(
        &mut self,
        let_statement: &hir::LetStatement,
    ) -> Result<ExpressionTypeConstraints<'intern>, SpannedTypeError> {
        let ty = let_statement.ty;
        let ty_expr = ty.lookup(self.db)?.expr;
        let mut typeloc = self.infctx.tr.loc.clone();
        let infty = self.resolve_type_expr(&mut typeloc, &ty_expr, ty)?;
        Ok(ExpressionTypeConstraints {
            root_type: Some(infty),
            from_type: None,
        })
    }
    fn init_var_at_loc(&mut self, infty: InfTypeId<'intern>) {
        self.infctx
            .tr
            .inftypes
            .insert(self.infctx.tr.loc.loc, infty);
    }
    fn initialize_vars(&mut self) -> Result<(), SpannedTypeError> {
        let pd = self.infctx.tr.loc.pd;
        for child in ChildIter::new(pd.0, self.db) {
            let ty = match child {
                hir::HirNode::Let(l) => {
                    self.set_current_loc(l.id.0);
                    if let Some(ty) = self.let_statement_types(&l)?.root_type {
                        ty
                    } else {
                        self.infctx.var()
                    }
                }
                hir::HirNode::Parse(parse) => {
                    self.set_current_loc(parse.id.0);
                    self.infctx.var()
                }
                hir::HirNode::ChoiceIndirection(choice) => {
                    self.set_current_loc(choice.id.0);
                    self.infctx.var()
                }
                _ => continue,
            };
            self.init_var_at_loc(ty);
        }
        self.constrain_public_types()
    }
    fn constrain_public_types(&mut self) -> Result<(), SpannedTypeError> {
        let pd = self.infctx.tr.loc.pd;
        for child_node in ChildIter::new(pd.0, self.db) {
            let span = IndirectSpan::default_span(child_node.id());
            let block = match child_node {
                hir::HirNode::Block(block) => block,
                _ => continue,
            };
            let root_ctx = block.root_context.lookup(self.db)?;
            for (_, child) in root_ctx.vars.iter() {
                let child = child.inner();
                let public_type = match self.db.public_type(*child) {
                    Ok(ty) => ty,
                    Err(_) => continue,
                };
                let public_inftype = self.infctx.from_type(public_type);
                let current_inftype = self.infctx.tr.inftypes[child];
                self.infctx
                    .constrain(current_inftype, public_inftype)
                    .map_err(|e| SpannedTypeError::new(e, span))?;
            }
        }
        Ok(())
    }
    fn set_ambient_type(&mut self, infty: Option<InfTypeId<'intern>>) {
        self.infctx.tr.current_ambient = infty;
    }
    fn ambient_type(&self) -> Option<InfTypeId<'intern>> {
        self.infctx.tr.current_ambient
    }
    fn with_ambient_type<T>(
        &mut self,
        infty: Option<InfTypeId<'intern>>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let old_ambient = self.ambient_type();
        self.set_ambient_type(infty);
        let ret = f(self);
        self.set_ambient_type(old_ambient);
        ret
    }
    fn with_loc<T>(&mut self, loc: DefId, f: impl FnOnce(&mut Self) -> T) -> T {
        let old_loc = self.infctx.tr.loc.loc;
        self.infctx.tr.loc.loc = loc;
        let ret = f(self);
        self.infctx.tr.loc.loc = old_loc;
        ret
    }
    fn type_parserdef(&mut self, pd: hir::ParserDefId) -> Result<(), SpannedTypeError> {
        let parserdef = pd.lookup(self.db)?;
        let sig = self.db.parser_args(pd)?;
        let ambient = sig.from.map(|ty| self.infctx.from_type(ty));
        self.set_ambient_type(ambient);

        let expr = parserdef.to.lookup(self.db)?;
        let ret = self.type_expr(&expr)?;
        let previous_ret = self
            .infctx
            .from_type(self.db.parser_returns(self.infctx.tr.loc.pd)?.deref);
        let return_spanned = |e| SpannedTypeError::new(e, IndirectSpan::default_span(pd.0));
        self.infctx
            .constrain(ret, previous_ret)
            .map_err(return_spanned)?;
        Ok(())
    }
    fn type_expr(&mut self, expr: &ValExpression) -> Result<InfTypeId<'intern>, SpannedTypeError> {
        let spanned = |e| SpannedTypeError::new(e, IndirectSpan::default_span(expr.id.0));
        let mut typeloc = self.infctx.tr.loc.clone();
        let resolved_expr = self.db.resolve_expr(expr.id)?;
        let inf_expression = self.val_expression_type(&mut typeloc, &resolved_expr, expr.id)?;
        let root = inf_expression.0.root_data().0;
        let ret = if let Some(ambient) = self.ambient_type() {
            self.infctx.parser_apply(root, ambient).map_err(spanned)?
        } else {
            root
        };
        for part in ExprIter::new(&inf_expression) {
            match &part.0 {
                ExpressionHead::Niladic(OpWithData {
                    data,
                    inner: ResolvedAtom::Block(block_id),
                }) => {
                    let spanned =
                        |e| SpannedTypeError::new(e, IndirectSpan::new(expr.id.0, data.1));
                    let ambient = self.infctx.reuse_parser_arg(data.0).map_err(spanned)?;
                    let block = block_id.lookup(self.db)?;
                    self.with_ambient_type(Some(ambient), |ctx| ctx.type_block(&block))?;
                }
                _ => continue,
            }
        }
        self.infctx
            .tr
            .inf_expressions
            .insert(expr.id, inf_expression);
        Ok(ret)
    }
    fn type_block(&mut self, block: &Block) -> Result<(), SpannedTypeError> {
        let root_ctx = block.root_context.lookup(self.db)?;
        self.type_context(&root_ctx)
    }
    fn type_context(&mut self, context: &StructCtx) -> Result<(), SpannedTypeError> {
        self.with_loc(context.id.0, |ctx| {
            for child in context.children.iter() {
                ctx.type_block_component(*child)?;
            }
            Ok(())
        })
    }
    fn type_block_component(&mut self, id: DefId) -> Result<(), SpannedTypeError> {
        match self.db.hir_node(id)? {
            hir::HirNode::Let(l) => self.type_let(&l),
            hir::HirNode::Parse(parse) => self.type_parse(&parse),
            hir::HirNode::Choice(choice) => self.type_choice(&choice),
            hir::HirNode::Context(context) => self.type_context(&context),
            hir::HirNode::ChoiceIndirection(ci) => self.type_choice_indirection(&ci),
            _ => unreachable!("Invalid type block component"),
        }
    }
    fn type_choice(&mut self, choice: &StructChoice) -> Result<(), SpannedTypeError> {
        for child in choice.subcontexts.iter() {
            let context = child.lookup(self.db)?;
            self.type_context(&context)?;
        }
        Ok(())
    }
    fn type_choice_indirection(
        &mut self,
        choice_ind: &ChoiceIndirection,
    ) -> Result<(), SpannedTypeError> {
        let current = self.infty_at(choice_ind.id.0);
        for (_, choice_id) in choice_ind.choices.iter() {
            let choice = self.infty_at(*choice_id);
            self.infctx.constrain(choice, current).map_err(|e| {
                SpannedTypeError::new(e, IndirectSpan::default_span(choice_ind.id.0))
            })?;
        }
        Ok(())
    }
    fn type_parse(&mut self, parse: &ParseStatement) -> Result<(), SpannedTypeError> {
        let expr = parse.expr.lookup(self.db)?;
        let ty = self.type_expr(&expr)?;
        let self_ty = self.infty_at(parse.id.0);
        self.infctx
            .constrain(ty, self_ty)
            .map_err(|e| SpannedTypeError::new(e, IndirectSpan::default_span(parse.id.0)))
    }
    fn type_let(&mut self, let_statement: &hir::LetStatement) -> Result<(), SpannedTypeError> {
        let expr = let_statement.expr.lookup(self.db)?;
        let ty = self.with_ambient_type(None, |ctx| ctx.type_expr(&expr))?;
        let self_ty = self.infty_at(let_statement.id.0);
        self.infctx
            .constrain(ty, self_ty)
            .map_err(|e| SpannedTypeError::new(e, IndirectSpan::default_span(let_statement.id.0)))
    }
}

pub struct FullResolver<'a, 'intern> {
    db: &'a dyn TyHirs,
    inftypes: FxHashMap<DefId, InfTypeId<'intern>>,
    inf_expressions: FxHashMap<hir::ExprId, InfTypedExpression<'intern>>,
    loc: TypingLocation,
    current_ambient: Option<InfTypeId<'intern>>,
}

impl<'a, 'intern> FullResolver<'a, 'intern> {
    pub fn new(db: &'a dyn TyHirs, loc: DefId) -> SResult<Self> {
        Ok(Self {
            db,
            inftypes: Default::default(),
            inf_expressions: Default::default(),
            loc: TypingLocation::at_id(db, loc)?,
            current_ambient: None,
        })
    }
}

impl<'a, 'intern> TypeResolver<'intern> for FullResolver<'a, 'intern> {
    type DB = dyn TyHirs + 'a;
    fn db(&self) -> &Self::DB {
        self.db
    }

    fn field_type(
        &self,
        ty: &NominalInfHead<'intern>,
        name: FieldName,
    ) -> Result<EitherType<'intern>, TypeError> {
        let block = match NominalId::from_nominal_inf_head(ty) {
            NominalId::Def(pd) => {
                dbpanic!(self.db, "field_type called on parser def {}", &pd.0)
            }
            NominalId::Block(b) => b,
        }
        .lookup(self.db)?;
        let child_id = block
            .root_context
            .0
            .child(self.db, PathComponent::Named(name));
        if ty.internal {
            self.inftypes
                .get(&child_id)
                .map(|&id| id.into())
                .ok_or(TypeError::UnknownField(name))
        } else {
            Ok(self.db.public_type(child_id).map(|t| t.into())?)
        }
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

    fn lookup(&self, val: DefId) -> Result<EitherType<'intern>, TypeError> {
        Ok(self.inftypes[&val].into())
    }

    fn name(&self) -> String {
        dbformat!(self.db, "full at {}", &self.loc.pd.0)
    }

    fn parserdef(&self, pd: DefId) -> Result<EitherType<'intern>, TypeError> {
        get_parserdef(self.db(), pd).map(|x| x.into())
    }
}

#[cfg(test)]
mod tests {
    use crate::databased_display::DatabasedDisplay;
    use crate::hir::Hirs;
    use crate::{context::Context, types::TypeInterner};

    use super::*;

    #[test]
    fn test_type_expr() {
        let ctx = Context::mock(
            r#"
def for['t] *> nil = {}
def for['t] *> expr1 = {
    a: ~,
    b: {
        let c: int = 2,
        d: ~,
        ;
        let c: int = 1,
    },
}
def each[int] *> expr2 = {
    x: expr1,
    let y: int = 3 + x.a,
}
def for['t] *> expr3 = ~
def for[for[int]] *> expr4 = {
    x: expr3 |> expr3,
    let b: for[int] *> (for[int] &> expr3) = expr3,
    y: ~ |> b,
    let a: int = x + y,
}
def each[int] *> expr5 = {
    x: expr2,
    let b: expr2 = x,
}
def each[int] *> expr6 = {
    let expr3: each[int] *> expr5 = expr5,
    b: expr3,
    inner: {
        let expr3: each[int] *> expr2 = expr2,
        b: expr3,
    },
}
            "#,
        );
        let full_type = |name: &str, fields: &[&str]| {
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
                ret = ctx.db.parser_type_at(child).unwrap();
            }
            ret.to_db_string(&ctx.db)
        };
        assert_eq!(full_type("expr1", &["a"]), "'t");
        assert_eq!(full_type("expr1", &["b", "c"]), "int");
        assert_eq!(full_type("expr1", &["b", "d"]), "'t");
        assert_eq!(full_type("expr2", &["x"]), "for[int] &> file[_].expr1");
        assert_eq!(full_type("expr2", &["y"]), "int");
        assert_eq!(full_type("expr4", &["x"]), "for[int] &> file[_].expr3");
        assert_eq!(
            full_type("expr4", &["b"]),
            "for[int] *> for[int] &> file[_].expr3"
        );
        assert_eq!(full_type("expr4", &["y"]), "for[int] &> file[_].expr3");
        assert_eq!(full_type("expr4", &["a"]), "int");
        assert_eq!(full_type("expr5", &["x"]), "each[int] &> file[_].expr2");
        assert_eq!(full_type("expr5", &["b"]), "each[int] &> file[_].expr2");
        assert_eq!(
            full_type("expr6", &["expr3"]),
            "each[int] *> each[int] &> file[_].expr5"
        );
        assert_eq!(full_type("expr6", &["b"]), "each[int] &> file[_].expr5");
        assert_eq!(
            full_type("expr6", &["inner", "expr3"]),
            "each[int] *> each[int] &> file[_].expr2"
        );
        assert_eq!(
            full_type("expr6", &["inner", "b"]),
            "each[int] &> file[_].expr2"
        );
    }
}
