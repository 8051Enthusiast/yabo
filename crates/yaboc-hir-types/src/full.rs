use std::rc::Rc;

use yaboc_base::{
    dbformat, dbpanic,
    error::{SResult, Silencable, SilencedError},
    interner::PathComponent,
};
use yaboc_hir::walk::ChildIter;
use yaboc_types::inference::{InfTypeId, NominalInfHead, TypeResolver};

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
    let loc = TypingLocation::at_id(db, id.0)?;
    let mut ctx = TypingContext::new(db, resolver, loc, &bump, true);
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
    // the context does not need the expressions anymore, so we can just mem::take them
    for (&id, expr) in std::mem::take(&mut ctx.inf_expressions).iter() {
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
    let res = types.types.get(&id).copied().ok_or_else(SilencedError::new);
    res
}

pub fn parser_expr_at(db: &dyn TyHirs, id: hir::ExprId) -> SResult<TypedExpression> {
    let parent_pd = db.hir_parent_parserdef(id.0)?;
    let types = db.parser_full_types(parent_pd).silence()?;
    types.exprs.get(&id).cloned().ok_or_else(SilencedError::new)
}

impl<'a, 'intern> TypingContext<'a, 'intern, FullResolver<'a, 'intern>> {
    fn initialize_vars(&mut self) -> Result<(), SpannedTypeError> {
        let pd = self.loc.pd;
        let mut vars = FxHashMap::default();
        let blocks = self.db.all_parserdef_blocks(pd);
        self.initialize_vars_at(pd.0, &mut vars)?;
        for block in blocks.iter() {
            let block_val = block.lookup(self.db)?;
            if !block_val.returns {
                self.initialize_vars_at(block_val.root_context.0, &mut vars)?;
            }
        }
        self.initialize_parserdef_args(pd, &mut vars)?;
        let ret = self.db.parser_returns(pd)?;
        let ret_inf = self.infctx.convert_type_into_inftype(ret.deref);
        vars.insert(pd.0, ret_inf);
        let vars = Rc::new(vars);
        self.infctx.tr.inftypes = vars.clone();
        self.inftypes = vars.clone();
        self.constrain_public_types()
    }
    fn constrain_public_types(&mut self) -> Result<(), SpannedTypeError> {
        let pd = self.loc.pd;
        for child_node in ChildIter::new(pd.0, self.db) {
            let span = IndirectSpan::default_span(child_node.id());
            let block = match child_node {
                hir::HirNode::Block(block) if !block.returns => block,
                _ => continue,
            };
            let root_ctx = block.root_context.lookup(self.db)?;
            for (_, child) in root_ctx.vars.iter() {
                let child = child.inner();
                let public_type = match self.db.public_type(*child) {
                    Ok(ty) => ty,
                    Err(_) => continue,
                };
                let public_inftype = self.infctx.convert_type_into_inftype(public_type);
                let current_inftype = self.infctx.tr.inftypes[child];
                self.infctx
                    .constrain(current_inftype, public_inftype)
                    .map_err(|e| SpannedTypeError::new(e, span))?;
            }
        }
        Ok(())
    }
}

pub struct FullResolver<'a, 'intern> {
    db: &'a dyn TyHirs,
    inftypes: Rc<FxHashMap<DefId, InfTypeId<'intern>>>,
    name: String,
}

impl<'a, 'intern> FullResolver<'a, 'intern> {
    pub fn new(db: &'a dyn TyHirs, loc: DefId) -> SResult<Self> {
        let pd = db.hir_parent_parserdef(loc)?;
        let name = dbformat!(db, "full at {}", &pd.0);
        Ok(Self {
            db,
            inftypes: Default::default(),
            name,
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
                .ok_or_else(|| TypeError::UnknownField(name))
        } else {
            Ok(self.db.public_type(child_id).map(|t| t.into())?)
        }
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

    fn signature(&self, ty: &NominalInfHead<'intern>) -> Result<Signature, TypeError> {
        get_signature(self.db, ty)
    }

    fn lookup(&self, val: DefId) -> Result<EitherType<'intern>, TypeError> {
        Ok(self.inftypes[&val].into())
    }

    fn name(&self) -> String {
        self.name.clone()
    }

    fn parserdef(&self, pd: DefId) -> Result<EitherType<'intern>, TypeError> {
        get_parserdef(self.db(), pd).map(|x| x.into())
    }
}

#[cfg(test)]
mod tests {
    use crate::hir::Hirs;
    use hir::Parser;
    use yaboc_ast::import::Import;
    use yaboc_base::databased_display::DatabasedDisplay;
    use yaboc_base::Context;
    use yaboc_types::TypeInterner;

    use super::*;

    #[test]
    fn test_type_expr() {
        let ctx = Context::<crate::tests::HirTypesTestDatabase>::mock(
            r#"
def for['t] *> nil = {}
def for['t] *> expr1 = {
  a: ~
  b: {
    | let c: int = 2
      d: ~
    | let c: int = 1
  }
}
def each[int] *> expr2 = {
  x: expr1
  let y: int = 3 + x.a
}
def for[for[int]] *> expr4 = {
  x: ~ |> ~
  let b: for[int] *> int = ~
  y: ~ |> b
  let a: int = x + y
}
def each[int] *> expr5 = {
  x: expr2
  let b: expr2 = x
}
def each[int] *> expr6 = {
  let expr3: each[int] *> expr5 = expr5
  b: expr3
  inner: {
    let expr3: each[int] *> expr2 = expr2
    b: expr3
  }
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
        assert_eq!(full_type("expr1", &["a"]), "'0");
        assert_eq!(full_type("expr1", &["b", "c"]), "int");
        assert_eq!(full_type("expr1", &["b", "d"]), "'0");
        assert_eq!(full_type("expr2", &["x"]), "for[int] &> file[_].expr1");
        assert_eq!(full_type("expr2", &["y"]), "int");
        assert_eq!(full_type("expr4", &["x"]), "int");
        assert_eq!(full_type("expr4", &["b"]), "for[int] *> int");
        assert_eq!(full_type("expr4", &["y"]), "int");
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
