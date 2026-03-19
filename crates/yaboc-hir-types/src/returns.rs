use yaboc_base::{dbformat, dbpanic};
use yaboc_expr::FetchKindData;
use yaboc_hir::{HirNode, ParserDefId};
use yaboc_resolve::parserdef_ssc::FunctionSscId;
use yaboc_types::{inference::InternedBlockHead, to_type::TypeConvertMemo};

use super::*;

pub const THUNK_BIT: u8 = 8;
pub const VTABLE_BIT: u8 = 0;

pub fn parser_type_at(db: &dyn TyHirs, id: DefId) -> SResult<TypeId> {
    let parent_pd = db.hir_parent_parserdef(id)?;
    let types = db.ssc_types(db.parser_ssc(parent_pd)?).silence()?;
    let res = types.types.get(&id).copied().ok_or_else(SilencedError::new);
    res
}

pub fn parser_expr_at(db: &dyn TyHirs, id: hir::ExprId) -> SResult<Arc<ExprTypeData>> {
    let parent_pd = db.hir_parent_parserdef(id.0)?;
    let types = db.ssc_types(db.parser_ssc(parent_pd)?).silence()?;
    Ok(Arc::new(
        types
            .exprs
            .get(&id)
            .cloned()
            .ok_or_else(SilencedError::new)?,
    ))
}

impl<DB: TyHirs + ?Sized> FetchKindData<FullTypeId, ExprId, DB> for Resolved {
    type Err = SilencedError;
    type Data = Arc<ExprTypeData>;

    fn fetch_kind_data(db: &DB, id: ExprId) -> Result<Self::Data, Self::Err> {
        db.parser_expr_at(id)
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct SscTypes {
    pub types: BTreeMap<DefId, TypeId>,
    pub exprs: BTreeMap<hir::ExprId, ExprTypeData>,
    pub args: BTreeMap<hir::ParserDefId, Signature>,
    pub sigs: BTreeMap<hir::ParserDefId, TypeId>,
}

pub fn parser_returns(db: &dyn TyHirs, id: hir::ParserDefId) -> SResult<ParserDefType> {
    let ssc_types = db.ssc_types(db.parser_ssc(id)?).silence()?;
    let ty = ssc_types
        .types
        .get(&id.0)
        .copied()
        .unwrap_or_else(|| dbpanic!(db, "parser_returns: no type for parserdef {}", &id.0));
    Ok(ParserDefType { id, deref: ty })
}

pub fn ssc_types(db: &dyn TyHirs, id: FunctionSscId) -> Result<SscTypes, SpannedTypeError> {
    let def_ids = db.lookup_intern_recursion_scc(id);
    let defs = def_ids
        .iter()
        .map(|fun: &hir::ParserDefId| fun.lookup(db))
        .collect::<Result<Vec<_>, _>>()?;

    let resolver = ReturnResolver::new(db);
    let bump = Bump::new();
    let placeholder_id = defs[0].id;
    let loc = TypingLocation::at_id(db, placeholder_id.0)?;
    let mut ctx = TypingContext::new(db, resolver, loc, &bump);

    let mut inftypes = FxHashMap::default();
    let mut local_inftypes = FxHashMap::default();
    let mut inftype_def = FxHashMap::default();
    let mut froms = vec![];
    let mut argss = vec![];
    let mut ty_argss = vec![];
    for def in defs.iter() {
        ctx.loc = TypingLocation::at_id(db, def.id.0)?;
        ctx.initialize_vars_at(def.id.0, &mut local_inftypes)?;
        let (from, args) = ctx.initialize_parserdef_args(def.id, &mut local_inftypes)?;
        for (id, inftype) in local_inftypes.drain() {
            inftypes.insert(id, inftype);
            inftype_def.insert(id, def.id);
        }
        froms.push(from);
        let mut sig = inftypes[&def.id.0];
        if let Some(from) = from {
            sig = ctx.infctx.parser(sig, from);
        }
        if let Some(args) = &args {
            let arg_slice = ctx.infctx.intern_infty_slice(&args);
            sig = ctx.infctx.function(sig, arg_slice, None);
        }
        argss.push(args);
        let ty_args = ctx.loc.vars.defs.clone();
        let tyarg_count = ty_args.len();
        ty_argss.push(ty_args);

        ctx.infctx.tr.signatures.insert(def.id, (sig, tyarg_count));
    }
    ctx.infctx.tr.inftypes = inftypes;
    for (def, from) in defs.iter().zip(froms.iter()) {
        // in this separate loop we do the actual type inference
        ctx.loc = TypingLocation::at_id(db, def.id.0)?;
        ctx.type_parserdef(def.id, *from)?;
    }
    let mut rets = Vec::new();
    let mut types = SscTypes::default();
    let inf_expressions = ctx.inf_expressions.clone();
    let mut converter = ctx.infctx.type_converter(placeholder_id.0)?;
    for def in defs.iter() {
        let spanned = |e| SpannedTypeError::new(e, IndirectSpan::default_span(def.to.0));
        // here we are finished with inference so we can convert to actual types
        converter.set_id(def.id.0);
        let deref = converter
            .convert_to_type(converter.ctx.tr.inftypes[&def.id.0])
            .map_err(spanned)?;
        rets.push(ParserDefType { id: def.id, deref });
    }
    for (id, infty) in converter
        .ctx
        .tr
        .inftypes
        .iter()
        .map(|(id, infty)| (*id, *infty))
        .collect::<Vec<_>>()
    {
        let def = inftype_def[&id];
        converter.set_id(def.0);
        let ty = converter
            .convert_to_type(infty)
            .map_err(|e| SpannedTypeError::new(e, IndirectSpan::default_span(id)))?;
        types.types.insert(id, ty);
    }
    for (id, infty) in converter
        .ctx
        .tr
        .signatures
        .iter()
        .map(|(id, (infty, _))| (*id, *infty))
        .collect::<Vec<_>>()
    {
        converter.set_id(id.0);
        let ty = converter
            .convert_to_type(infty)
            .map_err(|e| SpannedTypeError::new(e, IndirectSpan::default_span(id.0)))?;
        types.sigs.insert(id, ty);
    }
    for (id, ((args, from), ty_args)) in defs
        .iter()
        .zip(argss.iter().zip(froms.iter()).zip(ty_argss.into_iter()))
    {
        converter.set_id(id.id.0);
        fn conv<'a, 'intern>(
            converter: &mut TypeConvertMemo<'a, 'intern, ReturnResolver<'intern>>,
            infty: InfTypeId<'intern>,
            id: DefId,
        ) -> Result<TypeId, SpannedTypeError> {
            converter
                .convert_to_type(infty)
                .map_err(|e| SpannedTypeError::new(e, IndirectSpan::default_span(id)))
        }

        let args = args
            .as_ref()
            .map(|x| {
                x.iter()
                    .map(|infty| conv(&mut converter, *infty, id.id.0))
                    .collect::<Result<Vec<_>, _>>()
            })
            .transpose()?
            .map(Arc::new);
        let from = from
            .as_ref()
            .map(|infty| conv(&mut converter, *infty, id.id.0))
            .transpose()?;
        let sig = Signature {
            ty_args: Arc::new(ty_args),
            from,
            args,
            thunky: id.kind.thunky(),
        };
        types.args.insert(id.id, sig);
    }
    // make sure we get the modified return
    for ParserDefType { id, deref } in rets.iter() {
        types.types.insert(id.0, *deref);
    }
    // the context does not need the expressions anymore, so we can just mem::take them
    for (id, expr) in inf_expressions {
        let def = db.hir_parent_parserdef(id.0)?;
        converter.set_id(def.0);
        let expr = &expr;
        let spans = db.resolve_expr(id)?;
        let expr = expr
            .as_slice()
            .zip(spans.data.as_slice())
            .map(|(ty, span)| {
                converter
                    .convert_to_type(*ty)
                    .map_err(|e| SpannedTypeError::new(e, IndirectSpan::new(id.0, *span)))
            })
            .try_collect()?;
        types.exprs.insert(id, expr);
    }
    Ok(types)
}

pub struct ReturnResolver<'intern> {
    db: &'intern dyn TyHirs,
    pub inftypes: FxHashMap<DefId, InfTypeId<'intern>>,
    pub signatures: FxHashMap<ParserDefId, (InfTypeId<'intern>, usize)>,
}

impl<'intern> ReturnResolver<'intern> {
    #[must_use]
    pub fn new(db: &'intern dyn TyHirs) -> Self {
        Self {
            db,
            inftypes: Default::default(),
            signatures: Default::default(),
        }
    }
}

impl<'intern> TypeResolver<'intern> for ReturnResolver<'intern> {
    type DB = dyn TyHirs + 'intern;

    fn db(&self) -> &Self::DB {
        self.db
    }

    fn field_type(
        &self,
        ty: &InternedBlockHead<'intern>,
        name: FieldName,
    ) -> Result<EitherType<'intern>, TypeError> {
        let HirNode::Block(block) = self.db.hir_node(ty.def)? else {
            panic!("Non-block on field_type")
        };
        let root_context = block.root_context.lookup(self.db)?;
        let Some(child_id) = root_context.vars.get(name) else {
            return Err(TypeError::UnknownField(name));
        };
        Ok(if ty.internal {
            self.inftypes[child_id.inner()].into()
        } else {
            self.db
                .parser_type_at(*child_id.inner())
                .map(|t| t.into())?
        })
    }

    fn signature(&self, id: DefId) -> SResult<(EitherType<'intern>, usize)> {
        if let Some((ty, tyarg_count)) = self.signatures.get(&ParserDefId(id)) {
            Ok((EitherType::Inference(*ty), *tyarg_count))
        } else {
            let (ty, tyarg_count) = self.db().parser_signature(ParserDefId(id))?;
            Ok((EitherType::Regular(ty), tyarg_count))
        }
    }

    fn lookup(&self, val: DefId) -> Result<EitherType<'intern>, TypeError> {
        Ok(self.inftypes[&val].into())
    }

    fn name(&self) -> String {
        let mut ret = String::from("returns for (");
        for (i, id) in self.inftypes.keys().enumerate() {
            if i > 0 {
                ret.push_str(", ");
            }
            ret.push_str(&dbformat!(self.db, "{}", id));
        }
        ret.push(')');
        ret
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefType {
    pub id: hir::ParserDefId,
    pub deref: TypeId,
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
    fn return_types() {
        let ctx = Context::<HirTypesTestDatabase>::mock(
            r#"
def []T ~> nil[T] = {}
def (nil[int]) ~> expr1 = {t: expr1}
def single = ~
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
        assert_eq!(return_type("nil"), "unit");
        assert_eq!(return_type("expr1"), "<anonymous block file[_].expr1.1.0>");
        assert_eq!(return_type("single"), "int");
    }
    #[test]
    fn block_with_return() {
        let ctx = Context::<HirTypesTestDatabase>::mock(
            r#"
def u16l = {
    low: ~
    high: ~
    let return: int = low + high * 256
}
            "#,
        );
        let parser = ctx.parser("u16l");
        let ret = ctx.db.parser_returns(parser).unwrap().deref;
        assert_eq!(ret.to_db_string(&ctx.db), "int");
    }
    #[test]
    fn test_type_expr() {
        let ctx = Context::<HirTypesTestDatabase>::mock(
            r#"
def []T ~> nil[T] = {}
def []T ~> expr1[T] = {
  a: ~
  b: {
    case
    | let c: int = 2
      d: ~
    | let c: int = 1
    \
  }
}
def expr2 = {
  x: expr1
  let y: int = 3 + x.a
}
def [][]u8 ~> expr4 = {
  x: ~ |> ~
  let b: ~int = ~
  y: ~ |> b
  let a: int = x + y
}
def expr5 = {
  x: expr2
  let b: expr2 = x
}
def expr6 = {
  let expr3: ~expr5 = expr5
  b: expr3
  inner: {
    let expr3: ~expr2 = expr2
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
                    Type::Block(n) => n.def,
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
        assert_eq!(
            full_type("expr2", &["x"]),
            "<anonymous block file[_].expr1.1.0[int]>"
        );
        assert_eq!(full_type("expr2", &["y"]), "int");
        //assert_eq!(full_type("expr4", &["x"]), "int");
        assert_eq!(full_type("expr4", &["b"]), "[int] ~> int");
        //assert_eq!(full_type("expr4", &["y"]), "int");
        assert_eq!(full_type("expr4", &["a"]), "int");
        assert_eq!(
            full_type("expr5", &["x"]),
            "<anonymous block file[_].expr2.1.0>"
        );
        assert_eq!(
            full_type("expr5", &["b"]),
            "<anonymous block file[_].expr2.1.0>"
        );
        assert_eq!(
            full_type("expr6", &["expr3"]),
            "[int] ~> <anonymous block file[_].expr5.1.0>"
        );
        assert_eq!(
            full_type("expr6", &["b"]),
            "<anonymous block file[_].expr5.1.0>"
        );
        assert_eq!(
            full_type("expr6", &["inner", "expr3"]),
            "[int] ~> <anonymous block file[_].expr2.1.0>"
        );
        assert_eq!(
            full_type("expr6", &["inner", "b"]),
            "<anonymous block file[_].expr2.1.0>"
        );
    }
}
