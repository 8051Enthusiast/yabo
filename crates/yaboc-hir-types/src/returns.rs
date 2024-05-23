use std::fmt::Display;

use yaboc_base::{dbformat, dbpanic};
use yaboc_expr::FetchKindData;
use yaboc_resolve::parserdef_ssc::FunctionSscId;
use yaboc_types::inference::InternedNomHead;

use super::*;

pub fn deref_type(db: &dyn TyHirs, ty: TypeId) -> SResult<Option<TypeId>> {
    match db.lookup_intern_type(ty) {
        Type::Nominal(nom) => {
            let id = match NominalId::from_nominal_head(&nom) {
                NominalId::Def(id) => id,
                NominalId::Block(_) => return Ok(None),
            };
            let deref_ty = db.parser_returns(id)?.deref;
            let subst_deref_ty = db.substitute_typevar(deref_ty, nom.ty_args);
            Ok(Some(subst_deref_ty))
        }
        Type::Primitive(PrimitiveType::U8) => Ok(Some(db.int())),
        _ => Ok(None),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DerefLevel(u64);

const RESERVED_DEREF_METADATA_BITS: u8 = 8;
pub const VTABLE_BIT: u8 = 0;
pub const NOBACKTRACK_BIT: u8 = 1;

impl DerefLevel {
    pub fn into_shifted_runtime_value(self) -> u64 {
        self.0 << RESERVED_DEREF_METADATA_BITS
    }
    pub fn is_deref(self) -> bool {
        self.0 > 0
    }
    pub fn zero() -> Self {
        DerefLevel(0)
    }
    pub fn max() -> Self {
        DerefLevel(u64::MAX >> RESERVED_DEREF_METADATA_BITS)
    }
    pub fn inc(self) -> Self {
        DerefLevel(self.0 + 1)
    }
}

impl Display for DerefLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Removes top-level `fun` and `static` nominals from the type
pub fn normalize_head(db: &dyn TyHirs, ty: TypeId) -> SResult<TypeId> {
    match db.lookup_intern_type(ty) {
        Type::Nominal(nom) => {
            let NominalId::Def(_) = NominalId::from_nominal_head(&nom) else {
                return Ok(ty);
            };
            if nom.kind == NominalKind::Def {
                return Ok(ty);
            }
            let Some(deref_ty) = db.deref_type(ty)? else {
                return Ok(ty);
            };
            db.normalize_head(deref_ty)
        }
        _ => Ok(ty),
    }
}

pub fn deref_level(db: &dyn TyHirs, ty: TypeId) -> SResult<DerefLevel> {
    match db.lookup_intern_type(ty) {
        Type::Primitive(PrimitiveType::U8) => Ok(DerefLevel::zero().inc()),
        Type::Nominal(nom) => {
            let id = match NominalId::from_nominal_head(&nom) {
                NominalId::Def(id) => id,
                NominalId::Block(_) => return Ok(DerefLevel::zero()),
            };
            let deref_ty = db.parser_returns(id)?.deref;
            let subst_deref_ty = db.substitute_typevar(deref_ty, nom.ty_args);
            let mut deref_level = db.deref_level(subst_deref_ty)?;
            if nom.kind == NominalKind::Def {
                deref_level = deref_level.inc();
            }
            Ok(deref_level)
        }
        _ => Ok(DerefLevel::zero()),
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
    for def in defs.iter() {
        // in this loop we do not directly type the inference variables we are creating yet
        ctx.initialize_vars_at(def.id.0, &mut local_inftypes)?;
        ctx.initialize_parserdef_args(def.id, &mut local_inftypes)?;
        for (id, inftype) in local_inftypes.drain() {
            inftypes.insert(id, inftype);
            inftype_def.insert(id, def.id);
        }
    }
    ctx.inftypes = Rc::new(inftypes);
    ctx.infctx.tr.inftypes = ctx.inftypes.clone();
    for def in defs.iter() {
        // in this separate loop we do the actual type inference
        ctx.loc = TypingLocation::at_id(db, def.id.0)?;
        ctx.type_parserdef(def.id)?;
    }
    let mut rets = Vec::new();
    let mut types = SscTypes::default();
    let inftypes = ctx.inftypes.clone();
    let inf_expressions = ctx.inf_expressions.clone();
    let locals = defs.iter().map(|d| d.id.0).collect::<Vec<_>>();
    let mut converter = ctx.infctx.type_converter(placeholder_id.0, &locals)?;
    for def in defs.iter() {
        let spanned = |e| SpannedTypeError::new(e, IndirectSpan::default_span(def.to.0));
        // here we are finished with inference so we can convert to actual types
        converter.set_id(def.id.0);
        let mut deref = converter
            .convert_to_type(ctx.inftypes[&def.id.0])
            .map_err(spanned)?;
        if def.kind == DefKind::Static && def.ret_ty.is_none() {
            deref = db.least_deref_type(deref)?;
        }
        rets.push(ParserDefType { id: def.id, deref });
    }
    for (&id, &infty) in inftypes.iter() {
        let def = inftype_def[&id];
        converter.set_id(def.0);
        let ty = converter
            .convert_to_type(infty)
            .map_err(|e| SpannedTypeError::new(e, IndirectSpan::default_span(id)))?;
        types.types.insert(id, ty);
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
    inftypes: Rc<FxHashMap<DefId, InfTypeId<'intern>>>,
}

impl<'intern> ReturnResolver<'intern> {
    #[must_use]
    pub fn new(db: &'intern dyn TyHirs) -> Self {
        Self {
            db,
            inftypes: Default::default(),
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
        ty: &InternedNomHead<'intern>,
        name: FieldName,
    ) -> Result<EitherType<'intern>, TypeError> {
        let block = match NominalId::from_nominal_inf_head(ty) {
            NominalId::Def(pd) => {
                dbpanic!(self.db, "field_type called on parser def {}", &pd.0)
            }
            NominalId::Block(b) => b,
        }
        .lookup(self.db)?;
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

    fn deref(&self, ty: &InternedNomHead<'intern>) -> SResult<Option<EitherType<'intern>>> {
        if let Some(deref_ty) = self.inftypes.get(&ty.def) {
            Ok(Some(EitherType::Inference(*deref_ty)))
        } else {
            let id = match NominalId::from_nominal_inf_head(ty) {
                NominalId::Def(d) => d,
                NominalId::Block(_) => return Ok(None),
            };
            Ok(Some(EitherType::Regular(self.db.parser_returns(id)?.deref)))
        }
    }

    fn signature(&self, id: DefId) -> SResult<Signature> {
        get_signature(self.db, id)
    }

    fn is_local(&self, pd: DefId) -> bool {
        self.inftypes.get(&pd).is_some()
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
def ['t] *> nil = {}
def nil *> expr1 = {}
def *single = ~
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
            "<anonymous block ['0] &> file[_].nil.1.0>"
        );
        assert_eq!(
            return_type("expr1"),
            "<anonymous block ['0] &> file[_].nil &> file[_].expr1.1.0>"
        );
        assert_eq!(return_type("single"), "u8");
    }
    #[test]
    fn block_with_return() {
        let ctx = Context::<HirTypesTestDatabase>::mock(
            r#"
def *u16l = {
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
def ['t] *> nil = {}
def ['t] *> expr1 = {
  a: ~
  b: {
    | let c: int = 2
      d: ~
    | let c: int = 1
  }
}
def *expr2 = {
  x: expr1
  let y: int = 3 + x.a
}
def [[u8]] *> expr4 = {
  x: ~ |> ~
  let b: *int = ~
  y: ~ |> b
  let a: int = x + y
}
def *expr5 = {
  x: expr2
  let b: expr2 = x
}
def *expr6 = {
  let expr3: *expr5 = expr5
  b: expr3
  inner: {
    let expr3: *expr2 = expr2
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
        assert_eq!(full_type("expr2", &["x"]), "[u8] &> file[_].expr1");
        assert_eq!(full_type("expr2", &["y"]), "int");
        //assert_eq!(full_type("expr4", &["x"]), "int");
        assert_eq!(full_type("expr4", &["b"]), "[u8] *> int");
        //assert_eq!(full_type("expr4", &["y"]), "int");
        assert_eq!(full_type("expr4", &["a"]), "int");
        assert_eq!(full_type("expr5", &["x"]), "[u8] &> file[_].expr2");
        assert_eq!(full_type("expr5", &["b"]), "[u8] &> file[_].expr2");
        assert_eq!(
            full_type("expr6", &["expr3"]),
            "[u8] *> [u8] &> file[_].expr5"
        );
        assert_eq!(full_type("expr6", &["b"]), "[u8] &> file[_].expr5");
        assert_eq!(
            full_type("expr6", &["inner", "expr3"]),
            "[u8] *> [u8] &> file[_].expr2"
        );
        assert_eq!(full_type("expr6", &["inner", "b"]), "[u8] &> file[_].expr2");
    }
}
