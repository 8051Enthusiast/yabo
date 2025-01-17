use hir::HirNode;
use yaboc_expr::FetchExpr;
use yaboc_types::inference::InternedNomHead;

use super::*;

/// Returns an array of all types of bound arguments at a given point
pub fn bound_args(db: &dyn TyHirs, id: DefId) -> SResult<Arc<[TypeId]>> {
    let (end, ancestor) = match db.hir_node(id)? {
        HirNode::ParserDef(pd) => {
            let args = parser_args(db, pd.id)?;
            let mut ret = args
                .args
                .iter()
                .flat_map(|x| x.iter().copied())
                .collect::<Vec<TypeId>>();
            ret.extend(args.from);
            return Ok(Arc::from(ret));
        }
        HirNode::Import(_) => return Ok(Arc::from([])),
        HirNode::Block(block) => {
            let expr_id = block.enclosing_expr;
            let expr = Resolved::expr_with_data::<FullTypeId>(db, expr_id)?;
            let (_, ty) = expr
                .take_ref()
                .iter_parts()
                .find(|(head, _)| matches!(head, ExprHead::Niladic(ResolvedAtom::Block(id, _)) if *id == block.id))
                .expect("block not found in containing expr");
            let arg_ty = match db.lookup_intern_type(*ty) {
                Type::ParserArg { arg, .. } => Some(arg),
                Type::FunctionArg(..) => None,
                _ => panic!("block not in function or parser arg"),
            };
            (arg_ty, expr_id.0)
        }
        HirNode::Context(ctx) => (None, ctx.block_id.0),
        _ => {
            let parent = id.parent(db).unwrap();
            (None, parent)
        }
    };
    let mut ret = Vec::from(db.bound_args(ancestor)?.as_ref());
    ret.extend(end);
    Ok(Arc::from(ret))
}

pub fn fun_arg_count(db: &dyn TyHirs, ty: TypeId) -> SResult<Option<u32>> {
    let ldt_ty = db.least_deref_type(ty)?;
    Ok(
        if let Type::FunctionArg(_, args) = db.lookup_intern_type(ldt_ty) {
            Some(args.len().try_into().unwrap())
        } else {
            None
        },
    )
}

pub fn parser_args(db: &dyn TyHirs, id: hir::ParserDefId) -> SResult<Signature> {
    parser_args_error(db, id).silence()
}

pub fn parser_args_error(
    db: &dyn TyHirs,
    id: hir::ParserDefId,
) -> Result<Signature, SpannedTypeError> {
    let pd = id.lookup(db)?;
    let loc = TypingLocation {
        vars: TypeVarCollection::at_id(db, id)?,
        loc: db.hir_parent_module(id.0)?.0,
        pd: id,
    };
    let arg_resolver = ArgResolver::new(db);
    let bump = Bump::new();
    let mut tcx = TypingContext::new(db, arg_resolver, loc, &bump);
    let mut arg_inftys = pd
        .args
        .as_ref()
        .map(|x| -> Result<_, SpannedTypeError> {
            let mut ret = Vec::new();
            for arg in x.iter() {
                // top level definitions always have type annotation
                // (enforced by syntax definition)
                let arg_ty = arg.lookup(db)?.ty.unwrap();
                let arg_expr = arg_ty.lookup(db)?;
                let arg_infty = tcx.resolve_type_expr(arg_expr.expr.take_ref(), arg_ty)?;
                ret.push(arg_infty);
            }
            Ok(ret)
        })
        .transpose()?
        .unwrap_or_default();
    if let Some(from) = pd.from {
        let from_expr = from.lookup(db)?;
        let from_infty = tcx.resolve_type_expr(from_expr.expr.take_ref(), from)?;
        arg_inftys.push(from_infty);
    }
    let inftys = &arg_inftys;
    // we do not deref anything here, so we do not need to pass any
    // local ids to the deref cache
    let mut converter = tcx.infctx.type_converter(id.0, &[])?;
    let mut args = Vec::new();
    for infty in inftys {
        let new_ty = converter
            .convert_to_type(*infty)
            .map_err(|e| SpannedTypeError::new(e, IndirectSpan::default_span(pd.id.0)))?;
        args.push(new_ty);
    }
    let ty_args = tcx.loc.vars.var_types(db, id);
    let from_ty = if pd.from.is_some() {
        Some(args.pop().unwrap())
    } else {
        None
    };
    let args = if pd.args.is_some() {
        Some(Arc::new(args))
    } else {
        None
    };
    let kind = match pd.kind {
        DefKind::Def => NominalKind::Def,
        DefKind::Fun => NominalKind::Fun,
        DefKind::Static => NominalKind::Static,
    };
    let thunk = NominalTypeHead {
        kind,
        def: id.0,
        parse_arg: from_ty,
        fun_args: args.clone().unwrap_or_default(),
        ty_args: Arc::new(ty_args),
    };
    Ok(Signature {
        ty_args: Arc::new(tcx.loc.vars.defs),
        from: from_ty,
        args,
        thunk,
        thunky: pd.kind.thunky(),
    })
}

pub struct ArgResolver<'a>(&'a dyn TyHirs);

impl<'a> ArgResolver<'a> {
    pub fn new(db: &'a dyn TyHirs) -> Self {
        ArgResolver(db)
    }
}

impl<'a> TypeResolver<'a> for ArgResolver<'a> {
    fn field_type(
        &self,
        _ty: &InternedNomHead<'a>,
        _name: FieldName,
    ) -> Result<EitherType<'a>, TypeError> {
        Ok(self.0.intern_type(Type::Unknown).into())
    }

    fn deref(&self, _ty: &InternedNomHead<'a>) -> SResult<Option<EitherType<'a>>> {
        Ok(None)
    }

    fn signature(&self, id: DefId) -> SResult<Signature> {
        get_signature(self.0, id)
    }

    fn is_local(&self, _: DefId) -> bool {
        false
    }

    fn lookup(&self, _val: DefId) -> Result<EitherType<'a>, TypeError> {
        Err(SilencedError::new().into())
    }

    type DB = dyn TyHirs + 'a;

    fn db(&self) -> &Self::DB {
        self.0
    }

    fn name(&self) -> String {
        String::from("signature")
    }
}

pub fn get_signature(db: &dyn TyHirs, id: DefId) -> SResult<Signature> {
    let HirNode::ParserDef(pd) = db.hir_node(id)? else {
        panic!("attempted to extract signature from non-parser-def")
    };
    db.parser_args(pd.id)
}

#[cfg(test)]
mod tests {
    use crate::tests::HirTypesTestDatabase;
    use hir::Parser;
    use yaboc_ast::import::Import;
    use yaboc_base::databased_display::DatabasedDisplay;
    use yaboc_base::Context;

    use super::*;
    #[test]
    fn arg_types() {
        let ctx = Context::<HirTypesTestDatabase>::mock(
            r#"
def ~expr1 = {}
def [expr1] ~> expr2 = {}
def [X] ~> expr3[X] = {}
def [[expr1] ~> expr2] ~> expr4 = {}
def [expr3[int]] ~> expr5 = {}
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
        assert_eq!("[u8]", arg_type("expr1"));
        assert_eq!("[file[_].expr1]", arg_type("expr2"));
        assert_eq!("['0]", arg_type("expr3"));
        assert_eq!("[[file[_].expr1] ~> file[_].expr2]", arg_type("expr4"));
        assert_eq!("[file[_].expr3[int]]", arg_type("expr5"));
    }
}
