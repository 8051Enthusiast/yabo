use hir::HirNode;
use yaboc_base::dbpanic;
use yaboc_expr::FetchExpr;

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
                Type::ParserArg { arg, .. } => vec![arg],
                Type::FunctionArg(..) => vec![],
                _ => panic!("block not in function or parser arg"),
            };
            (arg_ty, expr_id.0)
        }
        HirNode::Context(ctx) => (vec![], ctx.block_id.0),
        HirNode::Lambda(lid) => {
            let expr_id = lid.enclosing_expr;
            let tys = lid
                .args
                .iter()
                .map(|arg| db.parser_type_at(arg.0))
                .collect::<SResult<Vec<TypeId>>>()?;
            (tys, expr_id.0)
        }
        _ => {
            let parent = id.parent(db).unwrap();
            (vec![], parent)
        }
    };
    let mut ret = Vec::from(db.bound_args(ancestor)?.as_ref());
    ret.extend(end);
    Ok(Arc::from(ret))
}

pub fn fun_arg_count(db: &dyn TyHirs, ty: TypeId) -> SResult<Option<u32>> {
    Ok(
        if let Type::FunctionArg(_, args) = db.lookup_intern_type(ty) {
            Some(args.len().try_into().unwrap())
        } else {
            None
        },
    )
}

pub fn parser_args(db: &dyn TyHirs, id: hir::ParserDefId) -> SResult<Signature> {
    let ssc_types = db.ssc_types(db.parser_ssc(id)?).silence()?;
    let sig = ssc_types
        .args
        .get(&id)
        .unwrap_or_else(|| dbpanic!(db, "parser_args: no signature for parserdef {}", &id.0))
        .clone();
    Ok(sig)
}

pub fn parser_signature(db: &dyn TyHirs, id: hir::ParserDefId) -> SResult<(TypeId, usize)> {
    let ssc_types = db.ssc_types(db.parser_ssc(id)?).silence()?;
    let sig = ssc_types
        .sigs
        .get(&id)
        .unwrap_or_else(|| dbpanic!(db, "parser_args: no signature for parserdef {}", &id.0))
        .clone();
    let args = db.parser_args(id)?;
    Ok((sig, args.ty_args.len()))
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
        assert_eq!("[int]", arg_type("expr1"));
        assert_eq!("[unit]", arg_type("expr2"));
        assert_eq!("['0]", arg_type("expr3"));
        assert_eq!("[[unit] ~> unit]", arg_type("expr4"));
        assert_eq!("[unit]", arg_type("expr5"));
    }
}
