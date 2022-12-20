use yaboc_ast::{
    expr::{Dyadic, ExpressionHead, Ignorable, Monadic, OpWithData, Variadic},
    ArrayKind,
};
use yaboc_base::{databased_display::DatabasedDisplay, dbwrite};

use super::*;

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for HirNode {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            HirNode::Let(_) => write!(f, "Let"),
            HirNode::Expr(e) => dbwrite!(f, db, "Expr({})", e),
            HirNode::TExpr(e) => dbwrite!(f, db, "TExpr({})", e),
            HirNode::Parse(_) => write!(f, "Parse"),
            HirNode::Array(a) => write!(f, "Array({:?})", a.direction),
            HirNode::Block(_) => write!(f, "Block"),
            HirNode::Choice(_) => write!(f, "Choice"),
            HirNode::Import(_) => write!(f, "Import"),
            HirNode::ArgDef(_) => write!(f, "ArgDef"),
            HirNode::Module(_) => write!(f, "Module"),
            HirNode::Context(_) => write!(f, "Context"),
            HirNode::ParserDef(n) => {
                if n.qualifier == Qualifier::Export {
                    write!(f, "export ")?;
                }
                write!(f, "ParserDef")
            }
            HirNode::ChoiceIndirection(_) => write!(f, "ChoiceIndirection"),
        }
    }
}

impl std::fmt::Display for Qualifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let r = match self {
            Qualifier::Export => "export",
            Qualifier::Regular => "regular",
        };
        write!(f, "{r}")
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for ValExpression {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        W(&self.expr).db_fmt(f, db)
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for TypeExpression {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        W(&self.expr).db_fmt(f, db)
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for ParserAtom {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            ParserAtom::Atom(atom) => atom.db_fmt(f, db),
            ParserAtom::Single => dbwrite!(f, db, "~"),
            ParserAtom::Nil => dbwrite!(f, db, "+"),
            ParserAtom::Block(id) => dbwrite!(
                f,
                db,
                "block({})",
                db.lookup_intern_hir_path(id.0)
                    .path()
                    .iter()
                    .last()
                    .unwrap()
            ),
        }
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for TypeAtom {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            TypeAtom::ParserDef(pd) => pd.db_fmt(f, db),
            TypeAtom::Array(arr) => arr.db_fmt(f, db),
            TypeAtom::Primitive(p) => p.db_fmt(f, db),
            TypeAtom::TypeVar(v) => v.db_fmt(f, db),
        }
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for ParserDefRef {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        if let Some(fr) = self.from.as_ref() {
            dbwrite!(f, db, "{} &> ", &W(fr))?;
        }
        for (i, field) in self.name.iter().enumerate() {
            if i > 0 {
                dbwrite!(f, db, ".")?;
            }
            dbwrite!(f, db, "{}", &field.atom)?;
        }
        if !self.args.is_empty() {
            write!(f, "[")?;
            for (i, arg) in self.args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                dbwrite!(f, db, "{}", &W(arg))?;
            }
            write!(f, "]")?;
        }
        Ok(())
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for TypePrimitive {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, _db: &DB) -> std::fmt::Result {
        match self {
            TypePrimitive::Mem => write!(f, "<mem>"),
            TypePrimitive::Int => write!(f, "<int>"),
            TypePrimitive::Bit => write!(f, "<bit>"),
            TypePrimitive::Char => write!(f, "<char>"),
        }
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for TypeArray {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self.direction {
            ArrayKind::For => dbwrite!(f, db, "for[{}]", &W(&self.expr)),
            ArrayKind::Each => dbwrite!(f, db, "each[{}]", &W(&self.expr)),
        }
    }
}

#[repr(transparent)]
pub struct W<T>(pub T);

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for W<&Expression<HirConstraintSpanned>> {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match &self.0 .0.as_ref().map_inner(|x| W(&**x)) {
            ExpressionHead::Niladic(a) => a.inner.db_fmt(f, db),
            ExpressionHead::Monadic(Monadic { op, inner }) => match &op.inner {
                expr::ConstraintUnOp::Not => dbwrite!(f, db, "!{}", inner),
                expr::ConstraintUnOp::Dot(a) => dbwrite!(f, db, "{}.{}", inner, a),
            },
            ExpressionHead::Dyadic(Dyadic { op, inner }) => {
                dbwrite!(f, db, "{} {} {}", &inner[0], &op.inner, &inner[1])
            }
            ExpressionHead::Variadic(v) => v.ignore(),
        }
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for W<&Expression<HirValSpanned>> {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match &self.0 .0.as_ref().map_inner(|x| W(&**x)) {
            ExpressionHead::Niladic(a) => a.inner.db_fmt(f, db),
            ExpressionHead::Monadic(Monadic { op, inner }) => match &op.inner {
                expr::ValUnOp::Not => dbwrite!(f, db, "!{}", inner),
                expr::ValUnOp::Neg => dbwrite!(f, db, "-{}", inner),
                expr::ValUnOp::Wiggle(right, kind) => {
                    dbwrite!(f, db, "{} {} {}", inner, kind, &W(&**right))
                }
                expr::ValUnOp::Dot(a, b) => {
                    dbwrite!(f, db, "{}.{}", inner, a)?;
                    if *b {
                        write!(f, "?")?;
                    }
                    Ok(())
                }
            },
            ExpressionHead::Dyadic(Dyadic { op, inner }) => {
                dbwrite!(f, db, "{} {} {}", &inner[0], &op.inner, &inner[1])
            }
            ExpressionHead::Variadic(Variadic { inner, .. }) => {
                dbwrite!(f, db, "{}(", &inner[0])?;
                for (i, arg) in inner[1..].iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    dbwrite!(f, db, "{}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for W<&Expression<HirTypeSpanned>> {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match &self.0 .0.as_ref().map_inner(|x| W(&**x)) {
            ExpressionHead::Niladic(a) => a.inner.db_fmt(f, db),
            ExpressionHead::Monadic(Monadic { op, inner }) => match &op.inner {
                expr::TypeUnOp::Wiggle(right) => dbwrite!(f, db, "{} ~ {}", inner, &W(&**right)),
                expr::TypeUnOp::ByteParser => dbwrite!(f, db, "*{}", inner),
            },
            ExpressionHead::Dyadic(Dyadic { op, inner }) => {
                dbwrite!(f, db, "{} {} {}", &inner[0], &op.inner, &inner[1])
            }
            ExpressionHead::Variadic(Variadic {
                op:
                    OpWithData {
                        inner: expr::TypeVarOp::Call,
                        ..
                    },
                inner,
            }) => {
                dbwrite!(f, db, "{}(", &inner[0])?;
                for (i, arg) in inner[1..].iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    dbwrite!(f, db, "{}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Clone)]
pub struct HirGraph<'a>(pub &'a dyn Hirs);

impl<'a> dot::Labeller<'a, DefId, (DefId, DefId, String, dot::Style)> for HirGraph<'a> {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("HIR").unwrap()
    }

    fn node_id(&'a self, n: &DefId) -> dot::Id<'a> {
        dot::Id::new(n.graphviz_name()).unwrap()
    }

    fn node_label(&'a self, n: &DefId) -> dot::LabelText<'a> {
        let text = match self.0.hir_node(*n) {
            Ok(node) => node.to_db_string(self.0),
            Err(_) => String::from("error"),
        };
        dot::LabelText::label(text)
    }

    fn edge_label(&'a self, e: &(DefId, DefId, String, dot::Style)) -> dot::LabelText<'_> {
        dot::LabelText::label(e.2.clone())
    }

    fn edge_style(&'a self, e: &(DefId, DefId, String, dot::Style)) -> dot::Style {
        e.3
    }
}

fn parser_pred(pp: &ParserPredecessor) -> String {
    match pp {
        ParserPredecessor::ChildOf(_) => "ChildOf",
        ParserPredecessor::After(_) => "After",
    }
    .to_string()
}

impl<'a> dot::GraphWalk<'a, DefId, (DefId, DefId, String, dot::Style)> for HirGraph<'a> {
    fn nodes(&'a self) -> dot::Nodes<'a, DefId> {
        Cow::Owned(self.0.all_def_ids())
    }

    fn edges(&'a self) -> dot::Edges<'a, (DefId, DefId, String, dot::Style)> {
        self.0
            .all_def_ids()
            .iter()
            .flat_map(|&x| self.0.hir_node(x).ok())
            .flat_map(|node| {
                match node {
                    HirNode::Let(LetStatement {
                        id,
                        ty,
                        expr,
                        context,
                    }) => {
                        let mut v = vec![
                            (id.0, expr.0, "expr".to_string(), dot::Style::Bold),
                            (id.0, context.0, "context".to_string(), dot::Style::Dotted),
                        ];
                        if let Some(ty) = ty {
                            v.push((id.0, ty.0, "ty".to_string(), dot::Style::Bold))
                        }
                        v
                    }
                    HirNode::Expr(ValExpression { id, children, .. }) => children
                        .iter()
                        .enumerate()
                        .map(|(i, p)| (id.0, *p, format!("children[{i}]"), dot::Style::Bold))
                        .collect(),
                    HirNode::TExpr(_) => vec![],
                    HirNode::Parse(ParseStatement {
                        id,
                        front,
                        back,
                        expr,
                        ..
                    }) => {
                        let [f, b] = [front.id(), back.id()];
                        let fs = parser_pred(&front);
                        let bs = parser_pred(&back);
                        vec![
                            (id.0, expr.0, "expr".to_string(), dot::Style::Bold),
                            (id.0, f, fs, dot::Style::Dotted),
                            (id.0, b, bs, dot::Style::Dotted),
                        ]
                    }
                    HirNode::Array(ParserArray { id, expr, .. }) => {
                        vec![(id.0, expr.0, "expr".to_string(), dot::Style::Bold)]
                    }
                    HirNode::Block(Block {
                        id,
                        root_context,
                        super_context,
                        ..
                    }) => {
                        let mut v = vec![(
                            id.0,
                            root_context.0,
                            "root_context".to_string(),
                            dot::Style::Bold,
                        )];
                        v.extend(
                            super_context.map(|c| {
                                (id.0, c.0, "super_context".to_string(), dot::Style::Dotted)
                            }),
                        );
                        v
                    }
                    HirNode::Import(Import { id, mod_ref, .. }) => {
                        vec![(id.0, mod_ref.0, "mod_ref".to_string(), dot::Style::Dotted)]
                    }
                    HirNode::ArgDef(ArgDef { id, ty, .. }) => {
                        vec![(id.0, ty.0, "ty".to_string(), dot::Style::Bold)]
                    }
                    HirNode::Choice(StructChoice {
                        id,
                        parent_context,
                        subcontexts,
                        endpoints,
                        ..
                    }) => {
                        let mut v: Vec<_> = subcontexts
                            .iter()
                            .enumerate()
                            .map(|(i, x)| {
                                (id.0, x.0, format!("subcontexts[{i}]"), dot::Style::Bold)
                            })
                            .collect();
                        v.push((
                            id.0,
                            parent_context.0,
                            "parent_context".to_string(),
                            dot::Style::Dotted,
                        ));
                        if let Some([front, back]) = endpoints {
                            let [f, b] = [front.id(), back.id()];
                            let [fs, bs] = [parser_pred(&front), parser_pred(&back)];
                            v.push((id.0, f, fs, dot::Style::Dotted));
                            v.push((id.0, b, bs, dot::Style::Dotted));
                        }
                        v
                    }
                    HirNode::Context(StructCtx {
                        id,
                        block_id,
                        parent_choice,
                        parent_context,
                        children,
                        endpoints,
                        ..
                    }) => {
                        let mut v: Vec<_> = children
                            .iter()
                            .map(|p| {
                                let last_name = self
                                    .0
                                    .lookup_intern_hir_path(*p)
                                    .path()
                                    .iter()
                                    .last()
                                    .unwrap()
                                    .to_db_string(self.0);
                                (id.0, *p, last_name, dot::Style::Bold)
                            })
                            .collect();
                        v.push((id.0, block_id.0, "block_id".to_string(), dot::Style::Dotted));
                        if let Some(p) = parent_choice {
                            v.push((id.0, p.0, "parent_choice".to_string(), dot::Style::Dotted));
                        }
                        if let Some(p) = parent_context {
                            v.push((id.0, p.0, "parent_context".to_string(), dot::Style::Dotted));
                        }
                        if let Some((front, back)) = endpoints {
                            v.push((id.0, front, "front".to_string(), dot::Style::Dotted));
                            v.push((id.0, back, "back".to_string(), dot::Style::Dotted));
                        }
                        v
                    }
                    HirNode::Module(Module { id, defs, imports }) => defs
                        .iter()
                        .map(|(ident, def)| {
                            (
                                id.0,
                                def.0,
                                self.0.lookup_intern_identifier(*ident).name,
                                dot::Style::Bold,
                            )
                        })
                        .chain(imports.iter().map(|(ident, module)| {
                            (
                                id.0,
                                module.0,
                                self.0.lookup_intern_identifier(*ident).name,
                                dot::Style::Bold,
                            )
                        }))
                        .collect(),
                    HirNode::ParserDef(ParserDef {
                        id,
                        from,
                        to,
                        args,
                        ret_ty,
                        ..
                    }) => {
                        let mut v = vec![
                            (id.0, from.0, "from".to_string(), dot::Style::Bold),
                            (id.0, to.0, "to".to_string(), dot::Style::Bold),
                        ];
                        if let Some(args) = args {
                            v.extend(
                                args.iter().enumerate().map(|(i, p)| {
                                    (id.0, p.0, format!("args[{i}]"), dot::Style::Bold)
                                }),
                            );
                        }
                        if let Some(ret_ty) = ret_ty {
                            v.push((id.0, ret_ty.0, "ret_ty".to_string(), dot::Style::Bold));
                        }
                        v
                    }
                    HirNode::ChoiceIndirection(ChoiceIndirection {
                        id,
                        target_choice,
                        choices,
                        ..
                    }) => {
                        let mut v = choices
                            .iter()
                            .map(|(i, nid)| (id.0, *nid, format!("{i}"), dot::Style::Dotted))
                            .collect::<Vec<_>>();
                        v.push((
                            id.0,
                            target_choice.0,
                            "target_choice".to_string(),
                            dot::Style::Dotted,
                        ));
                        v
                    }
                }
                .into_iter()
            })
            .collect()
    }

    fn source(&'a self, edge: &(DefId, DefId, String, dot::Style)) -> DefId {
        edge.0
    }

    fn target(&'a self, edge: &(DefId, DefId, String, dot::Style)) -> DefId {
        edge.1
    }
}
