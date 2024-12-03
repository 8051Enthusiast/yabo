use yaboc_base::{databased_display::DatabasedDisplay, dbwrite};
use yaboc_expr::Expression as NewExpression;

use super::*;

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for HirNode {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            HirNode::Let(_) => write!(f, "Let"),
            HirNode::Expr(e) => dbwrite!(f, db, "Expr({})", e),
            HirNode::TExpr(e) => dbwrite!(f, db, "TExpr({})", e),
            HirNode::Parse(_) => write!(f, "Parse"),
            HirNode::Block(_) => write!(f, "Block"),
            HirNode::Choice(_) => write!(f, "Choice"),
            HirNode::Import(_) => write!(f, "Import"),
            HirNode::ArgDef(_) => write!(f, "ArgDef"),
            HirNode::Lambda(_) => write!(f, "Lambda"),
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
        use yaboc_expr::WriteEvent::*;
        self.expr.expr.try_print(|event| match event {
            Niladic(a) => a.db_fmt(f, db),
            OpenMonadic(expr::ValUnOp::Not) => write!(f, "!"),
            OpenMonadic(expr::ValUnOp::Neg) => write!(f, "-"),
            CloseMonadic(expr::ValUnOp::Wiggle(right, kind)) => {
                dbwrite!(f, db, " {} {}", kind, right)
            }
            CloseMonadic(expr::ValUnOp::Dot(a, acc)) => {
                dbwrite!(f, db, "{acc}{}", a)
            }
            InterDyadic(op) => write!(f, " {} ", op),
            InterVariadic(_, 0) => write!(f, "("),
            InterVariadic(_, 1..) => write!(f, ", "),
            CloseVariadic(_) => write!(f, ")"),
            _ => Ok(()),
        })
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
            ParserAtom::ArrayFill => dbwrite!(f, db, "[..]"),
            ParserAtom::Span(start, end) => dbwrite!(f, db, "{}..{}", start, end),
            ParserAtom::Regex(regex) => regex.db_fmt(f, db),
            ParserAtom::String(string) => string.fmt(f),
            ParserAtom::Block(id, _) => dbwrite!(f, db, "block({})", &id.0.unwrap_unnamed_id(db)),
            ParserAtom::Lambda(id) => dbwrite!(f, db, "lambda({})", &id.0.unwrap_unnamed_id(db)),
        }
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for TypeAtom {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            TypeAtom::ParserDef(pd) => pd.db_fmt(f, db),
            TypeAtom::Array(arr) => arr.db_fmt(f, db),
            TypeAtom::Primitive(p) => p.db_fmt(f, db),
            TypeAtom::Placeholder => write!(f, "_"),
        }
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for ParserDefRef {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        for (i, field) in self.path.iter().enumerate() {
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
            TypePrimitive::Int => write!(f, "<int>"),
            TypePrimitive::Bit => write!(f, "<bit>"),
            TypePrimitive::Char => write!(f, "<char>"),
            TypePrimitive::U8 => write!(f, "<u8>"),
        }
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for TypeArray {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self.direction {
            ArrayKind::Each => dbwrite!(f, db, "[{}]", &W(&self.expr)),
        }
    }
}

#[repr(transparent)]
pub struct W<T>(pub T);

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for HirConstraintId {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        use yaboc_expr::WriteEvent::*;
        db.lookup_intern_hir_constraint(*self)
            .expr
            .try_print(|event| match event {
                Niladic(x) => x.db_fmt(f, db),
                OpenMonadic(ConstraintUnOp::Not) => dbwrite!(f, db, "!"),
                CloseMonadic(ConstraintUnOp::Dot(right)) => dbwrite!(f, db, ".{}", right),
                InterDyadic(op) => dbwrite!(f, db, " {} ", op),
                _ => Ok(()),
            })
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for W<&DataExpr<HirType, SpanIndex>> {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        self.0.expr.try_print(|event| match event {
            yaboc_expr::WriteEvent::Niladic(a) => a.db_fmt(f, db),
            yaboc_expr::WriteEvent::OpenMonadic(expr::TypeUnOp::ByteParser) => write!(f, "*"),
            yaboc_expr::WriteEvent::CloseMonadic(expr::TypeUnOp::Wiggle(right)) => {
                dbwrite!(f, db, " ~ {}", right)
            }
            yaboc_expr::WriteEvent::InterDyadic(op) => write!(f, " {} ", op),
            yaboc_expr::WriteEvent::InterVariadic(expr::TypeVarOp::Call, 0) => write!(f, "("),
            yaboc_expr::WriteEvent::InterVariadic(expr::TypeVarOp::Call, 1..) => write!(f, ", "),
            yaboc_expr::WriteEvent::CloseVariadic(expr::TypeVarOp::Call) => write!(f, ")"),
            _ => Ok(()),
        })
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

    fn edge_label(&'a self, e: &(DefId, DefId, String, dot::Style)) -> dot::LabelText<'a> {
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
                        if let Some(ty) = ty {
                            vec![(id.0, ty.0, "ty".to_string(), dot::Style::Bold)]
                        } else {
                            vec![]
                        }
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
                                let last_name = p.unwrap_path_end(self.0).to_db_string(self.0);
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
                        let mut v = vec![(id.0, to.0, "to".to_string(), dot::Style::Bold)];
                        if let Some(from) = from {
                            v.push((id.0, from.0, "from".to_string(), dot::Style::Bold))
                        }
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
                    HirNode::Lambda(Lambda { id, expr, args, .. }) => {
                        let mut v = vec![(id.0, expr.0, "expr".to_string(), dot::Style::Bold)];
                        v.extend(
                            args.iter()
                                .enumerate()
                                .map(|(i, p)| (id.0, p.0, format!("args[{i}]"), dot::Style::Bold)),
                        );
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
