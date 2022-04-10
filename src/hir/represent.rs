use crate::{databased_display::DatabasedDisplay, dbwrite, ast::ArrayKind};

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
            HirNode::Module(_) => write!(f, "Module"),
            HirNode::Context(_) => write!(f, "Context"),
            HirNode::ParserDef(_) => write!(f, "ParserDef"),
            HirNode::ChoiceIndirection(_) => write!(f, "ChoiceIndirection"),
        }
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for ValExpression {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        self.expr.db_fmt(f, db)
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for TypeExpression {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        self.expr.db_fmt(f, db)
    }
}

impl<T, S, R, DB: Hirs + ?Sized> DatabasedDisplay<DB> for expr::TypeBinOp<T, S, R>
where
    T: ExpressionKind,
    S: ExpressionKind,
    Expression<T>: DatabasedDisplay<DB>,
    Expression<S>: DatabasedDisplay<DB>,
    R: Clone + Hash + Eq + Debug,
{
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            expr::TypeBinOp::Ref(a, b, _) => dbwrite!(f, db, "({} &> {})", a, b),
            expr::TypeBinOp::ParseArg(a, b, _) => dbwrite!(f, db, "({} *> {})", a, b),
            expr::TypeBinOp::Wiggle(a, b, _) => dbwrite!(f, db, "({} ~ {})", a, b),
        }
    }
}

impl<T, S, DB: Hirs + ?Sized> DatabasedDisplay<DB> for expr::TypeUnOp<T, S>
where
    T: ExpressionKind,
    Expression<T>: DatabasedDisplay<DB>,
    S: Clone + Hash + Eq + Debug,
{
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            expr::TypeUnOp::Ref(a, _) => dbwrite!(f, db, "&{}", a),
        }
    }
}

impl<T, S, R, DB: Hirs + ?Sized> DatabasedDisplay<DB> for expr::ValBinOp<T, S, R>
where
    T: ExpressionKind,
    S: ExpressionKind,
    Expression<T>: DatabasedDisplay<DB>,
    Expression<S>: DatabasedDisplay<DB>,
    R: Clone + Hash + Eq + Debug,
{
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        use expr::ValBinOp::*;
        match self {
            Basic(a, op, b, _) => dbwrite!(f, db, "({} {} {})", a, op, b),
            Wiggle(a, b, _) => dbwrite!(f, db, "({} ~ {})", a, b),
            Else(a, b, _) => dbwrite!(f, db, "({} else {})", a, b),
            Dot(a, b, _) => dbwrite!(f, db, "{}.{}", a, b),
        }
    }
}

impl<T, S, DB: Hirs + ?Sized> DatabasedDisplay<DB> for expr::ValUnOp<T, S>
where
    T: ExpressionKind,
    Expression<T>: DatabasedDisplay<DB>,
    S: Clone + Hash + Eq + Debug,
{
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        let (x, op) = match self {
            expr::ValUnOp::Not(a, _) => (a, "!"),
            expr::ValUnOp::Neg(a, _) => (a, "-"),
            expr::ValUnOp::Pos(a, _) => (a, "+"),
            expr::ValUnOp::If(a, _) => (a, "if "),
        };
        dbwrite!(f, db, "{}{}", &op, x)
    }
}

impl<T, S, DB: Hirs + ?Sized> DatabasedDisplay<DB> for expr::ConstraintBinOp<T, S>
where
    T: ExpressionKind,
    Expression<T>: DatabasedDisplay<DB>,
    S: Clone + Hash + Eq + Debug,
{
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        let (a, b, op) = match self {
            expr::ConstraintBinOp::And(a, b, _) => (a, b, "and"),
            expr::ConstraintBinOp::Or(a, b, _) => (a, b, "or"),
            expr::ConstraintBinOp::Dot(a, b, _) => return dbwrite!(f, db, "{}.{}", a, b),
        };
        dbwrite!(f, db, "{} {} {}", a, &op, b)
    }
}

impl<T, S, DB: Hirs + ?Sized> DatabasedDisplay<DB> for expr::ConstraintUnOp<T, S>
where
    T: ExpressionKind,
    Expression<T>: DatabasedDisplay<DB>,
    S: Clone + Hash + Eq + Debug,
{
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            expr::ConstraintUnOp::Not(a, _) => dbwrite!(f, db, "!{}", a),
        }
    }
}

impl<T, DB: Hirs + ?Sized> DatabasedDisplay<DB> for IndexSpanned<T>
where
    T: DatabasedDisplay<DB>,
{
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        self.atom.db_fmt(f, db)
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for ParserAtom {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            ParserAtom::Atom(atom) => atom.db_fmt(f, db),
            ParserAtom::Single => dbwrite!(f, db, "~"),
            ParserAtom::Array(id) => dbwrite!(
                f,
                db,
                "array({})",
                db.lookup_intern_hir_path(id.0)
                    .path()
                    .iter()
                    .last()
                    .unwrap()
            ),
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
            dbwrite!(f, db, "{} &> ", fr)?;
        }
        dbwrite!(f, db, "{}", &self.name.atom)?;
        if !self.args.is_empty() {
            write!(f, "[")?;
            for (i, arg) in self.args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                dbwrite!(f, db, "{}", arg)?;
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
            ArrayKind::For => dbwrite!(f, db, "for[{}]", &self.expr),
            ArrayKind::Each => dbwrite!(f, db, "each[{}]", &self.expr),
        }
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for Atom {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            Atom::Field(FieldName::Ident(id)) => dbwrite!(f, db, "{}", id),
            Atom::Field(FieldName::Return) => write!(f, "return"),
            Atom::Number(a) | Atom::Char(a) | Atom::String(a) => write!(f, "{}", a),
        }
    }
}

// somehow this does not work with a blanket implementation
// maybe because it is circular and defaults to "not implemented" instead of "implemented"
impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for Expression<HirConstraint> {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            Expression::BinaryOp(a) => a.db_fmt(f, db),
            Expression::UnaryOp(a) => a.db_fmt(f, db),
            Expression::Atom(a) => a.db_fmt(f, db),
        }
    }
}
impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for Expression<HirVal> {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            Expression::BinaryOp(a) => a.db_fmt(f, db),
            Expression::UnaryOp(a) => a.db_fmt(f, db),
            Expression::Atom(a) => a.db_fmt(f, db),
        }
    }
}
impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for Expression<HirType> {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            Expression::BinaryOp(a) => a.db_fmt(f, db),
            Expression::UnaryOp(a) => a.db_fmt(f, db),
            Expression::Atom(a) => a.db_fmt(f, db),
        }
    }
}

#[derive(Clone)]
pub struct HirGraph<'a>(pub &'a dyn Hirs);

impl<'a> dot::Labeller<'a, HirId, (HirId, HirId, String, dot::Style)> for HirGraph<'a> {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("HIR").unwrap()
    }

    fn node_id(&'a self, n: &HirId) -> dot::Id<'a> {
        dot::Id::new(n.graphviz_name()).unwrap()
    }

    fn node_label(&'a self, n: &HirId) -> dot::LabelText<'a> {
        let text = match self.0.hir_node(*n) {
            Ok(node) => node.to_db_string(self.0),
            Err(_) => String::from("error"),
        };
        dot::LabelText::label(text)
    }

    fn edge_label(&'a self, e: &(HirId, HirId, String, dot::Style)) -> dot::LabelText<'_> {
        dot::LabelText::label(e.2.clone())
    }

    fn edge_style(&'a self, e: &(HirId, HirId, String, dot::Style)) -> dot::Style {
        e.3
    }
}

impl<'a> dot::GraphWalk<'a, HirId, (HirId, HirId, String, dot::Style)> for HirGraph<'a> {
    fn nodes(&'a self) -> dot::Nodes<'a, HirId> {
        Cow::Owned(self.0.all_hir_ids())
    }

    fn edges(&'a self) -> dot::Edges<'a, (HirId, HirId, String, dot::Style)> {
        self.0
            .all_hir_ids()
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
                        vec![
                            (id.0, expr.0, "expr".to_string(), dot::Style::Bold),
                            (id.0, ty.0, "ty".to_string(), dot::Style::Bold),
                            (id.0, context.0, "context".to_string(), dot::Style::Dotted),
                        ]
                    }
                    HirNode::Expr(ValExpression { id, children, .. }) => children
                        .iter()
                        .enumerate()
                        .map(|(i, p)| (id.0, *p, format!("children[{}]", i), dot::Style::Bold))
                        .collect(),
                    HirNode::TExpr(_) => vec![],
                    HirNode::Parse(ParseStatement { id, prev, expr, .. }) => {
                        let p = match prev {
                            ParserPredecessor::ChildOf(p) | ParserPredecessor::After(p) => p,
                        };
                        let s = match prev {
                            ParserPredecessor::ChildOf(_) => "ChildOf".to_string(),
                            ParserPredecessor::After(_) => "After".to_string(),
                        };
                        vec![
                            (id.0, expr.0, "expr".to_string(), dot::Style::Bold),
                            (id.0, p, s, dot::Style::Dotted),
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
                    HirNode::Choice(StructChoice {
                        id,
                        parent_context,
                        subcontexts,
                    }) => {
                        let mut v: Vec<_> = subcontexts
                            .iter()
                            .enumerate()
                            .map(|(i, x)| {
                                (id.0, x.0, format!("subcontexts[{}]", i), dot::Style::Bold)
                            })
                            .collect();
                        v.push((
                            id.0,
                            parent_context.0,
                            "parent_context".to_string(),
                            dot::Style::Dotted,
                        ));
                        v
                    }
                    HirNode::Context(StructCtx {
                        id,
                        block_id,
                        parent_choice,
                        parent_context,
                        children,
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
                        v
                    }
                    HirNode::Module(Module { id, defs, submods }) => defs
                        .iter()
                        .map(|(ident, def)| {
                            (
                                id.0,
                                def.0,
                                self.0.lookup_intern_identifier(*ident).name,
                                dot::Style::Bold,
                            )
                        })
                        .chain(submods.iter().map(|(ident, module)| {
                            (
                                id.0,
                                module.0,
                                self.0.lookup_intern_identifier(*ident).name,
                                dot::Style::Bold,
                            )
                        }))
                        .collect(),
                    HirNode::ParserDef(ParserDef { id, from, to }) => {
                        vec![
                            (id.0, from.0, "from".to_string(), dot::Style::Bold),
                            (id.0, to.0, "to".to_string(), dot::Style::Bold),
                        ]
                    }
                    HirNode::ChoiceIndirection(ChoiceIndirection {
                        id,
                        target_choice,
                        choices,
                        ..
                    }) => {
                        let mut v = choices
                            .iter()
                            .map(|(i, nid)| (id.0, *nid, format!("{}", i), dot::Style::Dotted))
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

    fn source(&'a self, edge: &(HirId, HirId, String, dot::Style)) -> HirId {
        edge.0
    }

    fn target(&'a self, edge: &(HirId, HirId, String, dot::Style)) -> HirId {
        edge.1
    }
}
