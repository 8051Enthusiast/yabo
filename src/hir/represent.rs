use super::*;
pub trait HirToString {
    fn hir_to_string(&self, db: &dyn Hirs) -> String;
}

impl HirToString for HirNode {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self {
            HirNode::Let(_) => format!("Let"),
            HirNode::Expr(e) => format!("Expr({})", e.hir_to_string(db)),
            HirNode::TExpr(e) => format!("TExpr({})", e.hir_to_string(db)),
            HirNode::Parse(_) => format!("Parse"),
            HirNode::Array(a) => format!("Array({:?})", a.direction),
            HirNode::Block(_) => format!("Block"),
            HirNode::Choice(_) => format!("Choice"),
            HirNode::Module(_) => format!("Module"),
            HirNode::Context(_) => format!("Context"),
            HirNode::ParserDef(_) => format!("ParserDef"),
            HirNode::ChoiceIndirection(_) => format!("ChoiceIndirection"),
        }
    }
}

impl HirToString for ValExpression {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        self.expr.hir_to_string(db)
    }
}

impl HirToString for TypeExpression {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        self.expr.hir_to_string(db)
    }
}

impl<T, S, R> HirToString for expr::TypeBinOp<T, S, R>
where
    T: ExpressionKind,
    S: ExpressionKind,
    Expression<T>: HirToString,
    Expression<S>: HirToString,
    R: Clone + Hash + Eq + Debug,
{
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self {
            expr::TypeBinOp::Ref(a, b, _) => {
                format!("({} &> {})", a.hir_to_string(db), b.hir_to_string(db))
            }
            expr::TypeBinOp::ParseArg(a, b, _) => {
                format!("({} *> {})", a.hir_to_string(db), b.hir_to_string(db))
            }
            expr::TypeBinOp::Wiggle(a, b, _) => {
                format!("({} ~ {})", a.hir_to_string(db), b.hir_to_string(db))
            }
        }
    }
}

impl<T, S> HirToString for expr::TypeUnOp<T, S>
where
    T: ExpressionKind,
    Expression<T>: HirToString,
    S: Clone + Hash + Eq + Debug,
{
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self {
            expr::TypeUnOp::Ref(a, _) => format!("&{}", a.hir_to_string(db)),
        }
    }
}

impl<T, S, R> HirToString for expr::ValBinOp<T, S, R>
where
    T: ExpressionKind,
    S: ExpressionKind,
    Expression<T>: HirToString,
    Expression<S>: HirToString,
    R: Clone + Hash + Eq + Debug,
{
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        use expr::ValBinOp::*;
        match self {
            Basic(a, op, b, _) => {
                format!("({} {} {})", a.hir_to_string(db), op, b.hir_to_string(db))
            }
            Wiggle(a, b, _) => {
                format!("({} *> {})", a.hir_to_string(db), b.hir_to_string(db))
            }
            Else(a, b, _) => {
                format!("({} else {})", a.hir_to_string(db), b.hir_to_string(db))
            }
            Dot(a, b, _) => format!("{}.{}", a.hir_to_string(db), b.hir_to_string(db)),
        }
    }
}

impl<T, S> HirToString for expr::ValUnOp<T, S>
where
    T: ExpressionKind,
    Expression<T>: HirToString,
    S: Clone + Hash + Eq + Debug,
{
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        let (x, op) = match self {
            expr::ValUnOp::Not(a, _) => (a, "!"),
            expr::ValUnOp::Neg(a, _) => (a, "-"),
            expr::ValUnOp::Pos(a, _) => (a, "+"),
            expr::ValUnOp::If(a, _) => (a, "if "),
        };
        format!("{}{}", op, x.hir_to_string(db))
    }
}

impl<T, S> HirToString for expr::ConstraintBinOp<T, S>
where
    T: ExpressionKind,
    Expression<T>: HirToString,
    S: Clone + Hash + Eq + Debug,
{
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        let (a, b, op) = match self {
            expr::ConstraintBinOp::And(a, b, _) => (a, b, "&&"),
            expr::ConstraintBinOp::Or(a, b, _) => (a, b, "||"),
            expr::ConstraintBinOp::Dot(a, b, _) => {
                return format!("{}.{}", a.hir_to_string(db), b.hir_to_string(db))
            }
        };
        format!("{} {} {}", a.hir_to_string(db), op, b.hir_to_string(db))
    }
}

impl<T, S> HirToString for expr::ConstraintUnOp<T, S>
where
    T: ExpressionKind,
    Expression<T>: HirToString,
    S: Clone + Hash + Eq + Debug,
{
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self {
            expr::ConstraintUnOp::Not(a, _) => format!("!{}", a.hir_to_string(db)),
        }
    }
}

impl<T> HirToString for IndexSpanned<T>
where
    T: HirToString,
{
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        self.atom.hir_to_string(db)
    }
}

impl HirToString for ParserAtom {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self {
            ParserAtom::Atom(atom) => atom.hir_to_string(db),
            ParserAtom::Array(id) => format!(
                "array({})",
                db.lookup_intern_hir_path(id.0)
                    .path()
                    .iter()
                    .last()
                    .unwrap()
                    .to_name(db)
            ),
            ParserAtom::Block(id) => format!(
                "block({})",
                db.lookup_intern_hir_path(id.0)
                    .path()
                    .iter()
                    .last()
                    .unwrap()
                    .to_name(db)
            ),
        }
    }
}

impl HirToString for TypeAtom {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self {
            TypeAtom::Id(id) => id.hir_to_string(db),
            TypeAtom::Array(arr) => arr.hir_to_string(db),
        }
    }
}

impl HirToString for TypeArray {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self.direction.inner {
            ArrayKind::For => format!("for[{}]", self.expr.hir_to_string(db)),
            ArrayKind::Each => format!("each[{}]", self.expr.hir_to_string(db)),
        }
    }
}

impl HirToString for Atom {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self {
            Atom::Id(id) => db.lookup_intern_identifier(*id).name,
            Atom::Number(a) | Atom::Char(a) | Atom::String(a) => a.clone(),
        }
    }
}

impl HirToString for Identifier {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        db.lookup_intern_identifier(*self).name
    }
}

// somehow this does not work with a blanket implementation
// maybe because it is circular and defaults to "not implemented" instead of "implemented"
impl HirToString for Expression<HirConstraint> {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self {
            Expression::BinaryOp(a) => a.hir_to_string(db),
            Expression::UnaryOp(a) => a.hir_to_string(db),
            Expression::Atom(a) => a.hir_to_string(db),
        }
    }
}
impl HirToString for Expression<HirVal> {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self {
            Expression::BinaryOp(a) => a.hir_to_string(db),
            Expression::UnaryOp(a) => a.hir_to_string(db),
            Expression::Atom(a) => a.hir_to_string(db),
        }
    }
}
impl HirToString for Expression<HirType> {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self {
            Expression::BinaryOp(a) => a.hir_to_string(db),
            Expression::UnaryOp(a) => a.hir_to_string(db),
            Expression::Atom(a) => a.hir_to_string(db),
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
            Ok(node) => node.hir_to_string(self.0),
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
                            (id.0, expr.0, format!("expr"), dot::Style::Bold),
                            (id.0, ty.0, format!("ty"), dot::Style::Bold),
                            (id.0, context.0, format!("context"), dot::Style::Dotted),
                        ]
                    }
                    HirNode::Expr(ValExpression { id, children, .. }) => children
                        .iter()
                        .enumerate()
                        .map(|(i, p)| (id.0, *p, format!("children[{}]", i), dot::Style::Bold))
                        .collect(),
                    HirNode::TExpr(_) => vec![],
                    HirNode::Parse(ParseStatement { id, prev, expr }) => {
                        let p = match prev {
                            ParserPredecessor::ChildOf(p) | ParserPredecessor::After(p) => p,
                        };
                        let s = match prev {
                            ParserPredecessor::ChildOf(_) => format!("ChildOf"),
                            ParserPredecessor::After(_) => format!("After"),
                        };
                        vec![
                            (id.0, expr.0, format!("expr"), dot::Style::Bold),
                            (id.0, p, s, dot::Style::Dotted),
                        ]
                    }
                    HirNode::Array(ParserArray { id, expr, .. }) => {
                        vec![(id.0, expr.0, format!("expr"), dot::Style::Bold)]
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
                            format!("root_context"),
                            dot::Style::Bold,
                        )];
                        v.extend(
                            super_context
                                .map(|c| (id.0, c.0, format!("super_context"), dot::Style::Dotted)),
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
                            format!("parent_context"),
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
                                    .to_name(self.0);
                                (id.0, *p, last_name, dot::Style::Bold)
                            })
                            .collect();
                        v.push((id.0, block_id.0, format!("block_id"), dot::Style::Dotted));
                        if let Some(p) = parent_choice {
                            v.push((id.0, p.0, format!("parent_choice"), dot::Style::Dotted));
                        }
                        if let Some(p) = parent_context {
                            v.push((id.0, p.0, format!("parent_context"), dot::Style::Dotted));
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
                            (id.0, from.0, format!("from"), dot::Style::Bold),
                            (id.0, to.0, format!("to"), dot::Style::Bold),
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
                            format!("target_choice"),
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
