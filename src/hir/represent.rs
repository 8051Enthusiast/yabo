use crate::databased_display::DatabasedDisplay;

use super::*;
use std::fmt::Write;

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for HirNode {
    fn to_db_string(&self, db: &DB) -> String {
        match self {
            HirNode::Let(_) => "Let".to_string(),
            HirNode::Expr(e) => format!("Expr({})", e.to_db_string(db)),
            HirNode::TExpr(e) => format!("TExpr({})", e.to_db_string(db)),
            HirNode::Parse(_) => "Parse".to_string(),
            HirNode::Array(a) => format!("Array({:?})", a.direction),
            HirNode::Block(_) => "Block".to_string(),
            HirNode::Choice(_) => "Choice".to_string(),
            HirNode::Module(_) => "Module".to_string(),
            HirNode::Context(_) => "Context".to_string(),
            HirNode::ParserDef(_) => "ParserDef".to_string(),
            HirNode::ChoiceIndirection(_) => "ChoiceIndirection".to_string(),
        }
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for ValExpression {
    fn to_db_string(&self, db: &DB) -> String {
        self.expr.to_db_string(db)
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for TypeExpression {
    fn to_db_string(&self, db: &DB) -> String {
        self.expr.to_db_string(db)
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
    fn to_db_string(&self, db: &DB) -> String {
        match self {
            expr::TypeBinOp::Ref(a, b, _) => {
                format!("({} &> {})", a.to_db_string(db), b.to_db_string(db))
            }
            expr::TypeBinOp::ParseArg(a, b, _) => {
                format!("({} *> {})", a.to_db_string(db), b.to_db_string(db))
            }
            expr::TypeBinOp::Wiggle(a, b, _) => {
                format!("({} ~ {})", a.to_db_string(db), b.to_db_string(db))
            }
        }
    }
}

impl<T, S, DB: Hirs + ?Sized> DatabasedDisplay<DB> for expr::TypeUnOp<T, S>
where
    T: ExpressionKind,
    Expression<T>: DatabasedDisplay<DB>,
    S: Clone + Hash + Eq + Debug,
{
    fn to_db_string(&self, db: &DB) -> String {
        match self {
            expr::TypeUnOp::Ref(a, _) => format!("&{}", a.to_db_string(db)),
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
    fn to_db_string(&self, db: &DB) -> String {
        use expr::ValBinOp::*;
        match self {
            Basic(a, op, b, _) => {
                format!("({} {} {})", a.to_db_string(db), op, b.to_db_string(db))
            }
            Wiggle(a, b, _) => {
                format!("({} ~ {})", a.to_db_string(db), b.to_db_string(db))
            }
            Else(a, b, _) => {
                format!("({} else {})", a.to_db_string(db), b.to_db_string(db))
            }
            Dot(a, b, _) => format!("{}.{}", a.to_db_string(db), b.to_db_string(db)),
        }
    }
}

impl<T, S, DB: Hirs + ?Sized> DatabasedDisplay<DB> for expr::ValUnOp<T, S>
where
    T: ExpressionKind,
    Expression<T>: DatabasedDisplay<DB>,
    S: Clone + Hash + Eq + Debug,
{
    fn to_db_string(&self, db: &DB) -> String {
        let (x, op) = match self {
            expr::ValUnOp::Not(a, _) => (a, "!"),
            expr::ValUnOp::Neg(a, _) => (a, "-"),
            expr::ValUnOp::Pos(a, _) => (a, "+"),
            expr::ValUnOp::If(a, _) => (a, "if "),
        };
        format!("{}{}", op, x.to_db_string(db))
    }
}

impl<T, S, DB: Hirs + ?Sized> DatabasedDisplay<DB> for expr::ConstraintBinOp<T, S>
where
    T: ExpressionKind,
    Expression<T>: DatabasedDisplay<DB>,
    S: Clone + Hash + Eq + Debug,
{
    fn to_db_string(&self, db: &DB) -> String {
        let (a, b, op) = match self {
            expr::ConstraintBinOp::And(a, b, _) => (a, b, "and"),
            expr::ConstraintBinOp::Or(a, b, _) => (a, b, "or"),
            expr::ConstraintBinOp::Dot(a, b, _) => {
                return format!("{}.{}", a.to_db_string(db), b.to_db_string(db))
            }
        };
        format!("{} {} {}", a.to_db_string(db), op, b.to_db_string(db))
    }
}

impl<T, S, DB: Hirs + ?Sized> DatabasedDisplay<DB> for expr::ConstraintUnOp<T, S>
where
    T: ExpressionKind,
    Expression<T>: DatabasedDisplay<DB>,
    S: Clone + Hash + Eq + Debug,
{
    fn to_db_string(&self, db: &DB) -> String {
        match self {
            expr::ConstraintUnOp::Not(a, _) => format!("!{}", a.to_db_string(db)),
        }
    }
}

impl<T, DB: Hirs + ?Sized> DatabasedDisplay<DB> for IndexSpanned<T>
where
    T: DatabasedDisplay<DB>,
{
    fn to_db_string(&self, db: &DB) -> String {
        self.atom.to_db_string(db)
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for ParserAtom {
    fn to_db_string(&self, db: &DB) -> String {
        match self {
            ParserAtom::Atom(atom) => atom.to_db_string(db),
            ParserAtom::Single => "~".to_string(),
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

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for TypeAtom {
    fn to_db_string(&self, db: &DB) -> String {
        match self {
            TypeAtom::ParserDef(pd) => pd.to_db_string(db),
            TypeAtom::Array(arr) => arr.to_db_string(db),
            TypeAtom::Primitive(p) => p.to_db_string(db),
            TypeAtom::TypeVar(v) => v.to_db_string(db),
        }
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for TypeVar {
    fn to_db_string(&self, db: &DB) -> String {
        db.lookup_intern_type_var(*self).name
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for ParserDefRef {
    fn to_db_string(&self, db: &DB) -> String {
        let from = self.from.as_ref().map(|x| x.to_db_string(db));
        let args = self
            .args
            .iter()
            .map(|x| x.to_db_string(db))
            .collect::<Vec<_>>();
        let mut ret = String::new();
        if let Some(f) = from {
            let _ = write!(ret, "{f} &> ");
        }
        let _ = write!(ret, "{}", self.name.atom.to_db_string(db));
        if !args.is_empty() {
            let _ = write!(ret, "[{}]", args.join(", "));
        }
        ret
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for TypePrimitive {
    fn to_db_string(&self, _db: &DB) -> String {
        match self {
            TypePrimitive::Mem => "<mem>",
            TypePrimitive::Int => "<int>",
            TypePrimitive::Bit => "<bit>",
            TypePrimitive::Char => "<char>",
        }
        .into()
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for TypeArray {
    fn to_db_string(&self, db: &DB) -> String {
        match self.direction {
            ArrayKind::For => format!("for[{}]", self.expr.to_db_string(db)),
            ArrayKind::Each => format!("each[{}]", self.expr.to_db_string(db)),
        }
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for Atom {
    fn to_db_string(&self, db: &DB) -> String {
        match self {
            Atom::Field(FieldName::Ident(id)) => db.lookup_intern_identifier(*id).name,
            Atom::Field(FieldName::Return) => String::from("return"),
            Atom::Field(FieldName::Prev) => String::from("prev"),
            Atom::Field(FieldName::Next) => String::from("next"),
            Atom::Number(a) | Atom::Char(a) | Atom::String(a) => a.clone(),
        }
    }
}

impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for Identifier {
    fn to_db_string(&self, db: &DB) -> String {
        db.lookup_intern_identifier(*self).name
    }
}

// somehow this does not work with a blanket implementation
// maybe because it is circular and defaults to "not implemented" instead of "implemented"
impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for Expression<HirConstraint> {
    fn to_db_string(&self, db: &DB) -> String {
        match self {
            Expression::BinaryOp(a) => a.to_db_string(db),
            Expression::UnaryOp(a) => a.to_db_string(db),
            Expression::Atom(a) => a.to_db_string(db),
        }
    }
}
impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for Expression<HirVal> {
    fn to_db_string(&self, db: &DB) -> String {
        match self {
            Expression::BinaryOp(a) => a.to_db_string(db),
            Expression::UnaryOp(a) => a.to_db_string(db),
            Expression::Atom(a) => a.to_db_string(db),
        }
    }
}
impl<DB: Hirs + ?Sized> DatabasedDisplay<DB> for Expression<HirType> {
    fn to_db_string(&self, db: &DB) -> String {
        match self {
            Expression::BinaryOp(a) => a.to_db_string(db),
            Expression::UnaryOp(a) => a.to_db_string(db),
            Expression::Atom(a) => a.to_db_string(db),
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
                                    .to_name(self.0);
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
