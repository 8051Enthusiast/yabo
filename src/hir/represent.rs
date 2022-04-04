use crate::types::{NominalKind, PrimitiveType, Type};

use super::*;
use std::fmt::Write;
pub trait HirToString {
    fn hir_to_string(&self, db: &dyn Hirs) -> String;
}

impl HirToString for HirNode {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self {
            HirNode::Let(_) => "Let".to_string(),
            HirNode::Expr(e) => format!("Expr({})", e.hir_to_string(db)),
            HirNode::TExpr(e) => format!("TExpr({})", e.hir_to_string(db)),
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
                format!("({} ~ {})", a.hir_to_string(db), b.hir_to_string(db))
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
            expr::ConstraintBinOp::And(a, b, _) => (a, b, "and"),
            expr::ConstraintBinOp::Or(a, b, _) => (a, b, "or"),
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

impl HirToString for TypeAtom {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self {
            TypeAtom::ParserDef(pd) => pd.hir_to_string(db),
            TypeAtom::Array(arr) => arr.hir_to_string(db),
            TypeAtom::Primitive(p) => p.hir_to_string(db),
            TypeAtom::TypeVar(v) => v.hir_to_string(db),
        }
    }
}

impl HirToString for TypeVar {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        db.lookup_intern_type_var(*self).name
    }
}

impl HirToString for ParserDefRef {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        let from = self.from.as_ref().map(|x| x.hir_to_string(db));
        let args = self
            .args
            .iter()
            .map(|x| x.hir_to_string(db))
            .collect::<Vec<_>>();
        let mut ret = String::new();
        if let Some(f) = from {
            let _ = write!(ret, "{f} &> ");
        }
        let _ = write!(ret, "{}", self.name.atom.hir_to_string(db));
        if !args.is_empty() {
            let _ = write!(ret, "[{}]", args.join(", "));
        }
        ret
    }
}

impl HirToString for TypePrimitive {
    fn hir_to_string(&self, _db: &dyn Hirs) -> String {
        match self {
            TypePrimitive::Mem => "<mem>",
            TypePrimitive::Int => "<int>",
            TypePrimitive::Bit => "<bit>",
            TypePrimitive::Char => "<char>",
        }
        .into()
    }
}

impl HirToString for TypeArray {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self.direction {
            ArrayKind::For => format!("for[{}]", self.expr.hir_to_string(db)),
            ArrayKind::Each => format!("each[{}]", self.expr.hir_to_string(db)),
        }
    }
}

impl HirToString for Atom {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self {
            Atom::Field(FieldName::Ident(id)) => db.lookup_intern_identifier(*id).name,
            Atom::Field(FieldName::Return) => String::from("return"),
            Atom::Field(FieldName::Prev) => String::from("prev"),
            Atom::Field(FieldName::Next) => String::from("next"),
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

impl HirToString for TypeId {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        type_to_string(*self, db)
    }
}

fn type_to_string(ty: TypeId, db: &dyn Hirs) -> String {
    let ty = db.lookup_intern_type(ty);
    match ty {
        Type::Any => String::from("any"),
        Type::Bot => String::from("bot"),
        Type::Primitive(p) => p.hir_to_string(db),
        Type::TypeVarRef(loc, level, index) => format!(
            "<Var Ref ({}, {}, {})>",
            db.lookup_intern_hir_path(loc).to_name(db),
            level,
            index
        ),
        Type::ForAll(inner, vars) => {
            let args = vars
                .iter()
                .map(|x| {
                    x.name
                        .map(|y| db.lookup_intern_type_var(y).name)
                        .unwrap_or(String::from("'_"))
                })
                .collect::<Vec<String>>()
                .join(", ");
            format!("forall {args}. {}", type_to_string(inner, db))
        }
        Type::Nominal(n) => {
            let path = db.lookup_intern_hir_path(n.def).to_name(db);
            let from = n
                .parse_arg
                .map(|x| format!("{} &> ", type_to_string(x, db)))
                .unwrap_or_else(String::new);
            match n.kind {
                NominalKind::Def => {
                    format!("{from}{path}")
                }
                NominalKind::Block => {
                    format!("<anonymous block {from}{path}>")
                }
            }
        }
        Type::Loop(k, inner) => match k {
            ArrayKind::For => format!("for[{}]", type_to_string(inner, db)),
            ArrayKind::Each => format!("each[{}]", type_to_string(inner, db)),
        },
        Type::ParserArg { result, arg } => {
            format!("{} *> {}", type_to_string(arg, db), type_to_string(result, db))
        }
        Type::FunctionArg(res, args) => {
            let args = args
                .iter()
                .map(|x| type_to_string(*x, db))
                .collect::<Vec<String>>()
                .join(", ");
            format!("{}({})", type_to_string(res, db), args)
        }
        Type::Unknown => String::from("<unknown>"),
    }
}

impl HirToString for PrimitiveType {
    fn hir_to_string(&self, _: &dyn Hirs) -> String {
        match self {
            PrimitiveType::Int => String::from("int"),
            PrimitiveType::Bit => String::from("bit"),
            PrimitiveType::Char => String::from("char"),
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
