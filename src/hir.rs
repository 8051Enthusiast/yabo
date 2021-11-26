use std::{
    cell::RefCell,
    collections::{btree_map::Entry, BTreeMap},
};

use crate::{
    ast::{self, ArrayKind, AstConstraint, AstParse, AstVal},
    expr::{self, Atom, ExprConverter, Expression, ExpressionKind},
    interner::{HirId, HirPath, Identifier, PathComponent},
    source::{FileId, Span, Spanned},
};
#[salsa::query_group(HirDatabase)]
pub trait Hirs: ast::Asts {
    fn hir_parser_collection(
        &self,
        file: FileId,
        id: Identifier,
    ) -> Result<Option<HirParserCollection>, ()>;
    fn hir_node(&self, id: HirId) -> Result<HirNode, ()>;
}

fn hir_parser_collection(
    db: &dyn Hirs,
    file: FileId,
    id: Identifier,
) -> Result<Option<HirParserCollection>, ()> {
    let collection = HirParserCollection::new();
    let ctx = HirConversionCtx::new(collection, db);
    let path = HirPath::new(file, id);
    let hid = db.intern_hir_path(path.clone());
    let parser = match db.top_level_statement(file, id)? {
        None => return Ok(None),
        Some(x) => x,
    };
    parser_def(&parser, &ctx, ParserDefId(hid));
    Ok(Some(ctx.collection.into_inner()))
}

fn hir_node(db: &dyn Hirs, id: HirId) -> Result<HirNode, ()> {
    let path = db.lookup_intern_hir_path(id);
    let file = match path.path()[0] {
        PathComponent::File(f) => f,
        _ => panic!("Hir path {} does not start with file id", db.path_name(id)),
    };
    let fid = match path.path()[1] {
        PathComponent::Named(n) => n,
        _ => panic!(
            "Hir path {} does not have identifier as second element",
            db.path_name(id)
        ),
    };
    let hir_ctx = db
        .hir_parser_collection(file, fid)?
        .unwrap_or_else(|| panic!("Access to inexistent HIR path {}", db.path_name(id)));
    Ok(hir_ctx
        .map
        .get(&id)
        .unwrap_or_else(|| panic!("Access to inexistent HIR path {}", db.path_name(id)))
        .clone())
}

macro_rules! hir_id_wrapper {
    {type $name:ident = $id:ident ($ty:ty);} => {
        #[derive(Clone, Copy, Hash, PartialEq, Eq, Debug, PartialOrd, Ord)]
        pub struct $name(pub HirId);
        impl HirIdWrapper for $name {
            type Inner = $ty;
            fn id(self) -> HirId {
                self.0
            }
            fn extract(node: HirNode) -> Self::Inner {
                match node {
                    HirNode::$id(x) => x,
                    _ => panic!(
                        "Accessing hir node should have resulted in {} variant but did not",
                        stringify!($id)
                    ),
                }
            }
        }
    };
    {$(type $name:ident = $id:ident ($ty:ty);)*} => {
        $(hir_id_wrapper!{type $name= $id($ty);})*
    };
}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum HirNode {
    Let(LetStatement),
    Expr(ValExpression),
    PExpr(ParseExpression),
    Parse(ParseStatement),
    Array(ParserArray),
    Block(Block),
    Choice(StructChoice),
    Context(StructCtx),
    ParserDef(ParserDef),
}

hir_id_wrapper! {
    type LetId = Let(LetStatement);
    type ExprId = Expr(ValExpression);
    type PExprId = PExpr(ParseExpression);
    type ParseId = Parse(ParseStatement);
    type ArrayId = Array(ParserArray);
    type BlockId = Block(Block);
    type ChoiceId = Choice(StructChoice);
    type ContextId = Context(StructCtx);
    type ParserDefId = ParserDef(ParserDef);
}

pub trait HirIdWrapper: Copy {
    type Inner;
    fn id(self) -> HirId;
    fn extract(node: HirNode) -> Self::Inner;
    fn extend<DB: Hirs + ?Sized>(self, db: &DB, add: PathComponent) -> HirId {
        let id = self.id();
        extend_hir_id(db, id, add)
    }
    fn lookup<DB: Hirs + ?Sized>(self, db: &DB) -> Result<Self::Inner, ()> {
        let id = self.id();
        Ok(Self::extract(db.hir_node(id)?))
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct HirParserCollection {
    map: BTreeMap<HirId, HirNode>,
    spans: BTreeMap<HirId, Vec<Span>>,
}

impl HirParserCollection {
    pub fn new() -> Self {
        Self {
            map: BTreeMap::new(),
            spans: BTreeMap::new(),
        }
    }
}

pub struct HirConversionCtx<'a> {
    collection: RefCell<HirParserCollection>,
    db: &'a dyn Hirs,
}

impl<'a> HirConversionCtx<'a> {
    pub fn new(collection: HirParserCollection, db: &'a dyn Hirs) -> Self {
        Self {
            collection: RefCell::new(collection),
            db,
        }
    }

    fn insert(&self, id: HirId, node: HirNode, span: Vec<Span>) {
        let mut borrow = self.collection.borrow_mut();
        borrow.map.insert(id, node);
        borrow.spans.insert(id, span);
    }
}

impl<'a> dot::Labeller<'a, HirId, (HirId, HirId, String, dot::Style)> for HirConversionCtx<'a> {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("test").unwrap()
    }

    fn node_id(&'a self, n: &HirId) -> dot::Id<'a> {
        dot::Id::new(n.graphviz_name()).unwrap()
    }

    fn node_label(&'a self, n: &HirId) -> dot::LabelText<'a> {
        let node = self.collection.borrow().map[n].hir_to_string(self.db);
        dot::LabelText::label(node)
    }

    fn edge_label(&'a self, e: &(HirId, HirId, String, dot::Style)) -> dot::LabelText<'_> {
        dot::LabelText::label(e.2.clone())
    }

    fn edge_style(&'a self, e: &(HirId, HirId, String, dot::Style)) -> dot::Style {
        e.3
    }
}

impl<'a> dot::GraphWalk<'a, HirId, (HirId, HirId, String, dot::Style)> for HirConversionCtx<'a> {
    fn nodes(&'a self) -> dot::Nodes<'a, HirId> {
        self.collection.borrow().map.keys().copied().collect()
    }

    fn edges(&'a self) -> dot::Edges<'a, (HirId, HirId, String, dot::Style)> {
        self.collection
            .borrow()
            .map
            .iter()
            .flat_map(|(_id, node)| {
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
                    HirNode::PExpr(ParseExpression { id, children, .. }) => children
                        .iter()
                        .enumerate()
                        .map(|(i, p)| (id.0, *p, format!("children[{}]", i), dot::Style::Bold))
                        .collect(),
                    HirNode::Parse(ParseStatement { id, prev, expr }) => {
                        let p = match prev {
                            ParserPredecessor::ChildOf(p) | ParserPredecessor::After(p) => *p,
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
                                    .db
                                    .lookup_intern_hir_path(*p)
                                    .path()
                                    .iter()
                                    .last()
                                    .unwrap()
                                    .to_name(self.db);
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
                    HirNode::ParserDef(ParserDef { id, from, to }) => {
                        vec![
                            (id.0, from.0, format!("from"), dot::Style::Bold),
                            (id.0, to.0, format!("to"), dot::Style::Bold),
                        ]
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct SpanIndex(u32);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirConstraint {}
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct IndexSpanned<T> {
    pub atom: T,
    pub span: SpanIndex,
}

impl ExpressionKind for HirConstraint {
    type BinaryOp = expr::ConstraintBinOp<HirConstraint, SpanIndex>;
    type UnaryOp = expr::ConstraintUnOp<HirConstraint, SpanIndex>;
    type Atom = IndexSpanned<Atom>;
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirParse {}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ParseExpression {
    id: PExprId,
    expr: Expression<HirParse>,
    children: Vec<HirId>,
}

impl ExpressionKind for HirParse {
    type BinaryOp = expr::ParseBinOp<HirParse, HirConstraint, SpanIndex>;
    type UnaryOp = expr::ParseUnOp<HirParse, SpanIndex>;
    type Atom = IndexSpanned<ParserAtom>;
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirVal {}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ValExpression {
    id: ExprId,
    expr: Expression<HirVal>,
    children: Vec<HirId>,
}

impl ExpressionKind for HirVal {
    type BinaryOp = expr::ValBinOp<HirVal, HirParse, SpanIndex>;
    type UnaryOp = expr::ValUnOp<HirVal, SpanIndex>;
    type Atom = IndexSpanned<Atom>;
}

fn constraint_expression_converter<'a>(
    add_span: &'a impl Fn(&Span) -> SpanIndex,
) -> ExprConverter<'a, AstConstraint, HirConstraint> {
    type Converter<'b> = ExprConverter<'b, ast::AstConstraint, HirConstraint>;
    let bin_fun =
        move |bop: &ast::AstConstraintBinOp, c: &Converter| bop.convert_same(c, &add_span);
    let un_fun = move |uop: &ast::AstConstraintUnOp, c: &Converter| uop.convert_same(c, &add_span);
    let atom_fun = move |atom: &Spanned<Atom>| {
        let n = add_span(&atom.span);
        IndexSpanned {
            atom: atom.inner.clone(),
            span: n,
        }
    };
    ExprConverter::new(bin_fun, un_fun, atom_fun)
}

fn parse_expression_converter<'a>(
    ctx: &'a HirConversionCtx<'a>,
    add_span: &'a impl Fn(&Span) -> SpanIndex,
    new_id: &'a impl Fn() -> HirId,
    parent_context: Option<ContextId>,
    array: Option<ArrayKind>,
) -> ExprConverter<'a, AstParse, HirParse> {
    let atom_fun = move |x: &Spanned<ast::ParserAtom>| -> IndexSpanned<ParserAtom> {
        let n = add_span(&x.span);
        let atom = match &x.inner {
            ast::ParserAtom::Array(array) => {
                let nid = ArrayId(new_id());
                parser_array(array, ctx, nid, parent_context);
                ParserAtom::Array(nid)
            }
            ast::ParserAtom::Atom(atom) => ParserAtom::Atom(atom.clone()),
            ast::ParserAtom::Block(b) => {
                let nid = BlockId(new_id());
                block(b, ctx, nid, parent_context, array);
                ParserAtom::Block(nid)
            }
        };
        IndexSpanned { atom, span: n }
    };
    let constraint_converter = constraint_expression_converter(add_span);
    type PConverter<'b> = ExprConverter<'b, ast::AstParse, HirParse>;
    let pbin_fun = move |bop: &ast::AstParseBinOp, c: &PConverter| {
        bop.convert_same(c, &constraint_converter, add_span)
    };
    let pun_fun = move |bop: &ast::AstParseUnOp, c: &PConverter| bop.convert_same(c, add_span);
    ExprConverter::new(pbin_fun, pun_fun, atom_fun)
}

fn parse_expression(
    ast: &ast::ParseExpression,
    ctx: &HirConversionCtx,
    id: PExprId,
    parent_context: Option<ContextId>,
    array: Option<ArrayKind>,
) {
    let spans = RefCell::new(Vec::new());
    let children = RefCell::new(Vec::new());
    let add_span = |span: &Span| {
        let mut borrow = spans.borrow_mut();
        borrow.push(*span);
        SpanIndex(u32::try_from(borrow.len()).unwrap() - 1)
    };
    let new_id = || {
        let mut borrow = children.borrow_mut();
        let index: u32 = u32::try_from(borrow.len()).unwrap();
        let new_id = id.extend(ctx.db, PathComponent::Unnamed(index));
        borrow.push(new_id);
        new_id
    };
    let pconverter = parse_expression_converter(ctx, &add_span, &new_id, parent_context, array);
    let expr = pconverter.convert(ast);
    drop(pconverter);
    let pexpr = ParseExpression {
        id,
        expr,
        children: children.into_inner(),
    };
    ctx.insert(id.0, HirNode::PExpr(pexpr), spans.into_inner())
}

fn val_expression_converter<'a>(
    ctx: &'a HirConversionCtx<'a>,
    add_span: &'a impl Fn(&Span) -> SpanIndex,
    new_id: &'a impl Fn() -> HirId,
) -> ExprConverter<'a, AstVal, HirVal> {
    let parse_converter = parse_expression_converter(ctx, add_span, new_id, None, None);
    type Converter<'b> = ExprConverter<'b, ast::AstVal, HirVal>;
    let bin_fun = move |bop: &ast::AstValBinOp, c: &Converter| {
        bop.convert_same(c, &parse_converter, &add_span)
    };
    let un_fun = move |uop: &ast::AstValUnOp, c: &Converter| uop.convert_same(c, &add_span);
    let atom_fun = move |atom: &Spanned<Atom>| {
        let n = add_span(&atom.span);
        IndexSpanned {
            atom: atom.inner.clone(),
            span: n,
        }
    };
    ExprConverter::new(bin_fun, un_fun, atom_fun)
}

fn val_expression(ast: &ast::ValExpression, ctx: &HirConversionCtx, id: ExprId) {
    let spans = RefCell::new(Vec::new());
    let children = RefCell::new(Vec::new());
    let add_span = |span: &Span| {
        let mut borrow = spans.borrow_mut();
        borrow.push(*span);
        SpanIndex(u32::try_from(borrow.len()).unwrap() - 1)
    };
    let new_id = || {
        let mut borrow = children.borrow_mut();
        let index: u32 = u32::try_from(borrow.len()).unwrap();
        let new_id = id.extend(ctx.db, PathComponent::Unnamed(index));
        borrow.push(new_id);
        new_id
    };
    let vconverter = val_expression_converter(ctx, &add_span, &new_id);
    let expr = vconverter.convert(ast);
    drop(vconverter);
    let pexpr = ValExpression {
        id,
        expr,
        children: children.into_inner(),
    };
    ctx.insert(id.0, HirNode::Expr(pexpr), spans.into_inner())
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDef {
    id: ParserDefId,
    from: PExprId,
    to: PExprId,
}

fn parser_def(ast: &ast::ParserDefinition, ctx: &HirConversionCtx, id: ParserDefId) {
    let from = PExprId(id.extend(ctx.db, PathComponent::Unnamed(0)));
    let to = PExprId(id.extend(ctx.db, PathComponent::Unnamed(1)));
    parse_expression(&ast.from, ctx, from, None, None);
    parse_expression(&ast.to, ctx, to, None, None);
    let pdef = ParserDef { id, from, to };
    ctx.insert(id.0, HirNode::ParserDef(pdef), vec![ast.span]);
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
struct ParentInfo {
    parent_choice: Option<ChoiceId>,
    parent_context: Option<ContextId>,
    block_id: BlockId,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Block {
    id: BlockId,
    root_context: ContextId,
    super_context: Option<ContextId>,
    array: Option<ArrayKind>,
}

fn block(
    ast: &ast::Block,
    ctx: &HirConversionCtx,
    id: BlockId,
    super_context: Option<ContextId>,
    array: Option<ArrayKind>,
) {
    let context_id = ContextId(id.extend(ctx.db, PathComponent::Unnamed(0)));
    let parents = ParentInfo {
        parent_choice: None,
        parent_context: None,
        block_id: id,
    };
    match &ast.content {
        Some(c) => {
            struct_context(c, ctx, context_id, &parents);
        }
        None => {
            empty_struct_context(ctx, context_id, &parents, ast.span);
        }
    };
    let block = Block {
        id,
        root_context: context_id,
        super_context,
        array,
    };
    ctx.insert(id.0, HirNode::Block(block), vec![ast.span]);
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct StructChoice {
    pub id: ChoiceId,
    pub parent_context: ContextId,
    pub subcontexts: Vec<ContextId>,
}

fn struct_choice(
    ast: &ast::ParserChoice,
    ctx: &HirConversionCtx,
    id: ChoiceId,
    parents: &ParentInfo,
) -> VariableSet {
    let children = extract_non_choice(ast);
    let parents = ParentInfo {
        parent_choice: Some(id),
        ..parents.clone()
    };
    let mut subcontexts = Vec::new();
    let mut varset = None;
    for (idx, child) in children.into_iter().enumerate() {
        let subcontext_id = ContextId(id.extend(ctx.db, PathComponent::Unnamed(idx as u32)));
        subcontexts.push(subcontext_id);
        let new_vars = struct_context(child, ctx, subcontext_id, &parents);
        varset = varset
            .map(|x: VariableSet| x.merge_sum(&new_vars, id))
            .or(Some(new_vars));
    }
    let choice = StructChoice {
        id,
        parent_context: parents.parent_context.unwrap(),
        subcontexts,
    };
    ctx.insert(id.0, HirNode::Choice(choice), vec![ast.span]);
    varset.unwrap_or_default()
}

fn extract_non_choice(ast: &ast::ParserChoice) -> Vec<&ast::BlockContent> {
    let mut left = match &ast.left {
        ast::BlockContent::Choice(c) => extract_non_choice(c),
        otherwise => vec![otherwise],
    };
    let mut right = match &ast.right {
        ast::BlockContent::Choice(c) => extract_non_choice(c),
        otherwise => vec![otherwise],
    };
    left.append(&mut right);
    left
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct StructCtx {
    pub id: ContextId,
    pub block_id: BlockId,
    pub parent_choice: Option<ChoiceId>,
    pub parent_context: Option<ContextId>,
    pub vars: Box<VariableSet>,
    pub children: Box<Vec<HirId>>,
}

fn empty_struct_context(
    ctx: &HirConversionCtx,
    id: ContextId,
    parents: &ParentInfo,
    span: Span,
) -> VariableSet {
    let varset = VariableSet::new();
    let context = StructCtx {
        id,
        block_id: parents.block_id,
        parent_choice: parents.parent_choice,
        parent_context: parents.parent_context,
        vars: Box::new(varset.clone()),
        children: Box::new(Vec::new()),
    };
    ctx.insert(id.0, HirNode::Context(context), vec![span]);
    varset
}

fn struct_context(
    ast: &ast::BlockContent,
    ctx: &HirConversionCtx,
    id: ContextId,
    parents: &ParentInfo,
) -> VariableSet {
    let children = match ast {
        ast::BlockContent::Sequence(x) => x.content.iter().collect(),
        otherwise => vec![otherwise],
    };
    let mut children_id = Vec::new();
    let old_parents = parents.clone();
    let parents = ParentInfo {
        parent_context: Some(id),
        ..old_parents
    };
    let mut index = 0;
    let mut varset = VariableSet::new();
    let mut pred = ParserPredecessor::ChildOf(id.0);
    for child in children {
        let mut next_index = || {
            index += 1;
            PathComponent::Unnamed(index - 1)
        };
        let new_set = match child {
            ast::BlockContent::Choice(c) => {
                let sub_id = ChoiceId(id.extend(ctx.db, next_index()));
                children_id.push(sub_id.0);
                struct_choice(&c, ctx, sub_id, &parents)
            }
            ast::BlockContent::Statement(x) => {
                let name = x.id().map(PathComponent::Named).unwrap_or_else(next_index);
                let sub_id = id.extend(ctx.db, name);
                children_id.push(sub_id);
                match x.as_ref() {
                    ast::Statement::ParserDef(_) => {
                        panic!("Nested parser definitions are not yet implemented")
                    }
                    ast::Statement::Parse(p) => {
                        let set =
                            parse_statement(p, ctx, ParseId(sub_id), pred, parents.parent_context);
                        pred = ParserPredecessor::After(sub_id);
                        set
                    }
                    ast::Statement::Let(l) => let_statement(l, ctx, LetId(sub_id), id),
                }
            }
            ast::BlockContent::Sequence(_) => unreachable!(),
        };
        let (result_set, duplicate) = varset.merge_product(&new_set);
        varset = result_set;
    }
    let context = StructCtx {
        id,
        block_id: old_parents.block_id,
        parent_choice: old_parents.parent_choice,
        parent_context: old_parents.parent_context,
        vars: Box::new(varset.clone()),
        children: Box::new(children_id),
    };
    ctx.insert(id.0, HirNode::Context(context), vec![ast.span()]);
    varset
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct LetStatement {
    pub id: LetId,
    pub ty: PExprId,
    pub expr: ExprId,
    pub context: ContextId,
}

fn let_statement(
    ast: &ast::LetStatement,
    ctx: &HirConversionCtx,
    id: LetId,
    context: ContextId,
) -> VariableSet {
    let val_id = ExprId(id.extend(ctx.db, PathComponent::Unnamed(0)));
    let ty_id = PExprId(id.extend(ctx.db, PathComponent::Unnamed(1)));
    val_expression(&ast.expr, ctx, val_id);
    parse_expression(&ast.ty, ctx, ty_id, None, None);
    let st = LetStatement {
        id,
        ty: ty_id,
        expr: val_id,
        context,
    };
    ctx.insert(id.0, HirNode::Let(st), vec![ast.span, ast.name.span]);
    VariableSet::singular(ast.name.id, id.0)
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ParserPredecessor {
    ChildOf(HirId),
    After(HirId),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParseStatement {
    pub id: ParseId,
    pub prev: ParserPredecessor,
    pub expr: PExprId,
}

fn parse_statement(
    ast: &ast::ParseStatement,
    ctx: &HirConversionCtx,
    id: ParseId,
    prev: ParserPredecessor,
    parent_context: Option<ContextId>,
) -> VariableSet {
    let expr = PExprId(id.extend(ctx.db, PathComponent::Unnamed(0)));
    parse_expression(&ast.parser, ctx, expr, parent_context, None);
    let pt = ParseStatement { id, prev, expr };
    let spans = match &ast.name {
        Some(s) => vec![ast.span, s.span],
        None => vec![ast.span],
    };
    ctx.insert(id.0, HirNode::Parse(pt), spans);
    ast.name
        .as_ref()
        .map(|name| VariableSet::singular(name.id, id.0))
        .unwrap_or_default()
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ParserAtom {
    Atom(Atom),
    Array(ArrayId),
    Block(BlockId),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserArray {
    pub id: ArrayId,
    pub direction: ArrayKind,
    pub expr: PExprId,
}

fn parser_array(
    ast: &ast::ParserArray,
    ctx: &HirConversionCtx,
    id: ArrayId,
    parent_context: Option<ContextId>,
) {
    let expr = PExprId(id.extend(ctx.db, PathComponent::Unnamed(0)));
    parse_expression(
        &ast.expr,
        ctx,
        expr,
        parent_context,
        Some(ast.direction.inner.clone()),
    );
    let pa = ParserArray {
        id,
        direction: ast.direction.inner.clone(),
        expr,
    };
    ctx.insert(id.0, HirNode::Array(pa), vec![ast.span, ast.direction.span]);
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct VariableSet {
    set: BTreeMap<Identifier, VarStatus>,
}

impl VariableSet {
    pub fn new() -> Self {
        VariableSet {
            set: BTreeMap::new(),
        }
    }
    pub fn singular(id: Identifier, hir_id: HirId) -> Self {
        let mut set = BTreeMap::new();
        set.insert(id, VarStatus::Owned(hir_id));
        VariableSet { set }
    }
    pub fn merge_sum(&self, other: &Self, id: ChoiceId) -> Self {
        let id = id.0;
        let mut new = BTreeMap::new();
        new.extend(self.set.iter().map(|(k, v)| {
            (*k, {
                VarStatus::from_accessibility(v.is_accessible() && other.set.contains_key(k), id)
            })
        }));
        for (k, v) in other.set.iter() {
            let other_entry = v.is_accessible();
            new.entry(*k)
                .and_modify(|e| {
                    *e = VarStatus::from_accessibility(e.is_accessible() && other_entry, id)
                })
                .or_insert(VarStatus::Existent(id));
        }
        VariableSet { set: new }
    }
    pub fn merge_product(&self, other: &Self) -> (Self, Vec<Identifier>) {
        let mut set = self.set.clone();
        let mut doubled = Vec::new();
        for (k, v) in other.set.iter() {
            match set.entry(*k) {
                Entry::Vacant(entry) => {
                    entry.insert(*v);
                }
                Entry::Occupied(_) => doubled.push(*k),
            }
        }
        (VariableSet { set }, doubled)
    }
}

impl Default for VariableSet {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum VarStatus {
    /// the current contexts owns the variable
    Owned(HirId),
    /// the current context does not own the variable,
    /// but every variant of a choice has it, making
    /// it always accessible, the HirId points to the choice
    Present(HirId),
    /// the variable does exist somewhere under the current
    /// context, but is not always available because it is only
    /// in parts of a choice, the HirId points to the choice
    Existent(HirId),
}

impl VarStatus {
    fn is_accessible(self) -> bool {
        !matches!(self, Self::Existent(_))
    }
    fn from_accessibility(access: bool, id: HirId) -> Self {
        if access {
            Self::Present(id)
        } else {
            Self::Existent(id)
        }
    }
}

fn extend_hir_id<DB: Hirs + ?Sized>(db: &DB, id: HirId, add: PathComponent) -> HirId {
    let mut path = db.lookup_intern_hir_path(id);
    path.push(add);
    db.intern_hir_path(path)
}

trait HirToString {
    fn hir_to_string(&self, db: &dyn Hirs) -> String;
}

impl HirToString for HirNode {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self {
            HirNode::Let(_) => format!("Let"),
            HirNode::Expr(e) => format!("Expr({})", e.hir_to_string(db)),
            HirNode::PExpr(e) => format!("PExpr({})", e.hir_to_string(db)),
            HirNode::Parse(_) => format!("Parse"),
            HirNode::Array(a) => format!("Array({:?})", a.direction),
            HirNode::Block(_) => format!("Block"),
            HirNode::Choice(_) => format!("Choice"),
            HirNode::Context(_) => format!("Context"),
            HirNode::ParserDef(_) => format!("ParserDef"),
        }
    }
}

impl HirToString for ValExpression {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        self.expr.hir_to_string(db)
    }
}

impl HirToString for ParseExpression {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        self.expr.hir_to_string(db)
    }
}

impl<T, S, R> HirToString for expr::ValBinOp<T, S, R>
where
    T: ExpressionKind,
    S: ExpressionKind,
    Expression<T>: HirToString,
    Expression<S>: HirToString,
    R: Clone + std::hash::Hash + Eq + std::fmt::Debug,
{
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        use expr::ValBinOp::*;
        match self {
            Basic(a, op, b, _) => {
                format!("({} {} {})", a.hir_to_string(db), op, b.hir_to_string(db))
            }
            Pipe(a, b, _) => {
                format!("({} |> {})", a.hir_to_string(db), b.hir_to_string(db))
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
    S: Clone + std::hash::Hash + Eq + std::fmt::Debug,
{
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        let (x, op) = match self {
            expr::ValUnOp::Not(a, _) => (a, "!"),
            expr::ValUnOp::Neg(a, _) => (a, "-"),
            expr::ValUnOp::Pos(a, _) => (a, "+"),
        };
        format!("{}{}", op, x.hir_to_string(db))
    }
}

impl<T, S, R> HirToString for expr::ParseBinOp<T, S, R>
where
    T: ExpressionKind,
    S: ExpressionKind,
    Expression<T>: HirToString,
    Expression<S>: HirToString,
    R: Clone + std::hash::Hash + Eq + std::fmt::Debug,
{
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        use expr::ParseBinOp::*;
        match self {
            Dot(a, b, _) => format!("{}.{}", a.hir_to_string(db), b.hir_to_string(db)),
            Pipe(a, b, _) => {
                format!("({} |> {})", a.hir_to_string(db), b.hir_to_string(db))
            }
            Wiggle(a, b, _) => {
                format!("({} ~ {})", a.hir_to_string(db), b.hir_to_string(db))
            }
        }
    }
}

impl<T, S> HirToString for expr::ParseUnOp<T, S>
where
    T: ExpressionKind,
    Expression<T>: HirToString,
    S: Clone + std::hash::Hash + Eq + std::fmt::Debug,
{
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        let (x, op) = match self {
            expr::ParseUnOp::If(a, _) => (a, "if"),
            expr::ParseUnOp::Try(a, _) => (a, "try"),
        };
        format!("{} {}", op, x.hir_to_string(db))
    }
}

impl<T, S> HirToString for expr::ConstraintBinOp<T, S>
where
    T: ExpressionKind,
    Expression<T>: HirToString,
    S: Clone + std::hash::Hash + Eq + std::fmt::Debug,
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
    S: Clone + std::hash::Hash + Eq + std::fmt::Debug,
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

impl HirToString for Atom {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self {
            Atom::Id(id) => db.lookup_intern_identifier(*id).name,
            Atom::Number(a) | Atom::Char(a) | Atom::String(a) => a.clone(),
        }
    }
}

// somehow this does not work with a blanket implementation
// maybe because it is circular and defaults to "not implemented" instead of "implemented"
impl HirToString for Expression<HirParse> {
    fn hir_to_string(&self, db: &dyn Hirs) -> String {
        match self {
            Expression::BinaryOp(a) => a.hir_to_string(db),
            Expression::UnaryOp(a) => a.hir_to_string(db),
            Expression::Atom(a) => a.hir_to_string(db),
        }
    }
}

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
#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::Context;
    #[test]
    fn nested_choice() {
        let ctx = Context::mock(
            r#"
parser complex = for [u8] *> {
    (
        a: u64;
        b: u32;
    |
        a: u64;
    )
}
        "#,
        );
        let main = FileId::default();
        let expr1 = ctx.id("complex");
        let collection = ctx.db.hir_parser_collection(main, expr1).unwrap().unwrap();
        let hirctx = HirConversionCtx::new(collection, &ctx.db);
        dot::render(&hirctx, &mut std::io::stdout());
    }
}
