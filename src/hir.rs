mod recursion;

use std::{
    borrow::Cow,
    cell::RefCell,
    collections::{btree_map::Entry, BTreeMap, BTreeSet},
    fmt::Debug,
    hash::Hash,
    sync::Arc,
};

use crate::{
    ast::{self, ArrayKind, AstConstraint, AstType, AstVal},
    expr::{self, Atom, ExprConverter, Expression, ExpressionKind},
    interner::{HirId, HirPath, Identifier, PathComponent},
    source::{FileId, Span, Spanned},
};

//use recursion::{mod_parser_ssc, parser_ssc, FunctionSscId};

#[salsa::query_group(HirDatabase)]
pub trait Hirs: ast::Asts {
    fn hir_parser_collection(&self, hid: HirId) -> Result<Option<HirParserCollection>, ()>;
    fn hir_node(&self, id: HirId) -> Result<HirNode, ()>;
    //    #[salsa::interned]
    //    fn intern_recursion_scc(&self, functions: Vec<ParserDefId>) -> recursion::FunctionSscId;
    //    fn mod_parser_ssc(
    //        &self,
    //        module: ModuleId,
    //    ) -> Result<Arc<BTreeMap<ParserDefId, FunctionSscId>>, ()>;
    //    fn parser_ssc(&self, parser: ParserDefId) -> Result<FunctionSscId, ()>;
    fn root_id(&self) -> ModuleId;
    fn all_hir_ids(&self) -> Vec<HirId>;
    fn hir_parent_block(&self, id: HirId) -> Result<Option<BlockId>, ()>;
    fn hir_parent_module(&self, id: HirId) -> Result<ModuleId, ()>;
}

fn hir_parser_collection(db: &dyn Hirs, hid: HirId) -> Result<Option<HirParserCollection>, ()> {
    let collection = HirParserCollection::new();
    let ctx = HirConversionCtx::new(collection, db);
    let path = db.lookup_intern_hir_path(hid);
    let file = path.path()[0].unwrap_file();
    let id = path.path()[1].unwrap_named();
    let parser = match db.top_level_statement(file, id)? {
        None => return Ok(None),
        Some(x) => x,
    };
    parser_def(&parser, &ctx, ParserDefId(hid));
    Ok(Some(ctx.collection.into_inner()))
}

fn hir_node(db: &dyn Hirs, id: HirId) -> Result<HirNode, ()> {
    let path = db.lookup_intern_hir_path(id);
    let file = path.path()[0].unwrap_file();
    let fid = match path.path().get(1) {
        Some(PathComponent::Named(n)) => n,
        None => return module_file(db, file).map(HirNode::Module),
        _ => panic!(
            "Hir path {} does not have identifier as second element",
            db.path_name(id)
        ),
    };
    let collection_id = db.intern_hir_path(HirPath::new_fid(file, *fid));
    let hir_ctx = db
        .hir_parser_collection(collection_id)?
        .unwrap_or_else(|| panic!("Access to inexistent HIR path {}", db.path_name(id)));
    Ok(hir_ctx
        .map
        .get(&id)
        .unwrap_or_else(|| panic!("Access to inexistent HIR path {}", db.path_name(id)))
        .clone())
}

fn root_id(db: &dyn Hirs) -> ModuleId {
    let root_path = HirPath::new_file(FileId::default());
    ModuleId(db.intern_hir_path(root_path))
}

fn all_hir_ids(db: &dyn Hirs) -> Vec<HirId> {
    let root = db.root_id();
    let module = match root.lookup(db) {
        Ok(m) => m,
        Err(_) => return Vec::new(),
    };
    let collections: Vec<_> = module
        .defs
        .values()
        .map(|id| db.hir_parser_collection(id.0))
        .collect();
    let mut ret = vec![root.0];
    for c in collections {
        let collection = match c {
            Err(_) | Ok(None) => continue,
            Ok(Some(col)) => col,
        };
        ret.extend(collection.map.keys().cloned())
    }
    ret
}

fn hir_parent_block(db: &dyn Hirs, id: HirId) -> Result<Option<BlockId>, ()> {
    match db.hir_node(id)? {
        HirNode::Let(x) => hir_parent_block(db, x.context.0),
        HirNode::Block(x) => match x.super_context {
            None => Ok(None),
            Some(ctx) => hir_parent_block(db, ctx.0),
        },
        HirNode::Choice(x) => hir_parent_block(db, x.parent_context.0),
        HirNode::Context(x) => Ok(Some(x.block_id)),
        HirNode::Module(_) | HirNode::ParserDef(_) => Ok(None),
        _ => hir_parent_block(db, id.parent(db)),
    }
}

fn hir_parent_module(db: &dyn Hirs, id: HirId) -> Result<ModuleId, ()> {
    // todo
    Ok(ModuleId(db.intern_hir_path(HirPath::new_file(
        db.lookup_intern_hir_path(id).path()[0].unwrap_file(),
    ))))
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
    TExpr(TypeExpression),
    Parse(ParseStatement),
    Array(ParserArray),
    Block(Block),
    Choice(StructChoice),
    Module(Module),
    Context(StructCtx),
    ParserDef(ParserDef),
    ChoiceIndirection(ChoiceIndirection),
}

impl HirNode {
    pub fn children(&self) -> Vec<HirId> {
        match self {
            HirNode::Let(x) => x.children(),
            HirNode::Expr(x) => x.children(),
            HirNode::TExpr(x) => x.children(),
            HirNode::Parse(x) => x.children(),
            HirNode::Array(x) => x.children(),
            HirNode::Block(x) => x.children(),
            HirNode::Choice(x) => x.children(),
            HirNode::Module(x) => x.children(),
            HirNode::Context(x) => x.children(),
            HirNode::ParserDef(x) => x.children(),
            HirNode::ChoiceIndirection(x) => x.children(),
        }
    }
}

hir_id_wrapper! {
    type LetId = Let(LetStatement);
    type ExprId = Expr(ValExpression);
    type TExprId = TExpr(TypeExpression);
    type ParseId = Parse(ParseStatement);
    type ArrayId = Array(ParserArray);
    type BlockId = Block(Block);
    type ChoiceId = Choice(StructChoice);
    type ModuleId = Module(Module);
    type ContextId = Context(StructCtx);
    type ParserDefId = ParserDef(ParserDef);
    type ChoiceIndirectId = ChoiceIndirection(ChoiceIndirection);
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
                            .enumerate()
                            .flat_map(|(i, x)| x.map(|y| (i, y)))
                            .map(|(i, nid)| (id.0, nid, format!("{}", i), dot::Style::Dotted))
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct SpanIndex(u32);
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct IndexSpanned<T> {
    pub atom: T,
    pub span: SpanIndex,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Module {
    id: ModuleId,
    defs: BTreeMap<Identifier, ParserDefId>,
    submods: BTreeMap<Identifier, ModuleId>,
}

impl Module {
    pub fn children(&self) -> Vec<HirId> {
        self.defs
            .values()
            .map(|x| x.0)
            .chain(self.submods.values().map(|x| x.0))
            .collect()
    }
}

fn module_file(db: &dyn Hirs, file: FileId) -> Result<Module, ()> {
    let id = ModuleId(db.intern_hir_path(HirPath::new_file(file)));
    let defs = db
        .symbols(file)?
        .iter()
        .map(|sym| {
            (
                *sym,
                ParserDefId(db.intern_hir_path(HirPath::new_fid(file, *sym))),
            )
        })
        .collect();
    Ok(Module {
        id,
        defs,
        submods: BTreeMap::new(),
    })
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirConstraint;

impl ExpressionKind for HirConstraint {
    type BinaryOp = expr::ConstraintBinOp<HirConstraint, SpanIndex>;
    type UnaryOp = expr::ConstraintUnOp<HirConstraint, SpanIndex>;
    type Atom = IndexSpanned<Atom>;
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirVal;

impl ExpressionKind for HirVal {
    type BinaryOp = expr::ValBinOp<HirVal, HirConstraint, SpanIndex>;
    type UnaryOp = expr::ValUnOp<HirVal, SpanIndex>;
    type Atom = IndexSpanned<ParserAtom>;
}
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ValExpression {
    id: ExprId,
    expr: Expression<HirVal>,
    children: Vec<HirId>,
}

impl ValExpression {
    fn children(&self) -> Vec<HirId> {
        self.children.clone()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirType;

impl ExpressionKind for HirType {
    type BinaryOp = expr::TypeBinOp<HirType, HirConstraint, SpanIndex>;
    type UnaryOp = expr::TypeUnOp<HirType, SpanIndex>;
    type Atom = IndexSpanned<TypeAtom>;
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct TypeExpression {
    id: TExprId,
    expr: Expression<HirType>,
}

impl TypeExpression {
    fn children(&self) -> Vec<HirId> {
        Vec::new()
    }
}

fn constraint_expression_converter<'a>(
    add_span: &'a impl Fn(&Span) -> SpanIndex,
) -> ExprConverter<'a, AstConstraint, HirConstraint> {
    type Converter<'b> = ExprConverter<'b, ast::AstConstraint, HirConstraint>;
    let bin_fun =
        move |bop: &ast::AstConstraintBinOp, c: &Converter| bop.convert_same(c, &add_span);
    let un_fun = move |uop: &ast::AstConstraintUnOp, c: &Converter| uop.convert_same(c, &add_span);
    let atom_fun = move |atom: &Spanned<Atom>, _: &Converter| {
        let n = add_span(&atom.span);
        IndexSpanned {
            atom: atom.inner.clone(),
            span: n,
        }
    };
    ExprConverter::new(bin_fun, un_fun, atom_fun)
}

fn val_expression_converter<'a>(
    ctx: &'a HirConversionCtx<'a>,
    add_span: &'a impl Fn(&Span) -> SpanIndex,
    new_id: &'a impl Fn() -> HirId,
    parent_context: Option<ContextId>,
    array: Option<ArrayKind>,
) -> ExprConverter<'a, AstVal, HirVal> {
    type VConverter<'b> = ExprConverter<'b, ast::AstVal, HirVal>;
    let atom_fun =
        move |x: &Spanned<ast::ParserAtom>, _: &VConverter| -> IndexSpanned<ParserAtom> {
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
    let pbin_fun = move |bop: &ast::AstValBinOp, c: &VConverter| {
        bop.convert_same(c, &constraint_converter, add_span)
    };
    let pun_fun = move |bop: &ast::AstValUnOp, c: &VConverter| bop.convert_same(c, add_span);
    ExprConverter::new(pbin_fun, pun_fun, atom_fun)
}

fn type_expression_converter<'a>(
    add_span: &'a impl Fn(&Span) -> SpanIndex,
) -> ExprConverter<'a, AstType, HirType> {
    type Converter<'b> = ExprConverter<'b, ast::AstType, HirType>;
    let constr = constraint_expression_converter(add_span);
    let bin_fun =
        move |bop: &ast::AstTypeBinOp, c: &Converter| bop.convert_same(c, &constr, &add_span);
    let un_fun = move |uop: &ast::AstTypeUnOp, c: &Converter| uop.convert_same(c, &add_span);
    let atom_fun = move |atom: &Spanned<ast::TypeAtom>, c: &Converter| {
        let n = add_span(&atom.span);
        let new_atom = match &atom.inner {
            ast::TypeAtom::Id(id) => TypeAtom::Id(*id),
            ast::TypeAtom::Array(arr) => {
                let new_expr = c.convert(&arr.expr);
                TypeAtom::Array(Box::new(TypeArray {
                    direction: arr.direction.clone(),
                    expr: new_expr,
                }))
            }
        };
        IndexSpanned {
            atom: new_atom,
            span: n,
        }
    };
    ExprConverter::new(bin_fun, un_fun, atom_fun)
}

fn val_expression(
    ast: &ast::ValExpression,
    ctx: &HirConversionCtx,
    id: ExprId,
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
    let vconverter = val_expression_converter(ctx, &add_span, &new_id, parent_context, array);
    let expr = vconverter.convert(ast);
    drop(vconverter);
    let expr = ValExpression {
        id,
        expr,
        children: children.into_inner(),
    };
    ctx.insert(id.0, HirNode::Expr(expr), spans.into_inner())
}

fn type_expression(ast: &ast::TypeExpression, ctx: &HirConversionCtx, id: TExprId) {
    let spans = RefCell::new(Vec::new());
    let add_span = |span: &Span| {
        let mut borrow = spans.borrow_mut();
        borrow.push(*span);
        SpanIndex(u32::try_from(borrow.len()).unwrap() - 1)
    };
    let tconverter = type_expression_converter(&add_span);
    let texpr = tconverter.convert(ast);
    drop(tconverter);
    let texpr = TypeExpression { id, expr: texpr };
    ctx.insert(id.0, HirNode::TExpr(texpr), spans.into_inner())
}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDef {
    id: ParserDefId,
    from: TExprId,
    to: ExprId,
}

impl ParserDef {
    pub fn children(&self) -> Vec<HirId> {
        vec![self.from.0, self.to.0]
    }
}

fn parser_def(ast: &ast::ParserDefinition, ctx: &HirConversionCtx, id: ParserDefId) {
    let from = TExprId(id.extend(ctx.db, PathComponent::Unnamed(0)));
    let to = ExprId(id.extend(ctx.db, PathComponent::Unnamed(1)));
    type_expression(&ast.from, ctx, from);
    val_expression(&ast.to, ctx, to, None, None);
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

impl Block {
    pub fn children(&self) -> Vec<HirId> {
        vec![self.root_context.0]
    }
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
pub struct ChoiceIndirection {
    pub id: ChoiceIndirectId,
    pub parent_context: ContextId,
    pub choices: Vec<Option<HirId>>,
    pub target_choice: ChoiceId,
}

impl ChoiceIndirection {
    pub fn children(&self) -> Vec<HirId> {
        Vec::new()
    }
}

pub fn choice_indirection(
    subs: &[Option<HirId>],
    ctx: &HirConversionCtx,
    id: ChoiceIndirectId,
    parent_context: ContextId,
    target_choice: ChoiceId,
) {
    let node = ChoiceIndirection {
        id,
        parent_context,
        choices: Vec::from(subs),
        target_choice,
    };
    ctx.insert(id.0, HirNode::ChoiceIndirection(node), vec![]);
}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct StructChoice {
    pub id: ChoiceId,
    pub parent_context: ContextId,
    pub subcontexts: Vec<ContextId>,
}

impl StructChoice {
    pub fn children(&self) -> Vec<HirId> {
        self.subcontexts.iter().map(|x| x.0).collect()
    }
}

fn struct_choice(
    ast: &ast::ParserChoice,
    ctx: &HirConversionCtx,
    id: ChoiceId,
    parents: &ParentInfo,
) -> VariableSet<Vec<Option<HirId>>> {
    let children = extract_non_choice(ast);
    let parents = ParentInfo {
        parent_choice: Some(id),
        ..parents.clone()
    };
    let mut subcontexts = Vec::new();
    let mut varset = None;
    let mut subvars = Vec::new();
    for (idx, child) in children.into_iter().enumerate() {
        let subcontext_id = ContextId(id.extend(ctx.db, PathComponent::Unnamed(idx as u32)));
        subcontexts.push(subcontext_id);
        let new_vars = struct_context(child, ctx, subcontext_id, &parents);
        varset = varset
            .map(|x: VariableSet<()>| x.merge_sum(&new_vars))
            .or(Some(new_vars.without_data()));
        subvars.push(new_vars);
    }
    let varset = varset.unwrap_or_default().map(|id, _| {
        subvars
            .iter()
            .map(|x| x.set.get(&id).map(|y| *y.inner()))
            .collect::<Vec<Option<HirId>>>()
    });

    let choice = StructChoice {
        id,
        parent_context: parents.parent_context.unwrap(),
        subcontexts,
    };
    ctx.insert(id.0, HirNode::Choice(choice), vec![ast.span]);
    varset
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
    pub vars: Box<VariableSet<HirId>>,
    pub children: Box<Vec<HirId>>,
}

impl StructCtx {
    pub fn children(&self) -> Vec<HirId> {
        self.children.as_ref().clone()
    }
}

fn empty_struct_context(
    ctx: &HirConversionCtx,
    id: ContextId,
    parents: &ParentInfo,
    span: Span,
) -> VariableSet<HirId> {
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
) -> VariableSet<HirId> {
    let children = match ast {
        ast::BlockContent::Sequence(x) => x.content.iter().collect(),
        otherwise => vec![otherwise],
    };
    let mut children_id = BTreeSet::new();
    let mut duplicate_ident = BTreeSet::<Identifier>::new();
    let old_parents = parents.clone();
    let parents = ParentInfo {
        parent_context: Some(id),
        ..old_parents
    };
    let mut index: u32 = 0;
    let mut varset = VariableSet::new();
    let mut pred = ParserPredecessor::ChildOf(id.0);
    for child in children {
        let mut new_id = |d: Option<Identifier>| {
            let sub_id = match d {
                Some(ident) => id.extend(ctx.db, PathComponent::Named(ident)),
                None => {
                    index = index
                        .checked_add(1)
                        .expect("Internal Compiler Error: overflowed variable counter");
                    id.extend(ctx.db, PathComponent::Unnamed(index - 1))
                }
            };
            children_id.insert(sub_id);
            sub_id
        };
        let new_set = match child {
            ast::BlockContent::Choice(c) => {
                let sub_id = ChoiceId(new_id(None));
                let choice_indirect = struct_choice(&c, ctx, sub_id, &parents);
                choice_indirect.map(|ident, b| {
                    let chin_id = ChoiceIndirectId(new_id(Some(ident)));
                    choice_indirection(b, ctx, chin_id, id, sub_id);
                    chin_id.0
                })
            }
            ast::BlockContent::Statement(x) => {
                let sub_id = new_id(x.id());
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
        duplicate_ident.extend(duplicate.iter());
        varset = result_set;
    }
    let context = StructCtx {
        id,
        block_id: old_parents.block_id,
        parent_choice: old_parents.parent_choice,
        parent_context: old_parents.parent_context,
        vars: Box::new(varset.clone()),
        children: Box::new(children_id.into_iter().collect()),
    };
    ctx.insert(id.0, HirNode::Context(context), vec![ast.span()]);
    varset
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct LetStatement {
    pub id: LetId,
    pub ty: TExprId,
    pub expr: ExprId,
    pub context: ContextId,
}

impl LetStatement {
    fn children(&self) -> Vec<HirId> {
        vec![self.ty.0, self.expr.0]
    }
}

fn let_statement(
    ast: &ast::LetStatement,
    ctx: &HirConversionCtx,
    id: LetId,
    context: ContextId,
) -> VariableSet<HirId> {
    let val_id = ExprId(id.extend(ctx.db, PathComponent::Unnamed(0)));
    let ty_id = TExprId(id.extend(ctx.db, PathComponent::Unnamed(1)));
    val_expression(&ast.expr, ctx, val_id, Some(context), None);
    type_expression(&ast.ty, ctx, ty_id);
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
    pub expr: ExprId,
}

impl ParseStatement {
    pub fn children(&self) -> Vec<HirId> {
        vec![self.expr.0]
    }
}

fn parse_statement(
    ast: &ast::ParseStatement,
    ctx: &HirConversionCtx,
    id: ParseId,
    prev: ParserPredecessor,
    parent_context: Option<ContextId>,
) -> VariableSet<HirId> {
    let expr = ExprId(id.extend(ctx.db, PathComponent::Unnamed(0)));
    val_expression(&ast.parser, ctx, expr, parent_context, None);
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
pub enum TypeAtom {
    Id(Identifier),
    Array(Box<TypeArray>),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ParserAtom {
    Atom(Atom),
    Array(ArrayId),
    Block(BlockId),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct TypeArray {
    pub direction: Spanned<ArrayKind>,
    pub expr: Expression<HirType>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserArray {
    pub id: ArrayId,
    pub direction: ArrayKind,
    pub expr: ExprId,
}

impl ParserArray {
    pub fn children(&self) -> Vec<HirId> {
        vec![self.expr.0]
    }
}

fn parser_array(
    ast: &ast::ParserArray,
    ctx: &HirConversionCtx,
    id: ArrayId,
    parent_context: Option<ContextId>,
) {
    let expr = ExprId(id.extend(ctx.db, PathComponent::Unnamed(0)));
    val_expression(
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
pub struct VariableSet<T: Clone + Hash + Eq + Debug> {
    set: BTreeMap<Identifier, VarStatus<T>>,
}

impl<T: Clone + Hash + Eq + Debug> VariableSet<T> {
    pub fn new() -> Self {
        VariableSet {
            set: BTreeMap::new(),
        }
    }
    pub fn singular(id: Identifier, data: T) -> Self {
        let mut set = BTreeMap::new();
        set.insert(id, VarStatus::Always(data));
        VariableSet { set }
    }
    pub fn merge_product(&self, other: &Self) -> (Self, Vec<Identifier>) {
        let mut set = self.set.clone();
        let mut doubled = Vec::new();
        for (k, v) in other.set.iter() {
            match set.entry(*k) {
                Entry::Vacant(entry) => {
                    entry.insert(v.clone());
                }
                Entry::Occupied(_) => doubled.push(*k),
            }
        }
        (VariableSet { set }, doubled)
    }
    pub fn without_data(&self) -> VariableSet<()> {
        let mut set = BTreeMap::new();
        for (k, v) in self.set.iter() {
            set.insert(*k, v.without_data());
        }
        VariableSet { set }
    }
    pub fn map<S: Clone + Eq + Debug + Hash>(
        &self,
        mut f: impl FnMut(Identifier, &T) -> S,
    ) -> VariableSet<S> {
        let mut set = BTreeMap::new();
        for (k, v) in self.set.iter() {
            set.insert(*k, v.map(|x| f(*k, x)));
        }
        VariableSet { set }
    }
}

impl VariableSet<()> {
    pub fn merge_sum<T: Hash + Eq + Clone + Debug>(&self, other: &VariableSet<T>) -> Self {
        let mut new = BTreeMap::new();
        new.extend(self.set.iter().map(|(k, v)| {
            (*k, {
                VarStatus::<()>::from_accessibility(v.is_accessible() && other.set.contains_key(k))
            })
        }));
        for (k, v) in other.set.iter() {
            let other_entry = v.is_accessible();
            new.entry(*k)
                .and_modify(|e| {
                    *e = VarStatus::<()>::from_accessibility(e.is_accessible() && other_entry)
                })
                .or_insert(VarStatus::Maybe(()));
        }
        VariableSet { set: new }
    }
}

impl<T: Copy + Hash + Eq + Debug> Default for VariableSet<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum VarStatus<T: Clone + Eq + Debug + Hash> {
    Always(T),
    Maybe(T),
}

impl<T: Clone + Eq + Debug + Hash> VarStatus<T> {
    fn is_accessible(&self) -> bool {
        matches!(self, Self::Always(_))
    }
    fn without_data(&self) -> VarStatus<()> {
        match self {
            VarStatus::Always(_) => VarStatus::Always(()),
            VarStatus::Maybe(_) => VarStatus::Maybe(()),
        }
    }
    fn map<S: Clone + Eq + Debug + Hash>(&self, f: impl FnOnce(&T) -> S) -> VarStatus<S> {
        match self {
            VarStatus::Always(a) => VarStatus::Always(f(a)),
            VarStatus::Maybe(a) => VarStatus::Always(f(a)),
        }
    }
    fn from_accessibility(access: bool) -> VarStatus<()> {
        if access {
            VarStatus::Always(())
        } else {
            VarStatus::Maybe(())
        }
    }
    fn inner(&self) -> &T {
        match self {
            VarStatus::Always(a) | VarStatus::Maybe(a) => a,
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
#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::Context;
    use crate::interner::Interner;
    #[test]
    fn eval_test() {
        let ctx = Context::mock(
            r#"
def for [u8] *> expr1: {
    (
        a: u64,
        b: u32,
    |
        a: u64,
    )
}
        "#,
        );
        ctx.db.all_hir_ids();
    }
    //    #[test]
    //    fn recursion_ssc() {
    //        let ctx = Context::mock(
    //            r#"
    //def a: for [u8] *> {x: c, y: {b, z: d,},}
    //def b: for [u8] *> {x: a, y: c,}
    //def c: for [u8] *> {x: c,}
    //def d: for [u8] *> {let a: u64 = 1, let b: u64 = a + 1,}
    //def e: for [u8] *> {}
    //            "#,
    //        );
    //        let fd = FileId::default();
    //        let var = |s| ParserDefId(ctx.db.intern_hir_path(HirPath::new_fid(fd, ctx.id(s))));
    //        let a = var("a");
    //        let b = var("b");
    //        let c = var("c");
    //        let d = var("d");
    //        let e = var("d");
    //        let get_ssc = |x| ctx.db.parser_ssc(x).unwrap();
    //        let ssc_a = get_ssc(a);
    //        let ssc_b = get_ssc(b);
    //        let ssc_c = get_ssc(c);
    //        let ssc_d = get_ssc(d);
    //        let ssc_e = get_ssc(e);
    //        assert!(ssc_a == ssc_b);
    //        assert!(ssc_b != ssc_c);
    //        assert!(ssc_c != ssc_d);
    //        assert!(ssc_b != ssc_d);
    //        assert!(ssc_b != ssc_e);
    //    }
}
