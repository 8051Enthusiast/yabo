mod recursion;
pub mod refs;
pub mod represent;
pub mod walk;

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
    expr::{self, Atom, ExprConverter, Expression, ExpressionComponent, ExpressionKind},
    interner::{HirId, HirPath, Identifier, PathComponent},
    source::{FileId, Span, Spanned},
};

use recursion::{mod_parser_ssc, parser_ssc, FunctionSscId};

#[salsa::query_group(HirDatabase)]
pub trait Hirs: ast::Asts {
    fn hir_parser_collection(&self, hid: HirId) -> Result<Option<HirParserCollection>, ()>;
    fn hir_node(&self, id: HirId) -> Result<HirNode, ()>;
    #[salsa::interned]
    fn intern_recursion_scc(&self, functions: Vec<ParserDefId>) -> recursion::FunctionSscId;
    fn mod_parser_ssc(
        &self,
        module: ModuleId,
    ) -> Result<Arc<BTreeMap<ParserDefId, FunctionSscId>>, ()>;
    fn parser_ssc(&self, parser: ParserDefId) -> Result<FunctionSscId, ()>;
    fn root_id(&self) -> ModuleId;
    fn all_hir_ids(&self) -> Vec<HirId>;
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

#[derive(Clone, Hash, PartialEq, Eq, Debug, Default)]
pub struct HirParserCollection {
    map: BTreeMap<HirId, HirNode>,
    spans: BTreeMap<HirId, Vec<Span>>,
}

impl HirParserCollection {
    pub fn new() -> Self {
        Self::default()
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

impl<K: ExpressionKind, T: ExpressionComponent<K>> ExpressionComponent<K> for IndexSpanned<T> {
    fn children(&self) -> Vec<&Expression<K>> {
        self.atom.children()
    }
}

impl ExpressionComponent<HirVal> for ParserAtom {
    fn children(&self) -> Vec<&Expression<HirVal>> {
        vec![]
    }
}

impl ExpressionComponent<HirType> for TypeAtom {
    fn children(&self) -> Vec<&Expression<HirType>> {
        match self {
            TypeAtom::Id(_) => vec![],
            TypeAtom::Array(a) => vec![&a.expr],
        }
    }
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

fn constraint_expression_converter(
    add_span: &impl Fn(&Span) -> SpanIndex,
) -> ExprConverter<AstConstraint, HirConstraint> {
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

fn type_expression_converter(
    add_span: &impl Fn(&Span) -> SpanIndex,
) -> ExprConverter<AstType, HirType> {
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
    pub choices: Vec<(u32, HirId)>,
    pub target_choice: ChoiceId,
}

impl ChoiceIndirection {
    pub fn children(&self) -> Vec<HirId> {
        Vec::new()
    }
}

pub fn choice_indirection(
    subs: &[(u32, HirId)],
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
) -> VariableSet<Vec<(u32, HirId)>> {
    let children = extract_non_choice(ast);
    let parents = ParentInfo {
        parent_choice: Some(id),
        ..*parents
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
            .or_else(|| Some(new_vars.without_data()));
        subvars.push(new_vars);
    }
    let varset = varset.unwrap_or_default().map(|id, _| {
        subvars
            .iter()
            .enumerate()
            .flat_map(|(i, x)| x.set.get(&id).map(|y| (i as u32, *y.inner())))
            .collect::<Vec<(u32, HirId)>>()
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
    let old_parents = *parents;
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
                let choice_indirect = struct_choice(c, ctx, sub_id, &parents);
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
                        let set = parse_statement(p, ctx, ParseId(sub_id), pred, id);
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
    pub parent_context: ContextId,
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
    parent_context: ContextId,
) -> VariableSet<HirId> {
    let expr = ExprId(id.extend(ctx.db, PathComponent::Unnamed(0)));
    val_expression(&ast.parser, ctx, expr, Some(parent_context), None);
    let pt = ParseStatement {
        id,
        prev,
        expr,
        parent_context,
    };
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
    pub context: Option<ContextId>,
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
        Some(ast.direction.inner),
    );
    let pa = ParserArray {
        id,
        direction: ast.direction.inner,
        expr,
        context: parent_context,
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
    pub fn get(&self, idx: Identifier) -> Option<&VarStatus<T>> {
        self.set.get(&idx)
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
        eprintln!("{:?}", ctx.db.all_hir_ids());
    }
    #[test]
    fn recursion_ssc() {
        let ctx = Context::mock(
            r#"
def for [u8] *> a: {x: c, y: {b, z: d,},}
def for [u8] *> b: {x: a, y: c,}
def for [u8] *> c: {x: c,}
def for [u8] *> d: {let a: u64 = 1, let b: u64 = a + 1,}
def for [u8] *> e: {}
            "#,
        );
        let fd = FileId::default();
        let var = |s| ParserDefId(ctx.db.intern_hir_path(HirPath::new_fid(fd, ctx.id(s))));
        let a = var("a");
        let b = var("b");
        let c = var("c");
        let d = var("d");
        let e = var("d");
        let get_ssc = |x| ctx.db.parser_ssc(x).unwrap();
        let ssc_a = get_ssc(a);
        let ssc_b = get_ssc(b);
        let ssc_c = get_ssc(c);
        let ssc_d = get_ssc(d);
        let ssc_e = get_ssc(e);
        assert!(ssc_a == ssc_b);
        assert!(ssc_b != ssc_c);
        assert!(ssc_c != ssc_d);
        assert!(ssc_b != ssc_d);
        assert!(ssc_b != ssc_e);
    }
}
