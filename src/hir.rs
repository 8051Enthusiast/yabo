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

struct HirConversionCtx<'a> {
    collection: RefCell<HirParserCollection>,
    db: &'a dyn Hirs,
}

impl<'a> HirConversionCtx<'a> {
    fn new(collection: HirParserCollection, db: &'a dyn Hirs) -> Self {
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
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirConstraint {}

impl ExpressionKind for HirConstraint {
    type BinaryOp = expr::ConstraintBinOp<HirConstraint, SpanIndex>;
    type UnaryOp = expr::ConstraintUnOp<HirConstraint, SpanIndex>;
    type Atom = (Atom, SpanIndex);
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirParse {}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ParseExpression {
    id: PExprId,
    expr: Expression<HirParse>,
}

impl ExpressionKind for HirParse {
    type BinaryOp = expr::ParseBinOp<HirParse, HirConstraint, SpanIndex>;
    type UnaryOp = expr::ParseUnOp<HirParse, SpanIndex>;
    type Atom = (ParserAtom, SpanIndex);
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirVal {}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ValExpression {
    id: ExprId,
    expr: Expression<HirVal>,
}

impl ExpressionKind for HirVal {
    type BinaryOp = expr::ValBinOp<HirVal, HirParse, SpanIndex>;
    type UnaryOp = expr::ValUnOp<HirVal, SpanIndex>;
    type Atom = (Atom, SpanIndex);
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
        (atom.inner.clone(), n)
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
    let atom_fun = move |x: &Spanned<ast::ParserAtom>| -> (ParserAtom, SpanIndex) {
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
        (atom, n)
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
    let atom_id = RefCell::new(0u32);
    let pexpr = {
        let add_span = |span: &Span| {
            let mut borrow = spans.borrow_mut();
            borrow.push(*span);
            SpanIndex(u32::try_from(borrow.len()).unwrap() - 1)
        };
        let new_id = || {
            let mut borrow = atom_id.borrow_mut();
            let old_value: u32 = *borrow;
            *borrow += 1;
            id.extend(ctx.db, PathComponent::Unnamed(old_value))
        };
        let pconverter =
            parse_expression_converter(ctx, &add_span, &new_id, parent_context, array);
        ParseExpression {
            id,
            expr: pconverter.convert(ast),
        }
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
        (atom.inner.clone(), n)
    };
    ExprConverter::new(bin_fun, un_fun, atom_fun)
}

fn val_expression(ast: &ast::ValExpression, ctx: &HirConversionCtx, id: ExprId) {
    let spans = RefCell::new(Vec::new());
    let atom_id = RefCell::new(0u32);
    let pexpr = {
        let add_span = |span: &Span| {
            let mut borrow = spans.borrow_mut();
            borrow.push(*span);
            SpanIndex(u32::try_from(borrow.len()).unwrap() - 1)
        };
        let new_id = || {
            let mut borrow = atom_id.borrow_mut();
            let old_value: u32 = *borrow;
            *borrow += 1;
            id.extend(ctx.db, PathComponent::Unnamed(old_value))
        };
        let vconverter = val_expression_converter(ctx, &add_span, &new_id);
        ValExpression {
            id,
            expr: vconverter.convert(ast),
        }
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
    let pdef = ParserDef {
        id,
        from,
        to,
    };
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
        varset = varset.map(|x: VariableSet| x.merge_sum(&new_vars, id)).or(Some(new_vars));
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
                struct_choice(c, ctx, sub_id, &parents)
            }
            ast::BlockContent::Statement(x) => {
                let name = x.id().map(PathComponent::Named).unwrap_or_else(next_index);
                let sub_id = id.extend(ctx.db, name);
                match x.as_ref() {
                    ast::Statement::ParserDef(_) => {
                        panic!("Nested parser definitions are not yet implemented")
                    }
                    ast::Statement::Parse(p) => {
                        let set = parse_statement(
                            p,
                            ctx,
                            ParseId(sub_id),
                            pred,
                            parents.parent_context,
                        );
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
    };
    ctx.insert(id.0, HirNode::Context(context), vec![ast.span()]);
    varset
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct LetStatement {
    pub id: LetId,
    pub ty: Option<PExprId>,
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
        ty: Some(ty_id),
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
        eprintln!("{:?} {:?}", self.set, other.set);
        let mut new = BTreeMap::new();
        new.extend(self.set.iter().map(|(k, v)| {
            (
                *k,
                {
                    VarStatus::from_accessibility(v.is_accessible() && other.set.contains_key(k), id)
                }
            )
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
        b: u64;
    |
        a: u64;
    )
}
        "#,
        );
        let main = FileId::default();
        let expr1 = ctx.id("complex");
        let collection = ctx.db.hir_parser_collection(main, expr1);
        println!("{:#?}", collection);
    }
}

