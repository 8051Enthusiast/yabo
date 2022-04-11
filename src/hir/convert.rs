use std::collections::HashMap;

use super::*;
use crate::{
    ast::{self, AstConstraint, AstType, AstVal},
    error::Silencable,
    error_type,
};

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum HirConversionError {
    DuplicateField {
        first: Span,
        duplicate: Span,
        name: FieldName,
    },
    Silenced,
}

impl From<SilencedError> for HirConversionError {
    fn from(_: SilencedError) -> Self {
        HirConversionError::Silenced
    }
}

impl Silencable for HirConversionError {
    type Out = SilencedError;
    fn silence(self) -> Self::Out {
        SilencedError
    }
}

error_type!(HirConversionErrors(BTreeSet<HirConversionError>) in crate::hir);

fn constraint_expression_converter(
    add_span: &impl Fn(&Span) -> SpanIndex,
) -> ExprConverter<AstConstraint, HirConstraint> {
    type Converter<'b> = ExprConverter<'b, ast::AstConstraint, HirConstraint>;
    let bin_fun =
        move |bop: &ast::AstConstraintBinOp, c: &Converter| bop.convert_same(c, &add_span);
    let un_fun = move |uop: &ast::AstConstraintUnOp, c: &Converter| uop.convert_same(c, &add_span);
    let atom_fun = move |atom: &Spanned<Atom>, _: &Converter| IndexSpanned::new(atom, add_span);
    ExprConverter::new(bin_fun, un_fun, atom_fun)
}

fn val_expression_converter<'a>(
    ctx: &'a HirConversionCtx<'a>,
    add_span: &'a impl Fn(&Span) -> SpanIndex,
    new_id: &'a impl Fn() -> HirId,
    parent_context: Option<ContextId>,
    enclosing_expr: ExprId,
    array: Option<ArrayKind>,
) -> ExprConverter<'a, AstVal, HirVal> {
    type VConverter<'b> = ExprConverter<'b, ast::AstVal, HirVal>;
    let atom_fun =
        move |x: &Spanned<ast::ParserAtom>, _: &VConverter| -> IndexSpanned<ParserAtom> {
            let n = add_span(&x.span);
            let atom = match &x.inner {
                ast::ParserAtom::Array(array) => {
                    let nid = ArrayId(new_id());
                    parser_array(array, ctx, nid, parent_context, enclosing_expr);
                    ParserAtom::Array(nid)
                }
                ast::ParserAtom::Single => ParserAtom::Single,
                ast::ParserAtom::Atom(atom) => ParserAtom::Atom(atom.clone()),
                ast::ParserAtom::Block(b) => {
                    let nid = BlockId(new_id());
                    block(b, ctx, nid, parent_context, enclosing_expr, array);
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
            ast::TypeAtom::ParserDef(pd) => {
                let from = pd.from.as_ref().map(|x| c.convert(x));
                let args = pd.args.iter().map(|x| c.convert(x)).collect();
                let name = IndexSpanned::new(&pd.name, add_span);
                TypeAtom::ParserDef(Box::new(ParserDefRef { from, name, args }))
            }
            ast::TypeAtom::Array(arr) => {
                let new_expr = c.convert(&arr.expr);
                TypeAtom::Array(Box::new(TypeArray {
                    direction: arr.direction.inner,
                    expr: new_expr,
                }))
            }
            ast::TypeAtom::Primitive(a) => TypeAtom::Primitive((*a).into()),
            ast::TypeAtom::TypeVar(v) => TypeAtom::TypeVar(*v),
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
    let add_span = SpanIndex::add_span(&spans);
    let new_id = || {
        let mut borrow = children.borrow_mut();
        let index: u32 = u32::try_from(borrow.len()).unwrap();
        let new_id = id.child(ctx.db, PathComponent::Unnamed(index));
        borrow.push(new_id);
        new_id
    };
    let vconverter = val_expression_converter(ctx, &add_span, &new_id, parent_context, id, array);
    let expr = vconverter.convert(ast);
    drop(vconverter);
    drop(add_span);
    let expr = ValExpression {
        id,
        expr,
        children: children.into_inner(),
    };
    ctx.insert(id.0, HirNode::Expr(expr), spans.into_inner())
}

fn type_expression(ast: &ast::TypeExpression, ctx: &HirConversionCtx, id: TExprId) {
    let spans = RefCell::new(Vec::new());
    let add_span = SpanIndex::add_span(&spans);
    let tconverter = type_expression_converter(&add_span);
    let texpr = tconverter.convert(ast);
    drop(tconverter);
    drop(add_span);
    let texpr = TypeExpression { id, expr: texpr };
    ctx.insert(id.0, HirNode::TExpr(texpr), spans.into_inner())
}

fn parser_def(ast: &ast::ParserDefinition, ctx: &HirConversionCtx, id: ParserDefId) {
    let from = TExprId(id.child(ctx.db, PathComponent::Unnamed(0)));
    let to = ExprId(id.child(ctx.db, PathComponent::Unnamed(1)));
    type_expression(&ast.from, ctx, from);
    val_expression(&ast.to, ctx, to, None, None);
    let pdef = ParserDef { id, from, to };
    ctx.insert(id.0, HirNode::ParserDef(pdef), vec![ast.span]);
}

fn block(
    ast: &ast::Block,
    ctx: &HirConversionCtx,
    id: BlockId,
    super_context: Option<ContextId>,
    enclosing_expr: ExprId,
    array: Option<ArrayKind>,
) {
    let context_id = ContextId(id.child(ctx.db, PathComponent::Unnamed(0)));
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
        enclosing_expr,
        array,
    };
    ctx.insert(id.0, HirNode::Block(block), vec![ast.span]);
}

fn struct_choice(
    ast: &ast::ParserChoice,
    ctx: &HirConversionCtx,
    id: ChoiceId,
    parents: &ParentInfo,
) -> VariableSet<Vec<(u32, HirId, Span)>> {
    let children = extract_non_choice(ast);
    let parents = ParentInfo {
        parent_choice: Some(id),
        ..*parents
    };
    let mut subcontexts = Vec::new();
    let mut varset = None;
    let mut subvars = Vec::new();
    for (idx, child) in children.into_iter().enumerate() {
        let subcontext_id = ContextId(id.child(ctx.db, PathComponent::Unnamed(idx as u32)));
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
            .flat_map(|(i, x)| x.set.get(&id).map(|y| (i as u32, y.inner().0, y.inner().1)))
            .collect::<Vec<(u32, HirId, Span)>>()
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
) -> VariableSet<(HirId, Span)> {
    let children = match ast {
        ast::BlockContent::Sequence(x) => x.content.iter().collect(),
        otherwise => vec![otherwise],
    };
    let mut children_id = BTreeSet::new();
    let mut duplicate_field = HashMap::<FieldName, HirConversionError>::new();
    let old_parents = *parents;
    let parents = ParentInfo {
        parent_context: Some(id),
        ..old_parents
    };
    let mut index: u32 = 0;
    let mut varset = VariableSet::new();
    let mut pred = ParserPredecessor::ChildOf(id.0);
    for child in children {
        let mut new_id = |d: Option<FieldName>| {
            let sub_id = match d {
                Some(field) => id.child(ctx.db, PathComponent::Named(field)),
                None => {
                    index = index
                        .checked_add(1)
                        .expect("Internal Compiler Error: overflowed variable counter");
                    id.child(ctx.db, PathComponent::Unnamed(index - 1))
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
                    (chin_id.0, b[0].2)
                })
            }
            ast::BlockContent::Statement(x) => {
                let sub_id = new_id(x.field());
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
        duplicate_field.extend(duplicate.into_iter().map(|(name, first, duplicate)| {
            (
                name,
                HirConversionError::DuplicateField {
                    name,
                    first: first.inner().1,
                    duplicate: duplicate.inner().1,
                },
            )
        }));
        ctx.add_errors(duplicate_field.values().cloned());
        varset = result_set;
    }
    let context = StructCtx {
        id,
        block_id: old_parents.block_id,
        parent_choice: old_parents.parent_choice,
        parent_context: old_parents.parent_context,
        vars: Box::new(varset.map(|_, (id, _)| *id)),
        children: Box::new(children_id.into_iter().collect()),
    };
    ctx.insert(id.0, HirNode::Context(context), vec![ast.span()]);
    varset
}

fn let_statement(
    ast: &ast::LetStatement,
    ctx: &HirConversionCtx,
    id: LetId,
    context: ContextId,
) -> VariableSet<(HirId, Span)> {
    let val_id = ExprId(id.child(ctx.db, PathComponent::Unnamed(0)));
    let ty_id = TExprId(id.child(ctx.db, PathComponent::Unnamed(1)));
    val_expression(&ast.expr, ctx, val_id, Some(context), None);
    type_expression(&ast.ty, ctx, ty_id);
    let st = LetStatement {
        id,
        ty: ty_id,
        expr: val_id,
        context,
    };
    ctx.insert(id.0, HirNode::Let(st), vec![ast.span, ast.name.span]);
    VariableSet::singular(ast.name.id, (id.0, ast.name.span))
}

fn parse_statement(
    ast: &ast::ParseStatement,
    ctx: &HirConversionCtx,
    id: ParseId,
    prev: ParserPredecessor,
    parent_context: ContextId,
) -> VariableSet<(HirId, Span)> {
    let expr = ExprId(id.child(ctx.db, PathComponent::Unnamed(0)));
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
        .map(|name| VariableSet::singular(name.id, (id.0, name.span)))
        .unwrap_or_default()
}

fn parser_array(
    ast: &ast::ParserArray,
    ctx: &HirConversionCtx,
    id: ArrayId,
    parent_context: Option<ContextId>,
    enclosing_expr: ExprId,
) {
    let expr = ExprId(id.child(ctx.db, PathComponent::Unnamed(0)));
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
        enclosing_expr,
    };
    ctx.insert(id.0, HirNode::Array(pa), vec![ast.span, ast.direction.span]);
}

pub fn choice_indirection(
    subs: &[(u32, HirId, Span)],
    ctx: &HirConversionCtx,
    id: ChoiceIndirectId,
    parent_context: ContextId,
    target_choice: ChoiceId,
) {
    let node = ChoiceIndirection {
        id,
        parent_context,
        choices: subs.iter().map(|(a, b, _)| (*a, *b)).collect(),
        target_choice,
    };
    ctx.insert(id.0, HirNode::ChoiceIndirection(node), vec![]);
}

impl From<ast::TypePrimitive> for TypePrimitive {
    fn from(p: ast::TypePrimitive) -> Self {
        match p {
            ast::TypePrimitive::Mem => TypePrimitive::Mem,
            ast::TypePrimitive::Int => TypePrimitive::Int,
            ast::TypePrimitive::Bit => TypePrimitive::Bit,
            ast::TypePrimitive::Char => TypePrimitive::Char,
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

    fn add_errors(&self, errors: impl IntoIterator<Item = HirConversionError>) {
        let mut borrow = self.collection.borrow_mut();
        for error in errors {
            borrow.errors.inner.insert(error);
        }
    }
}

pub fn hir_parser_collection(
    db: &dyn Hirs,
    hid: HirId,
) -> Result<Option<HirParserCollection>, SilencedError> {
    let collection = HirParserCollection::new();
    let ctx = HirConversionCtx::new(collection, db);
    let path = db.lookup_intern_hir_path(hid);
    let file = path.path()[0].unwrap_file();
    let id = path.path()[1].unwrap_ident();
    let parser = match db.top_level_statement(file, id)? {
        None => return Ok(None),
        Some(x) => x,
    };
    parser_def(&parser, &ctx, ParserDefId(hid));
    Ok(Some(ctx.collection.into_inner()))
}
