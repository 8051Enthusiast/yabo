use std::collections::HashMap;

use fxhash::FxHashSet;

use super::*;
use crate::{ast, error::Silencable, error_type, expr::OpWithData};

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum HirConversionError {
    DuplicateField {
        first: Span,
        duplicate: Span,
        name: FieldName,
    },
    DuplicateArg {
        name: Identifier,
        place: Span,
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
        SilencedError::new()
    }
}

error_type!(HirConversionErrors(BTreeSet<HirConversionError>) in crate::hir);

fn val_expression(
    ast: &ast::ValExpression,
    ctx: &HirConversionCtx,
    id: ExprId,
    parent_context: Option<ContextId>,
) {
    let spans = RefCell::new(Vec::new());
    let mut children = Vec::new();
    let mut add_span = SpanIndex::add_span(&spans);
    let mut new_id = || {
        let index: u32 = u32::try_from(children.len()).unwrap();
        let new_id = id.child(ctx.db, PathComponent::Unnamed(index));
        children.push(new_id);
        new_id
    };
    let expr: Expression<HirValSpanned> = ast.map(&mut add_span).convert(
        &mut |niladic| {
            let inner = match &niladic.inner {
                ast::ParserAtom::Atom(atom) => ParserAtom::Atom(atom.clone()),
                ast::ParserAtom::Single => ParserAtom::Single,
                ast::ParserAtom::Nil => ParserAtom::Nil,
                ast::ParserAtom::Block(b) => {
                    let nid = BlockId(new_id());
                    block(b, ctx, nid, parent_context, id);
                    ParserAtom::Block(nid)
                }
            };
            expr::OpWithData {
                data: niladic.data,
                inner,
            }
        },
        &mut |monadic, _| OpWithData {
            data: monadic.data,
            inner: monadic.inner.map_expr(|x| Arc::new(x.map(&mut add_span))),
        },
        &mut |dyadic, _, _| dyadic.clone(),
        &mut |variadic, _| variadic.clone(),
    );
    let expr = ValExpression {
        id,
        expr,
        children,
        parent_context,
    };
    drop(add_span);
    ctx.insert(id.0, HirNode::Expr(expr), spans.into_inner());
}

fn convert_type_expression(
    expr: &ast::TypeExpression,
    mut add_span: &impl Fn(&Span) -> SpanIndex,
) -> Expression<HirTypeSpanned> {
    let expr = expr.map(add_span);
    expr.convert_no_var(
        &mut |niladic| {
            let new_atom = match &niladic.inner {
                ast::TypeAtom::ParserDef(pd) => {
                    let from = pd
                        .from
                        .as_ref()
                        .map(|x| convert_type_expression(x, add_span));
                    let args = pd
                        .args
                        .iter()
                        .map(|x| convert_type_expression(x, add_span))
                        .collect();
                    let name = IndexSpanned {
                        span: add_span(&pd.name.span),
                        atom: pd.name.inner,
                    };
                    TypeAtom::ParserDef(Box::new(ParserDefRef { from, name, args }))
                }
                ast::TypeAtom::Array(arr) => {
                    let new_expr = convert_type_expression(&arr.expr, add_span);
                    TypeAtom::Array(Box::new(TypeArray {
                        direction: arr.direction.inner,
                        expr: new_expr,
                    }))
                }
                ast::TypeAtom::Primitive(a) => TypeAtom::Primitive((*a).into()),
                ast::TypeAtom::TypeVar(v) => TypeAtom::TypeVar(*v),
            };
            OpWithData {
                data: niladic.data,
                inner: new_atom,
            }
        },
        &mut |monadic, _| OpWithData {
            data: monadic.data,
            inner: monadic.inner.map_expr(|x| Arc::new(x.map(&mut add_span))),
        },
        &mut |dyadic, _, _| dyadic.clone(),
    )
}

fn type_expression(ast: &ast::TypeExpression, ctx: &HirConversionCtx, id: TExprId) {
    let spans = RefCell::new(Vec::new());
    let mut add_span = SpanIndex::add_span(&spans);
    let texpr = convert_type_expression(ast, &mut add_span);
    let texpr = TypeExpression { id, expr: texpr };
    drop(add_span);
    ctx.insert(id.0, HirNode::TExpr(texpr), spans.into_inner())
}

fn arg_def(ast: &ast::ArgDefinition, ctx: &HirConversionCtx, id: ArgDefId) {
    let ty = TExprId(id.child(ctx.db, PathComponent::Unnamed(0)));
    type_expression(&ast.ty, ctx, ty);
    let argdef = ArgDef {
        id,
        name: ast.name.inner,
        ty,
    };
    ctx.insert(id.0, HirNode::ArgDef(argdef), vec![ast.name.span]);
}

fn parser_def(ast: &ast::ParserDefinition, ctx: &HirConversionCtx, id: ParserDefId) {
    let from = TExprId(id.child(ctx.db, PathComponent::Unnamed(0)));
    let to = ExprId(id.child(ctx.db, PathComponent::Unnamed(1)));
    type_expression(&ast.from, ctx, from);
    val_expression(&ast.to, ctx, to, None);
    let qualifier = match ast.qualifier {
        Some(ast::Qualifier::Export) => Qualifier::Export,
        None => Qualifier::Regular,
    };
    let mut names = FxHashSet::default();
    let args = ast.argdefs.as_ref().map(|x| {
        let mut args = Vec::new();
        for arg in x.args.iter() {
            if !names.insert(arg.name.inner) {
                ctx.add_errors(Some(HirConversionError::DuplicateArg {
                    name: arg.name.inner,
                    place: arg.name.span,
                }));
                continue;
            };
            let arg_id = ArgDefId(id.child(
                ctx.db,
                PathComponent::Named(FieldName::Ident(arg.name.inner)),
            ));
            arg_def(arg, ctx, arg_id);
            args.push(arg_id);
        }
        args
    });
    let pdef = ParserDef {
        qualifier,
        id,
        from,
        args,
        to,
    };
    ctx.insert(
        id.0,
        HirNode::ParserDef(pdef),
        vec![ast.name.span, ast.span],
    );
}

fn block(
    ast: &ast::Block,
    ctx: &HirConversionCtx,
    id: BlockId,
    super_context: Option<ContextId>,
    enclosing_expr: ExprId,
) {
    let context_id = ContextId(id.child(ctx.db, PathComponent::Unnamed(0)));
    let parents = ParentInfo {
        parent_choice: None,
        parent_context: None,
        block_id: id,
    };
    let returns = match &ast.content {
        Some(c) => {
            let vars = struct_context(c, ctx, context_id, &parents);
            vars.get(FieldName::Return).is_some()
        }
        None => {
            empty_struct_context(ctx, context_id, &parents, ast.span);
            false
        }
    };
    let block = Block {
        id,
        root_context: context_id,
        super_context,
        enclosing_expr,
        returns,
    };
    ctx.insert(id.0, HirNode::Block(block), vec![ast.span]);
}

fn struct_choice(
    ast: &ast::ParserChoice,
    ctx: &HirConversionCtx,
    id: ChoiceId,
    pred: ParserPredecessor,
    parents: &ParentInfo,
) -> VariableSet<Vec<(u32, DefId, Span)>> {
    let children = &ast.content;
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
        let new_vars = struct_context(&child, ctx, subcontext_id, &parents);
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
            .collect::<Vec<(u32, DefId, Span)>>()
    });

    let choice = StructChoice {
        id,
        parent_context: parents.parent_context.unwrap(),
        front: pred,
        back: ParserPredecessor::ChildOf(parents.parent_context.unwrap()),
        subcontexts,
    };
    ctx.insert(id.0, HirNode::Choice(choice), vec![ast.span]);
    varset
}

fn empty_struct_context(
    ctx: &HirConversionCtx,
    id: ContextId,
    parents: &ParentInfo,
    span: Span,
) -> VariableSet<DefId> {
    let varset = VariableSet::new();
    let context = StructCtx {
        id,
        block_id: parents.block_id,
        parent_choice: parents.parent_choice,
        parent_context: parents.parent_context,
        vars: Box::new(varset.clone()),
        children: Default::default(),
        endpoints: None,
    };
    ctx.insert(id.0, HirNode::Context(context), vec![span]);
    varset
}

fn struct_context(
    ast: &ast::ParserSequence,
    ctx: &HirConversionCtx,
    id: ContextId,
    parents: &ParentInfo,
) -> VariableSet<(DefId, Span)> {
    let children = &ast.content;
    let mut children_id = BTreeSet::new();
    let mut duplicate_field = HashMap::<FieldName, HirConversionError>::new();
    let old_parents = *parents;
    let parents = ParentInfo {
        parent_context: Some(id),
        ..old_parents
    };
    let mut index: u32 = 0;
    let mut varset = VariableSet::new();
    let mut pred = ParserPredecessor::ChildOf(id);
    let mut endpoints = None;
    let mut update_pred = |new| {
        let old = pred;
        let new_pred = ParserPredecessor::After(new);
        endpoints = Some(
            endpoints
                .map(|(front, _)| (front, new))
                .unwrap_or((new, new)),
        );
        match pred {
            ParserPredecessor::After(id) => ctx.modify_back_predecessor(id, new_pred),
            ParserPredecessor::ChildOf(_) => (),
        }
        pred = new_pred;
        old
    };
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
    for child in children {
        let new_set = match child {
            ast::ParserSequenceElement::Choice(c) => {
                let sub_id = ChoiceId(new_id(None));
                let choice_indirect =
                    struct_choice(c, ctx, sub_id, update_pred(sub_id.0), &parents);
                choice_indirect.map(|ident, b| {
                    let chin_id = ChoiceIndirectId(new_id(Some(ident)));
                    choice_indirection(b, ctx, chin_id, id, sub_id);
                    (chin_id.0, b[0].2)
                })
            }
            ast::ParserSequenceElement::Statement(x) => {
                let sub_id = new_id(x.field());
                match x.as_ref() {
                    ast::Statement::Parse(p) => {
                        let set = parse_statement(p, ctx, ParseId(sub_id), update_pred(sub_id), id);
                        set
                    }
                    ast::Statement::Let(l) => let_statement(l, ctx, LetId(sub_id), id),
                }
            }
        };
        let (result_set, duplicate) = varset.merge_product(&new_set);
        duplicate_field.extend(duplicate.into_iter().map(|(name, duplicate, first)| {
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
        endpoints,
    };
    ctx.insert(id.0, HirNode::Context(context), vec![ast.span]);
    varset
}

fn let_statement(
    ast: &ast::LetStatement,
    ctx: &HirConversionCtx,
    id: LetId,
    context: ContextId,
) -> VariableSet<(DefId, Span)> {
    let val_id = ExprId(id.child(ctx.db, PathComponent::Unnamed(0)));
    let ty = if let Some(ty) = &ast.ty {
        let ty_id = TExprId(id.child(ctx.db, PathComponent::Unnamed(1)));
        type_expression(&ty, ctx, ty_id);
        Some(ty_id)
    } else {
        None
    };
    val_expression(&ast.expr, ctx, val_id, Some(context));
    let st = LetStatement {
        id,
        ty,
        expr: val_id,
        context,
    };
    ctx.insert(id.0, HirNode::Let(st), vec![ast.span, ast.name.span]);
    VariableSet::singular(ast.name.inner, (id.0, ast.name.span))
}

fn parse_statement(
    ast: &ast::ParseStatement,
    ctx: &HirConversionCtx,
    id: ParseId,
    prev: ParserPredecessor,
    parent_context: ContextId,
) -> VariableSet<(DefId, Span)> {
    let expr = ExprId(id.child(ctx.db, PathComponent::Unnamed(0)));
    val_expression(&ast.parser, ctx, expr, Some(parent_context));
    let pt = ParseStatement {
        id,
        front: prev,
        back: ParserPredecessor::ChildOf(parent_context),
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
        .map(|name| VariableSet::singular(name.inner, (id.0, name.span)))
        .unwrap_or_default()
}

pub fn choice_indirection(
    subs: &[(u32, DefId, Span)],
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

    fn insert(&self, id: DefId, node: HirNode, span: Vec<Span>) {
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

    fn modify_back_predecessor(&self, id: DefId, pred: ParserPredecessor) {
        let mut borrow = self.collection.borrow_mut();
        let node = match borrow
            .map
            .remove(&id)
            .expect("try to modify back predecessor of non-existent node")
        {
            HirNode::Parse(mut p) => {
                p.back = pred;
                HirNode::Parse(p)
            }
            HirNode::Choice(mut c) => {
                c.back = pred;
                HirNode::Choice(c)
            }
            _ => panic!("cannot set predecessor of non-parsing hir node"),
        };
        borrow.map.insert(id, node);
    }
}

pub fn hir_parser_collection(
    db: &dyn Hirs,
    did: DefId,
) -> Result<Option<HirParserCollection>, SilencedError> {
    let collection = HirParserCollection::new();
    let ctx = HirConversionCtx::new(collection, db);
    let path = db.lookup_intern_hir_path(did);
    let file = path.path()[0].unwrap_file();
    let id = path.path()[1].unwrap_ident();
    let parser = match db.top_level_statement(file, id)? {
        None => return Ok(None),
        Some(x) => x,
    };
    parser_def(&parser, &ctx, ParserDefId(did));
    Ok(Some(ctx.collection.into_inner()))
}
