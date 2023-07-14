use std::collections::HashMap;

use ast::expr::Ignorable;
use fxhash::FxHashSet;
use yaboc_expr::{DataExpr, ExprHead};

use super::*;
use yaboc_ast::{self as ast, expr::OpWithData, TopLevelStatement};
use yaboc_base::{error::Silencable, error_type, source::Spanned};

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
    EofInconsistentConjunction {
        span: Span,
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

error_type!(HirConversionErrors(BTreeSet<HirConversionError>) in crate);

fn ignores_eof(ast: &ast::ConstraintExpression) -> Result<bool, HirConversionError> {
    match ast.0.as_ref().map_inner(|i| ignores_eof(i)).transpose()? {
        ExpressionHead::Niladic(m) => match m.inner {
            ast::ConstraintAtom::NotEof => Ok(false),
            _ => Ok(true),
        },
        ExpressionHead::Monadic(m) => Ok(m.inner),
        ExpressionHead::Dyadic(d) => match d.op.inner {
            expr::ConstraintBinOp::And => Ok(d.inner[0] && d.inner[1]),
            expr::ConstraintBinOp::Or => {
                let [left, right] = d.inner;
                if left != right {
                    Err(HirConversionError::EofInconsistentConjunction { span: d.op.data })
                } else {
                    Ok(left)
                }
            }
        },
        ExpressionHead::Variadic(v) => v.op.ignore(),
    }
}

fn constraint_expression(
    ast: &ast::ConstraintExpression,
    ctx: &HirConversionCtx,
    add_span: &impl Fn(&Span) -> SpanIndex,
) -> HirConstraintId {
    let ignores_eof = match ignores_eof(ast) {
        Ok(k) => k,
        Err(e) => {
            ctx.add_errors(Some(e));
            true
        }
    };
    let ret = DataExpr::new_from_unfold(ast, |expr| {
        let span = *expr.0.root_data();
        let expr = match &expr.0 {
            ExpressionHead::Niladic(n) => ExprHead::Niladic(n.inner.clone()),
            ExpressionHead::Monadic(m) => ExprHead::Monadic(m.op.inner.clone(), &*m.inner),
            ExpressionHead::Dyadic(d) => {
                ExprHead::Dyadic(d.op.inner.clone(), [&*d.inner[0], &*d.inner[1]])
            }
            ExpressionHead::Variadic(v) => match v.op.inner {},
        };
        (expr, add_span(&span))
    });
    ctx.db.intern_hir_constraint(HirConstraintExpressionRoot {
        expr: ret.expr,
        data: ret.data,
        has_no_eof: !ignores_eof,
    })
}

fn val_expression(
    expr: &ast::ValExpression,
    ctx: &HirConversionCtx,
    id: ExprId,
    parent_context: Option<ContextId>,
) {
    let spans = RefCell::new(Vec::new());
    let mut children = Vec::new();
    let add_span = SpanIndex::add_span(&spans);
    let mut new_id = || {
        let index: u32 = u32::try_from(children.len()).unwrap();
        let new_id = id.child(ctx.db, PathComponent::Unnamed(index));
        children.push(new_id);
        new_id
    };
    let expr = DataExpr::new_from_unfold(expr, |expr| {
        let expr_res = match &expr.0 {
            ExpressionHead::Niladic(n) => ExprHead::Niladic(match &n.inner {
                ast::ParserAtom::Atom(atom) => ParserAtom::Atom(atom.clone()),
                ast::ParserAtom::Single => ParserAtom::Single,
                ast::ParserAtom::Nil => ParserAtom::Nil,
                ast::ParserAtom::Array => ParserAtom::Array,
                ast::ParserAtom::Regex(re, bt) => ParserAtom::Regex(*re, *bt),
                ast::ParserAtom::Block(b) => {
                    let nid = BlockId(new_id());
                    block(b, ctx, nid, parent_context, id);
                    ParserAtom::Block(nid)
                }
            }),
            ExpressionHead::Monadic(m) => ExprHead::Monadic(
                m.op.inner
                    .map_expr(|constr| constraint_expression(constr, ctx, &add_span)),
                &*m.inner,
            ),
            ExpressionHead::Dyadic(d) => {
                ExprHead::Dyadic(d.op.inner.clone(), [&*d.inner[0], &*d.inner[1]])
            }
            ExpressionHead::Variadic(v) => {
                ExprHead::Variadic(v.op.inner.clone(), v.inner.iter().map(|e| &**e).collect())
            }
        };
        (expr_res, add_span(expr.0.root_data()))
    });
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
    ctx: &HirConversionCtx,
    add_span: &impl Fn(&Span) -> SpanIndex,
) -> DataExpr<HirType, SpanIndex> {
    DataExpr::new_from_unfold(expr, |expr| {
        let span = *expr.0.root_data();
        let expr = match &expr.0 {
            ExpressionHead::Niladic(n) => match &n.inner {
                ast::TypeAtom::ParserDef(pd) => {
                    let from = pd
                        .from
                        .as_ref()
                        .map(|x| convert_type_expression(x, ctx, add_span));
                    let args = pd
                        .args
                        .iter()
                        .map(|x| convert_type_expression(x, ctx, add_span))
                        .collect();
                    let name = pd
                        .name
                        .iter()
                        .map(|name| IndexSpanned {
                            span: add_span(&name.span),
                            atom: name.inner,
                        })
                        .collect();
                    ExprHead::Niladic(TypeAtom::ParserDef(Box::new(ParserDefRef {
                        from,
                        name,
                        args,
                    })))
                }
                ast::TypeAtom::Primitive(a) => ExprHead::Niladic(TypeAtom::Primitive((*a).into())),
                ast::TypeAtom::Array(arr) => {
                    let new_expr = convert_type_expression(&arr.expr, ctx, add_span);
                    ExprHead::Niladic(TypeAtom::Array(Box::new(TypeArray {
                        direction: arr.direction.inner,
                        expr: new_expr,
                    })))
                }
                ast::TypeAtom::TypeVar(v) => ExprHead::Niladic(TypeAtom::TypeVar(*v)),
            },
            ExpressionHead::Monadic(monadic) => {
                let new_op = monadic
                    .op
                    .inner
                    .map_expr(|constr| constraint_expression(constr, ctx, add_span));
                ExprHead::Monadic(new_op, &*monadic.inner)
            }
            ExpressionHead::Dyadic(dyadic) => ExprHead::Dyadic(
                dyadic.op.inner.clone(),
                [&*dyadic.inner[0], &*dyadic.inner[1]],
            ),
            ExpressionHead::Variadic(variadic) => ExprHead::Variadic(
                variadic.op.inner,
                variadic.inner.iter().map(|x| &**x).collect(),
            ),
        };
        (expr, add_span(&span))
    })
}

fn type_expression(ast: &ast::TypeExpression, ctx: &HirConversionCtx, id: TExprId) {
    let spans = RefCell::new(Vec::new());
    let add_span = SpanIndex::add_span(&spans);
    let texpr = convert_type_expression(ast, ctx, &add_span);
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
    if let Some(f) = &ast.from {
        type_expression(f, ctx, from);
    } else {
        let span = ast.name.span;
        // desugars to for[int]
        let expr = Expression::new_niladic(OpWithData {
            inner: ast::TypeAtom::Array(Box::new(ast::TypeArray {
                direction: Spanned {
                    inner: ArrayKind::Each,
                    span,
                },
                expr: Expression::new_niladic(OpWithData {
                    inner: ast::TypeAtom::Primitive(ast::TypePrimitive::Int),
                    data: ast.span,
                }),
                span,
            })),
            data: ast.span,
        });
        type_expression(&expr, ctx, from);
    }
    let ret_ty = if let Some(f) = &ast.ret_ty {
        let ret_ty = TExprId(id.child(ctx.db, PathComponent::Unnamed(2)));
        type_expression(f, ctx, ret_ty);
        Some(ret_ty)
    } else {
        None
    };
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
    let thunky = ast.thunky;
    let pdef = ParserDef {
        qualifier,
        id,
        thunky,
        from,
        args,
        to,
        ret_ty,
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
            let (vars, _) = struct_context(c, ctx, context_id, &parents);
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
) -> (VariableSet<Vec<(u32, DefId, Span)>>, bool) {
    let children = &ast.content;
    let parents = ParentInfo {
        parent_choice: Some(id),
        ..*parents
    };
    let mut subcontexts = Vec::new();
    let mut varset = None;
    let mut subvars = Vec::new();
    let mut has_non_zero_len = false;
    for (idx, child) in children.iter().enumerate() {
        let subcontext_id = ContextId(id.child(ctx.db, PathComponent::Unnamed(idx as u32)));
        subcontexts.push(subcontext_id);
        let (new_vars, non_zero_len_context) = struct_context(child, ctx, subcontext_id, &parents);
        has_non_zero_len |= non_zero_len_context;
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

    let endpoints = if has_non_zero_len {
        Some([
            pred,
            ParserPredecessor::ChildOf(parents.parent_context.unwrap()),
        ])
    } else {
        None
    };
    let choice = StructChoice {
        id,
        parent_context: parents.parent_context.unwrap(),
        endpoints,
        subcontexts,
    };
    ctx.insert(id.0, HirNode::Choice(choice), vec![ast.span]);
    (varset, has_non_zero_len)
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
) -> (VariableSet<(DefId, Span)>, bool) {
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
    let mut update_pred = |pred: &mut ParserPredecessor, new| {
        let old = *pred;
        let new_pred = ParserPredecessor::After(new);
        endpoints = Some(
            endpoints
                .map(|(front, _)| (front, new))
                .unwrap_or((new, new)),
        );
        match *pred {
            ParserPredecessor::After(id) => ctx.modify_back_predecessor(id, new_pred),
            ParserPredecessor::ChildOf(_) => (),
        }
        *pred = new_pred;
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
                let (choice_indirect, nonzero_len) = struct_choice(c, ctx, sub_id, pred, &parents);
                if nonzero_len {
                    update_pred(&mut pred, sub_id.0);
                }
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
                        parse_statement(p, ctx, ParseId(sub_id), update_pred(&mut pred, sub_id), id)
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
    (varset, endpoints.is_some())
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
        type_expression(ty, ctx, ty_id);
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
                c.endpoints
                    .as_mut()
                    .expect("try to modify back predecessor of choice without endpoints")[1] = pred;
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
        None | Some(TopLevelStatement::Import(_)) => return Ok(None),
        Some(TopLevelStatement::ParserDefinition(x)) => x,
    };
    parser_def(&parser, &ctx, ParserDefId(did));
    Ok(Some(ctx.collection.into_inner()))
}
