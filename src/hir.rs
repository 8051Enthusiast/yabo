use std::{
    cell::RefCell,
    collections::{btree_map::Entry, BTreeMap},
};

use crate::{
    ast::{self, ArrayKind, AstConstraint, AstParse},
    expr::{self, Atom, ExprConverter, ExpressionKind},
    interner::{HirId, HirPath, Identifier, PathComponent, StructChoiceIdx},
    source::{FileId, Span, Spanned},
};
#[salsa::query_group(HirDatabase)]
pub trait Hirs: ast::Asts {
    fn hir_conv_ctx(&self, file: FileId, id: Identifier) -> Result<Option<HirConversionCtx>, ()>;
    fn hir_node(&self, id: HirId) -> Result<HirNode, ()>;
}

fn hir_conv_ctx(
    db: &dyn Hirs,
    file: FileId,
    id: Identifier,
) -> Result<Option<HirConversionCtx>, ()> {
    let mut ctx = HirConversionCtx::new();
    let path = HirPath::new(file, id);
    let hid = db.intern_hir_path(path.clone());
    let parser = match db.top_level_statement(file, id)? {
        None => return Ok(None),
        Some(x) => x,
    };
    match *parser {
        ast::Statement::ParserDef(ref p) => {
            ctx.spans.insert(hid, p.span);
            let def = HirNode::ParserDef(ParserDef {
                id: hid,
                from: todo!(),
                to: todo!(),
            });
            ctx.map.insert(hid, def);
        }
        ast::Statement::Parse(_) => panic!("Invalid top-level statement type parse"),
        ast::Statement::Let(_) => panic!("Invalid top-level statement type let"),
    }
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
        .hir_conv_ctx(file, fid)?
        .unwrap_or_else(|| panic!("Access to inexistent HIR path {}", db.path_name(id)));
    Ok(hir_ctx
        .map
        .get(&id)
        .unwrap_or_else(|| panic!("Access to inexistent HIR path {}", db.path_name(id)))
        .clone())
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct SpanIndex(usize);
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirConstraint {}

impl ExpressionKind for HirConstraint {
    type BinaryOp = expr::ConstraintBinOp<HirConstraint, SpanIndex>;
    type UnaryOp = expr::ConstraintUnOp<HirConstraint, SpanIndex>;
    type Atom = (Atom, SpanIndex);
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirParse {}

impl ExpressionKind for HirParse {
    type BinaryOp = expr::ParseBinOp<HirParse, HirConstraint, SpanIndex>;
    type UnaryOp = expr::ParseUnOp<HirParse, SpanIndex>;
    type Atom = (Atom, SpanIndex);
}

fn constraint_expression<'a>(
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

fn parse_expression<'a>(
    add_span: &'a impl Fn(&Span) -> SpanIndex,
) -> ExprConverter<'a, AstParse, HirParse> {
    let atom_fun = move |x: &Spanned<ast::ParserAtom>| -> (Atom, SpanIndex) {
        let n = add_span(&x.span);
        let atom = match &x.inner {
            ast::ParserAtom::Array(_) => todo!(),
            ast::ParserAtom::Atom(atom) => atom.clone(),
            ast::ParserAtom::Block(_) => todo!(),
        };
        (atom, n)
    };
    let constraint_converter = constraint_expression(add_span);
    type PConverter<'b> = ExprConverter<'b, ast::AstParse, HirParse>;
    let pbin_fun = move |bop: &ast::AstParseBinOp, c: &PConverter| {
        bop.convert_same(c, &constraint_converter, add_span)
    };
    let pun_fun = move |bop: &ast::AstParseUnOp, c: &PConverter| bop.convert_same(c, add_span);
    ExprConverter::new(pbin_fun, pun_fun, atom_fun)
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum HirNode {
    Let(LetStatement),
    Parse(ParseStatement),
    Context(StructCtx),
    ParserDef(ParserDef),
}
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct HirNodeIdx(u32);

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct HirConversionCtx {
    map: BTreeMap<HirId, HirNode>,
    spans: BTreeMap<HirId, Span>,
}

impl HirConversionCtx {
    pub fn new() -> Self {
        Self {
            map: BTreeMap::new(),
            spans: BTreeMap::new(),
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDef {
    id: HirId,
    from: HirId,
    to: HirId,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Struct {
    root_context: StructCtx,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct StructChoice {
    pub subcontexts: Vec<HirId>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct StructCtx {
    pub id: HirId,
    pub parent_context: Option<HirId>,
    pub choices: Vec<StructChoice>,
    pub vars: VariableSet,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct LetStatement {
    pub id: HirId,
    pub ty: Option<ast::ParseExpression>,
    pub expr: ast::ValExpression,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ParserAtom {
    Atom(Atom),
    Array(ParserArray),
    Struct(Struct),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserArray {
    pub direction: ArrayKind,
    pub expr: HirId,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParseStatement {
    pub id: HirId,
    pub parser: ast::ParseExpression,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct VariableSet {
    set: BTreeMap<Identifier, VarStatus>,
}

impl VariableSet {
    pub fn merge_sum(&self, other: &Self, idx: StructChoiceIdx) -> Self {
        let mut new = BTreeMap::new();
        new.extend(self.set.iter().map(|(k, v)| {
            (
                *k,
                VarStatus::from_accessibility(v.is_accessible() && other.set.contains_key(k), idx),
            )
        }));
        for (k, v) in other.set.iter() {
            let other_entry = v.is_accessible();
            new.entry(*k)
                .and_modify(|e| {
                    *e = VarStatus::from_accessibility(e.is_accessible() && other_entry, idx)
                })
                .or_insert(VarStatus::Existent(idx));
        }
        VariableSet { set: new }
    }
    pub fn merge_product(&self, other: &Self) -> Result<Self, Vec<Identifier>> {
        let mut new = self.set.clone();
        let mut doubled = Vec::new();
        for (k, v) in other.set.iter() {
            match new.entry(*k) {
                Entry::Vacant(entry) => {
                    entry.insert(*v);
                }
                Entry::Occupied(_) => doubled.push(*k),
            }
        }
        if doubled.is_empty() {
            Ok(VariableSet { set: new })
        } else {
            Err(doubled)
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum VarStatus {
    /// the current contexts owns the variable
    Owned(HirId),
    /// the current context does not own the variable,
    /// but every variant of a choice has it, making
    /// it always accessible
    Present(StructChoiceIdx),
    /// the variable does exist somewhere under the current
    /// context, but is not always available because it is only
    /// in parts of a choice
    Existent(StructChoiceIdx),
}

impl VarStatus {
    fn is_accessible(self) -> bool {
        !matches!(self, Self::Existent(_))
    }
    fn from_accessibility(access: bool, idx: StructChoiceIdx) -> Self {
        if access {
            Self::Present(idx)
        } else {
            Self::Existent(idx)
        }
    }
}
