mod convert;
pub mod error;
pub mod represent;
pub mod variable_set;
pub mod walk;

use std::{
    borrow::Cow,
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
    fmt::Debug,
    hash::Hash,
    sync::Arc,
};

use crate::{
    ast::{ArrayKind, AstConstraint},
    dbpanic,
    error::{SResult, SilencedError},
    expr::{self, Atom, Expression, ExpressionKind},
    interner::{DefId, FieldName, HirPath, Identifier, PathComponent, TypeVar},
    source::{FileId, IndirectSpan, Span},
};

use crate::source::IndexSpanned;
use crate::source::SpanIndex;
use enumflags2::{bitflags, BitFlags};
use fxhash::FxHashMap;
use variable_set::VariableSet;

use convert::hir_parser_collection;

use self::{convert::HirConversionErrors, walk::ChildIter};

#[salsa::query_group(HirDatabase)]
pub trait Hirs: crate::ast::Asts + crate::types::TypeInterner {
    fn hir_parser_collection(&self, did: DefId) -> SResult<Option<HirParserCollection>>;
    fn hir_node(&self, id: DefId) -> SResult<HirNode>;
    fn root_id(&self) -> ModuleId;
    fn all_def_ids(&self) -> Vec<DefId>;
    fn all_parserdefs(&self) -> Vec<ParserDefId>;
    fn all_exported_parserdefs(&self) -> Vec<ParserDefId>;
    fn hir_parent_module(&self, id: DefId) -> SResult<ModuleId>;
    fn hir_parent_parserdef(&self, id: DefId) -> SResult<ParserDefId>;
    fn hir_parent_block(&self, id: DefId) -> SResult<Option<BlockId>>;
    fn indirect_span(&self, span: IndirectSpan) -> SResult<Span>;
    fn indirection_targets(&self, id: DefId) -> SResult<Arc<Vec<DefId>>>;
    fn all_parserdef_blocks(&self, pd: ParserDefId) -> Arc<Vec<BlockId>>;
    fn sorted_block_fields(&self, bd: BlockId, discriminants: bool)
        -> SResult<Arc<Vec<FieldName>>>;
    fn sorted_field_index(
        &self,
        block: BlockId,
        name: FieldName,
        discriminants: bool,
    ) -> SResult<Option<usize>>;
    fn discriminant_mapping(&self, block: BlockId) -> SResult<Arc<FxHashMap<DefId, u64>>>;
}

fn hir_node(db: &dyn Hirs, id: DefId) -> SResult<HirNode> {
    let path = db.lookup_intern_hir_path(id);
    let file = path.path()[0].unwrap_file();
    let fid = match path.path().get(1) {
        Some(PathComponent::Named(n)) => n,
        None => return module_file(db, file).map(HirNode::Module),
        _ => dbpanic!(
            db,
            "Hir path {} does not have identifier as second element",
            &id
        ),
    };
    let collection_id = db.intern_hir_path(HirPath::new_fid(file, *fid));
    let hir_ctx = db
        .hir_parser_collection(collection_id)?
        .unwrap_or_else(|| dbpanic!(db, "Access to inexistent HIR path {}", &id));
    Ok(hir_ctx
        .map
        .get(&id)
        .unwrap_or_else(|| dbpanic!(db, "Access to inexistent HIR path {}", &id))
        .clone())
}

fn root_id(db: &dyn Hirs) -> ModuleId {
    let root_path = HirPath::new_file(FileId::default());
    ModuleId(db.intern_hir_path(root_path))
}

fn all_parserdef_blocks(db: &dyn Hirs, pd: ParserDefId) -> Arc<Vec<BlockId>> {
    let mut ret = Vec::new();
    for node in ChildIter::new(pd.0, db) {
        if let HirNode::Block(b) = node {
            ret.push(b.id)
        }
    }
    Arc::new(ret)
}

fn all_def_ids(db: &dyn Hirs) -> Vec<DefId> {
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

fn all_parserdefs(db: &dyn Hirs) -> Vec<ParserDefId> {
    let root = db.root_id();
    let module = match root.lookup(db) {
        Ok(m) => m,
        Err(_) => return Vec::new(),
    };
    let ret: Vec<_> = module.defs.values().cloned().collect();
    ret
}

fn all_exported_parserdefs(db: &dyn Hirs) -> Vec<ParserDefId> {
    db.all_parserdefs()
        .into_iter()
        .flat_map(|x| x.lookup(db))
        .flat_map(|x| (x.qualifier == Qualifier::Export).then_some(x.id))
        .collect()
}

fn hir_parent_module(db: &dyn Hirs, id: DefId) -> SResult<ModuleId> {
    // todo
    Ok(ModuleId(db.intern_hir_path(HirPath::new_file(
        db.lookup_intern_hir_path(id).path()[0].unwrap_file(),
    ))))
}

fn hir_parent_parserdef(db: &dyn Hirs, id: DefId) -> SResult<ParserDefId> {
    let path = db.lookup_intern_hir_path(id);
    // todo
    Ok(ParserDefId(db.intern_hir_path(HirPath::new_fid(
        path.path()[0].unwrap_file(),
        path.path()[1].unwrap_named(),
    ))))
}

fn hir_parent_block(db: &dyn Hirs, id: DefId) -> SResult<Option<BlockId>> {
    match db.hir_node(id)? {
        HirNode::Block(b) => return Ok(Some(b.id)),
        HirNode::ParserDef(_) => return Ok(None),
        _ => {}
    }
    let mut path = db.lookup_intern_hir_path(id);
    if path.pop().is_none() {
        return Ok(None);
    }
    let id = db.intern_hir_path(path);
    db.hir_parent_block(id)
}

fn indirect_span(db: &dyn Hirs, span: IndirectSpan) -> SResult<Span> {
    let id = db.hir_parent_parserdef(span.0)?;
    let collection = db.hir_parser_collection(id.0)?.ok_or(SilencedError)?;
    Ok(match span.1 {
        Some(index) => collection.span_with_index(span.0, index.as_usize()),
        None => collection.default_span(span.0),
    }
    .ok_or(SilencedError)?)
}

fn indirection_targets(db: &dyn Hirs, id: DefId) -> SResult<Arc<Vec<DefId>>> {
    let choice = if let HirNode::ChoiceIndirection(c) = db.hir_node(id)? {
        c
    } else {
        return Ok(Arc::new(vec![id]));
    };
    let mut ret = Vec::new();
    for (_, child) in choice.choices.iter() {
        ret.extend(db.indirection_targets(*child)?.iter())
    }
    Ok(Arc::new(ret))
}

fn sorted_block_fields(
    db: &dyn Hirs,
    bd: BlockId,
    discriminants: bool,
) -> SResult<Arc<Vec<FieldName>>> {
    let context = bd.lookup(db)?.root_context.lookup(db)?;
    let mut fields: Vec<_> = context
        .vars
        .set
        .into_values()
        .filter(|x| !(discriminants && x.is_accessible()))
        .map(|x| db.def_name(*x.inner()).unwrap())
        .collect();
    fields.sort_unstable_by_key(|def_id| match def_id {
        FieldName::Return => None,
        FieldName::Ident(id) => Some(db.lookup_intern_identifier(*id).name),
    });
    Ok(Arc::new(fields))
}

fn sorted_field_index(
    db: &dyn Hirs,
    block: BlockId,
    name: FieldName,
    discriminants: bool,
) -> SResult<Option<usize>> {
    let fields = db.sorted_block_fields(block, discriminants)?;
    let lookup_name = |x: &FieldName| match x {
        FieldName::Return => None,
        FieldName::Ident(f) => Some(db.lookup_intern_identifier(*f).name),
    };
    let needle = lookup_name(&name);
    Ok(fields.binary_search_by_key(&needle, lookup_name).ok())
}

fn discriminant_mapping(db: &dyn Hirs, block: BlockId) -> SResult<Arc<FxHashMap<DefId, u64>>> {
    let mut mapping = FxHashMap::default();
    let mut i = 0;
    let root_ctx = block.lookup(db)?.root_context.0;
    for field in db.sorted_block_fields(block, true)?.iter() {
        mapping.insert(root_ctx.child_field(db, *field), i);
        i += 1;
    }
    Ok(Arc::new(mapping))
}

macro_rules! hir_id_wrapper {
    {type $name:ident = $id:ident ($ty:ty);} => {
        #[derive(Clone, Copy, Hash, PartialEq, Eq, Debug, PartialOrd, Ord)]
        pub struct $name(pub DefId);
        impl HirIdWrapper for $name {
            type Inner = $ty;
            fn id(self) -> DefId {
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
            unsafe fn new_unchecked(id: DefId) -> Self {
                Self(id)
            }
        }
    };
    {$(type $name:ident = $id:ident ($ty:ty);)*} => {
        $(hir_id_wrapper!{type $name= $id($ty);})*
    };
}

macro_rules! hir_node_enum {
    {pub enum HirNode {$($variant:ident($inner:ty)),*$(,)?}} => {
        #[derive(Clone, Hash, PartialEq, Eq, Debug)]
        pub enum HirNode {
            $($variant($inner)),*
        }

        #[bitflags]
        #[repr(u32)]
        #[derive(Clone, Copy, Debug, PartialEq)]
        pub enum HirNodeKind {
            $($variant),*
        }

        impl HirNode {
            pub fn children(&self) -> Vec<DefId> {
                match self {
                    $(HirNode::$variant(x) => x.children()),*
                }
            }
            pub fn is_kind(&self, kind: BitFlags<HirNodeKind>) -> bool {
                match self {
                    $(HirNode::$variant(_) => kind.contains(HirNodeKind::$variant)),*
                }
            }
            pub fn id(&self) -> DefId {
                match self {
                    $(HirNode::$variant(x) => x.id.id()),*
                }
            }
        }
    };
}

hir_node_enum! {
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
    fn id(self) -> DefId;
    fn extract(node: HirNode) -> Self::Inner;
    unsafe fn new_unchecked(id: DefId) -> Self;
    fn child<DB: Hirs + ?Sized>(self, db: &DB, add: PathComponent) -> DefId {
        self.id().child(db, add)
    }
    fn lookup<DB: Hirs + ?Sized>(self, db: &DB) -> SResult<Self::Inner> {
        let id = self.id();
        Ok(Self::extract(db.hir_node(id)?))
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug, Default)]
pub struct HirParserCollection {
    map: BTreeMap<DefId, HirNode>,
    spans: BTreeMap<DefId, Vec<Span>>,
    errors: HirConversionErrors,
}

impl HirParserCollection {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn default_span(&self, id: DefId) -> Option<Span> {
        self.spans.get(&id)?.last().copied()
    }
    pub fn span_with_index(&self, id: DefId, index: usize) -> Option<Span> {
        self.spans.get(&id)?.get(index).copied()
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Module {
    pub id: ModuleId,
    pub defs: BTreeMap<Identifier, ParserDefId>,
    pub submods: BTreeMap<Identifier, ModuleId>,
}

impl Module {
    pub fn children(&self) -> Vec<DefId> {
        self.defs
            .values()
            .map(|x| x.0)
            .chain(self.submods.values().map(|x| x.0))
            .collect()
    }
}

fn module_file(db: &dyn Hirs, file: FileId) -> Result<Module, SilencedError> {
    let id = ModuleId(db.intern_hir_path(HirPath::new_file(file)));
    let defs = db
        .symbols(file)?
        .iter()
        .map(|sym| {
            (
                *sym,
                ParserDefId(db.intern_hir_path(HirPath::new_fid(file, FieldName::Ident(*sym)))),
            )
        })
        .collect();
    Ok(Module {
        id,
        defs,
        submods: BTreeMap::new(),
    })
}

pub type HirConstraint = AstConstraint;

pub type HirConstraintSpanned = expr::KindWithData<HirConstraint, SpanIndex>;

pub type ConstraintExpression = Expression<HirConstraintSpanned>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirVal;

impl ExpressionKind for HirVal {
    type DyadicOp = expr::ValBinOp;
    type MonadicOp = expr::ValUnOp<Arc<ConstraintExpression>>;
    type NiladicOp = ParserAtom;
}

pub type HirValSpanned = expr::KindWithData<HirVal, SpanIndex>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirType;

impl ExpressionKind for HirType {
    type DyadicOp = expr::TypeBinOp;
    type MonadicOp = expr::TypeUnOp<Arc<ConstraintExpression>>;
    type NiladicOp = TypeAtom;
}

pub type HirTypeSpanned = expr::KindWithData<HirType, SpanIndex>;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ValExpression {
    pub id: ExprId,
    pub expr: Expression<HirValSpanned>,
    pub children: Vec<DefId>,
    pub parent_context: Option<ContextId>,
}

impl ValExpression {
    fn children(&self) -> Vec<DefId> {
        self.children.clone()
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct TypeExpression {
    pub id: TExprId,
    pub expr: Expression<HirTypeSpanned>,
}

impl TypeExpression {
    fn children(&self) -> Vec<DefId> {
        Vec::new()
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub enum Qualifier {
    Export,
    Regular,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDef {
    pub id: ParserDefId,
    pub qualifier: Qualifier,
    pub from: TExprId,
    pub to: ExprId,
}

impl ParserDef {
    pub fn children(&self) -> Vec<DefId> {
        vec![self.from.0, self.to.0]
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
struct ParentInfo {
    parent_choice: Option<ChoiceId>,
    parent_context: Option<ContextId>,
    block_id: BlockId,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Block {
    pub id: BlockId,
    pub root_context: ContextId,
    pub super_context: Option<ContextId>,
    pub enclosing_expr: ExprId,
    pub returns: bool,
}

impl Block {
    pub fn children(&self) -> Vec<DefId> {
        vec![self.root_context.0]
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ChoiceIndirection {
    pub id: ChoiceIndirectId,
    pub parent_context: ContextId,
    pub choices: Vec<(u32, DefId)>,
    pub target_choice: ChoiceId,
}

impl ChoiceIndirection {
    pub fn children(&self) -> Vec<DefId> {
        Vec::new()
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct StructChoice {
    pub id: ChoiceId,
    pub parent_context: ContextId,
    pub front: ParserPredecessor,
    pub back: ParserPredecessor,
    pub subcontexts: Vec<ContextId>,
}

impl StructChoice {
    pub fn children(&self) -> Vec<DefId> {
        self.subcontexts.iter().map(|x| x.0).collect()
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct StructCtx {
    pub id: ContextId,
    pub block_id: BlockId,
    pub parent_choice: Option<ChoiceId>,
    pub parent_context: Option<ContextId>,
    pub vars: Box<VariableSet<DefId>>,
    pub children: Box<Vec<DefId>>,
    pub endpoints: Option<(DefId, DefId)>,
}

impl StructCtx {
    pub fn children(&self) -> Vec<DefId> {
        self.children.as_ref().clone()
    }
}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct LetStatement {
    pub id: LetId,
    pub ty: Option<TExprId>,
    pub expr: ExprId,
    pub context: ContextId,
}

impl LetStatement {
    fn children(&self) -> Vec<DefId> {
        let mut children = vec![self.expr.0];
        if let Some(ty) = self.ty {
            children.push(ty.0);
        }
        children
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum ParserPredecessor {
    ChildOf(ContextId),
    After(DefId),
}

impl ParserPredecessor {
    pub fn id(self) -> DefId {
        match self {
            ParserPredecessor::ChildOf(id) => id.0,
            ParserPredecessor::After(id) => id,
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParseStatement {
    pub id: ParseId,
    pub parent_context: ContextId,
    pub front: ParserPredecessor,
    pub back: ParserPredecessor,
    pub expr: ExprId,
}

impl ParseStatement {
    pub fn children(&self) -> Vec<DefId> {
        vec![self.expr.0]
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum TypeAtom {
    Primitive(TypePrimitive),
    ParserDef(Box<ParserDefRef>),
    Array(Box<TypeArray>),
    TypeVar(TypeVar),
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum TypePrimitive {
    Mem,
    Int,
    Bit,
    Char,
}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefRef {
    pub from: Option<Expression<HirTypeSpanned>>,
    pub name: IndexSpanned<Identifier>,
    pub args: Vec<Expression<HirTypeSpanned>>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ParserAtom {
    Atom(Atom),
    Single,
    Nil,
    Block(BlockId),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct TypeArray {
    pub direction: ArrayKind,
    pub expr: Expression<HirTypeSpanned>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserArray {
    pub id: ArrayId,
    pub direction: ArrayKind,
    pub context: Option<ContextId>,
    pub expr: ExprId,
    pub enclosing_expr: ExprId,
}

impl ParserArray {
    pub fn children(&self) -> Vec<DefId> {
        vec![self.expr.0]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::Context;
    #[test]
    fn eval_test() {
        let ctx = Context::mock(
            r#"
def for [u8] *> expr1 = {
  | a: u64
    b: u32
  | a: u64,
}
        "#,
        );
        eprintln!("{:?}", ctx.db.all_def_ids());
    }
}