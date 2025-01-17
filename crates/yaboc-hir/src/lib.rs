#![allow(clippy::type_complexity)]
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

use salsa::InternId;
use yaboc_ast::{
    expr::{self, Atom, ConstraintBinOp, ConstraintUnOp, Expression, ExpressionHead, Unused},
    ConstraintAtom,
};
use yaboc_ast::{ArrayKind, TopLevelStatement};
use yaboc_base::{
    dbpanic,
    error::{SResult, SilencedError},
    interner::{
        DefId, DefinitionPath, FieldName, Identifier, IdentifierName, PathComponent, Regex,
    },
    source::{FileId, IndexSpanned, IndirectSpan, Span, SpanIndex},
    Context,
};

use enumflags2::{bitflags, BitFlags};
use fxhash::FxHashMap;
use variable_set::VariableSet;

use convert::hir_parser_collection;
use yaboc_expr::{DataExpr, ExprKind, IdxExpression, ShapedData};

use self::{convert::HirConversionErrors, walk::ChildIter};

#[salsa::query_group(HirDatabase)]
pub trait Hirs: yaboc_ast::Asts {
    fn hir_parser_collection(&self, did: DefId) -> SResult<Option<HirParserCollection>>;
    fn hir_node(&self, id: DefId) -> SResult<HirNode>;
    fn all_modules(&self) -> Vec<ModuleId>;
    fn all_def_ids(&self) -> Vec<DefId>;
    fn all_parserdefs(&self) -> Vec<ParserDefId>;
    fn all_exported_parserdefs(&self) -> Vec<ParserDefId>;
    fn hir_parent_module(&self, id: DefId) -> SResult<ModuleId>;
    fn hir_parent_parserdef(&self, id: DefId) -> SResult<ParserDefId>;
    fn hir_parent_closure(&self, id: DefId) -> SResult<Option<DefId>>;
    fn indirect_span(&self, span: IndirectSpan) -> SResult<Span>;
    fn indirection_targets(&self, id: DefId) -> SResult<Arc<Vec<DefId>>>;
    fn all_parserdef_blocks(&self, pd: ParserDefId) -> Arc<Vec<BlockId>>;
    fn all_parserdef_lambdas(&self, pd: ParserDefId) -> Arc<Vec<LambdaId>>;
    fn sorted_block_fields(&self, bd: BlockId, discriminants: bool)
        -> SResult<Arc<Vec<FieldName>>>;
    fn sorted_field_index(
        &self,
        block: BlockId,
        name: FieldName,
        discriminants: bool,
    ) -> SResult<Option<usize>>;
    fn discriminant_mapping(&self, block: BlockId) -> SResult<Arc<FxHashMap<DefId, u64>>>;
    fn argnum(&self, pd: ParserDefId) -> SResult<Option<usize>>;
    fn parserdef_arg(&self, pd: ParserDefId, name: Identifier) -> SResult<Option<ArgDefId>>;
    fn parserdef_arg_index(&self, pd: ParserDefId, id: DefId) -> SResult<Option<usize>>;
    fn lambda_arg(&self, pd: LambdaId, name: Identifier) -> SResult<Option<ArgDefId>>;
    fn lambda_arg_index(&self, lambda: LambdaId, id: DefId) -> SResult<Option<usize>>;
    fn core_items(&self) -> SResult<CoreItems>;
    #[salsa::interned]
    fn intern_hir_constraint(&self, c: HirConstraintExpressionRoot) -> HirConstraintId;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum CoreItem {
    Compose,
    Index,
    StartWith,
}

impl CoreItem {
    fn name(self) -> &'static str {
        match self {
            CoreItem::Compose => "compose",
            CoreItem::Index => "index",
            CoreItem::StartWith => "start_with",
        }
    }
}

const CORE_ITEM_LIST: &[CoreItem] = &[CoreItem::Compose, CoreItem::Index, CoreItem::StartWith];

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirConstraintId(InternId);

impl salsa::InternKey for HirConstraintId {
    fn from_intern_id(v: InternId) -> Self {
        Self(v)
    }

    fn as_intern_id(&self) -> InternId {
        self.0
    }
}

impl HirConstraintId {
    pub fn as_u32(self) -> u32 {
        self.0.as_u32()
    }
}

fn hir_node(db: &dyn Hirs, id: DefId) -> SResult<HirNode> {
    let (top_component, top_parent) = match db.lookup_intern_hir_path(id) {
        DefinitionPath::Module(file) => return module_file(db, file).map(HirNode::Module),
        DefinitionPath::Path(top, top_parent) => (top, top_parent),
    };
    let collection_id = if let DefinitionPath::Module(file) = db.lookup_intern_hir_path(top_parent)
    {
        let PathComponent::Named(fid) = top_component else {
            dbpanic!(
                db,
                "Hir path {} does not have identifier as second element",
                &id
            )
        };
        let name = fid.unwrap_ident();
        if let Some(TopLevelStatement::Import(_)) = db.top_level_statement(file, name)? {
            let mod_file = db.import_id(file, name)?;
            let mod_path = db.intern_hir_path(DefinitionPath::Module(mod_file));
            return Ok(HirNode::Import(Import {
                id: ImportId(id),
                name,
                mod_ref: ModuleId(mod_path),
            }));
        }
        // if it is file.name, but not an import, it can only be a pd
        id
    } else {
        db.hir_parent_parserdef(id)?.0
    };
    let hir_ctx = db
        .hir_parser_collection(collection_id)?
        .unwrap_or_else(|| dbpanic!(db, "Access to inexistent HIR path {}", &id));
    Ok(hir_ctx
        .map
        .get(&id)
        .unwrap_or_else(|| dbpanic!(db, "Access to inexistent HIR path {}", &id))
        .clone())
}

fn all_modules(db: &dyn Hirs) -> Vec<ModuleId> {
    db.all_files()
        .iter()
        .map(|id| {
            let path = DefinitionPath::Module(*id);
            ModuleId(db.intern_hir_path(path))
        })
        .collect()
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
fn all_parserdef_lambdas(db: &dyn Hirs, pd: ParserDefId) -> Arc<Vec<LambdaId>> {
    let mut ret = Vec::new();
    for node in ChildIter::new(pd.0, db) {
        if let HirNode::Lambda(l) = node {
            ret.push(l.id)
        }
    }
    Arc::new(ret)
}

fn all_def_ids(db: &dyn Hirs) -> Vec<DefId> {
    let mut ret = Vec::new();
    for module in db
        .all_modules()
        .into_iter()
        .flat_map(|module| module.lookup(db))
    {
        let collections: Vec<_> = module
            .defs
            .values()
            .map(|id| db.hir_parser_collection(id.0))
            .collect();
        ret.push(module.id.0);
        for c in collections {
            let collection = match c {
                Err(_) | Ok(None) => continue,
                Ok(Some(col)) => col,
            };
            ret.extend(collection.map.keys().cloned())
        }
    }
    ret
}

fn all_parserdefs(db: &dyn Hirs) -> Vec<ParserDefId> {
    let mut ret = Vec::new();
    for module in db
        .all_modules()
        .into_iter()
        .flat_map(|module| module.lookup(db))
    {
        ret.extend(module.defs.values().cloned());
    }
    ret
}

fn all_exported_parserdefs(db: &dyn Hirs) -> Vec<ParserDefId> {
    db.all_parserdefs()
        .into_iter()
        .flat_map(|x| x.lookup(db))
        .flat_map(|x| (x.qualifier == Qualifier::Export).then_some(x.id))
        .collect()
}

/// finds the parent module of a given def id
/// the def id will just consist of a single component, namely the file
fn hir_parent_module(db: &dyn Hirs, id: DefId) -> SResult<ModuleId> {
    let mut current_id = id;
    loop {
        let path = db.lookup_intern_hir_path(current_id);
        match path {
            DefinitionPath::Module(_) => return Ok(ModuleId(db.intern_hir_path(path))),
            DefinitionPath::Path(_, parent) => current_id = parent,
        }
    }
}

/// finds the parent parserdef of a given def id
/// the def id will just consist of two components, namely the file and a named component
fn hir_parent_parserdef(db: &dyn Hirs, id: DefId) -> SResult<ParserDefId> {
    let mut previous_id = id;
    let mut current_id = id.parent(db).expect("child of pd has no parent");
    loop {
        let path = db.lookup_intern_hir_path(current_id);
        match path {
            DefinitionPath::Module(_) => return Ok(ParserDefId(previous_id)),
            DefinitionPath::Path(_, parent) => {
                previous_id = current_id;
                current_id = parent;
            }
        }
    }
}

fn hir_parent_closure(db: &dyn Hirs, id: DefId) -> SResult<Option<DefId>> {
    match db.hir_node(id)? {
        HirNode::Block(b) => return Ok(Some(b.id.0)),
        HirNode::Lambda(l) => return Ok(Some(l.id.0)),
        HirNode::ParserDef(_) => return Ok(None),
        _ => {}
    }
    db.hir_parent_closure(id.parent(db).expect("child of pd has no parent"))
}

fn indirect_span(db: &dyn Hirs, span: IndirectSpan) -> SResult<Span> {
    let id = db.hir_parent_parserdef(span.0)?;
    let collection = db
        .hir_parser_collection(id.0)?
        .ok_or_else(SilencedError::new)?;
    match span.1 {
        Some(index) => collection.span_with_index(span.0, index.as_usize()),
        None => collection.default_span(span.0),
    }
    .ok_or_else(SilencedError::new)
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
    let root_ctx = block.lookup(db)?.root_context.0;
    for (i, field) in db.sorted_block_fields(block, true)?.iter().enumerate() {
        mapping.insert(root_ctx.child_field(db, *field), i as u64);
    }
    Ok(Arc::new(mapping))
}

fn argnum(db: &dyn Hirs, pd: ParserDefId) -> SResult<Option<usize>> {
    let pd = pd.lookup(db)?;
    Ok(pd.args.map(|x| x.len()))
}

fn parserdef_arg(db: &dyn Hirs, pd: ParserDefId, name: Identifier) -> SResult<Option<ArgDefId>> {
    let pd = pd.lookup(db)?;
    let arg = pd
        .args
        .as_deref()
        .unwrap_or_default()
        .iter()
        .find(|x| db.def_name(x.0) == Some(FieldName::Ident(name)))
        .copied();
    Ok(arg)
}

fn parserdef_arg_index(db: &dyn Hirs, pd: ParserDefId, id: DefId) -> SResult<Option<usize>> {
    let pd = pd.lookup(db)?;
    let index = pd
        .args
        .as_deref()
        .unwrap_or_default()
        .iter()
        .position(|x| x.0 == id);
    Ok(index)
}

fn lambda_arg(db: &dyn Hirs, lambda_id: LambdaId, name: Identifier) -> SResult<Option<ArgDefId>> {
    let lambda = lambda_id.lookup(db)?;
    let arg = lambda
        .args
        .iter()
        .find(|x| db.def_name(x.0) == Some(FieldName::Ident(name)))
        .copied();
    Ok(arg)
}

fn lambda_arg_index(db: &dyn Hirs, lambda: LambdaId, id: DefId) -> SResult<Option<usize>> {
    let lambda = lambda.lookup(db)?;
    let index = lambda.args.iter().position(|x| x.0 == id);
    Ok(index)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CoreItems {
    items: Arc<Vec<ParserDefId>>,
}

impl std::ops::Index<CoreItem> for CoreItems {
    type Output = ParserDefId;

    fn index(&self, index: CoreItem) -> &Self::Output {
        &self.items[index as u8 as usize]
    }
}

fn core_items(db: &dyn Hirs) -> SResult<CoreItems> {
    let mut items = Vec::new();
    for item in CORE_ITEM_LIST.iter() {
        let name = item.name();
        let name = db.intern_identifier(IdentifierName { name: name.into() });
        let core_item = db.intern_hir_path(DefinitionPath::Path(
            PathComponent::Named(FieldName::Ident(name)),
            db.intern_hir_path(DefinitionPath::Module(db.core()?)),
        ));
        let HirNode::ParserDef(core_pd) = db.hir_node(core_item)? else {
            panic!("core item is not a parser def");
        };
        items.push(core_pd.id);
    }
    Ok(CoreItems {
        items: Arc::new(items),
    })
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
            pub fn kind(&self) -> HirNodeKind {
                match self {
                    $(HirNode::$variant(_) => HirNodeKind::$variant),*
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
        Block(Block),
        Import(Import),
        ArgDef(ArgDef),
        Choice(StructChoice),
        Module(Module),
        Lambda(Lambda),
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
    type BlockId = Block(Block);
    type ImportId = Import(Import);
    type ArgDefId = ArgDef(ArgDef);
    type LambdaId = Lambda(Lambda);
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
    /// Makes a new wrapped value from a DefId without checking if the DefId is valid.
    /// # Safety
    /// The DefId must be valid, otherwise this can potentially cause UB.
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
    pub imports: BTreeMap<Identifier, ImportId>,
}

impl Module {
    pub fn children(&self) -> Vec<DefId> {
        self.defs
            .values()
            .map(|x| x.0)
            .chain(self.imports.values().map(|x| x.0))
            .collect()
    }
}

fn module_file(db: &dyn Hirs, file: FileId) -> Result<Module, SilencedError> {
    let id = ModuleId(db.intern_hir_path(DefinitionPath::Module(file)));
    let symbols = db.symbols(file)?;
    let defs = symbols
        .iter()
        .flat_map(|(sym, is_parserdef)| {
            let path = db.intern_hir_path(DefinitionPath::Path(
                PathComponent::Named(FieldName::Ident(*sym)),
                id.0,
            ));
            is_parserdef.then(|| (*sym, ParserDefId(path)))
        })
        .collect();
    let imports = symbols
        .iter()
        .flat_map(|(sym, is_parserdef)| {
            let path = db.intern_hir_path(DefinitionPath::Path(
                PathComponent::Named(FieldName::Ident(*sym)),
                id.0,
            ));
            (!*is_parserdef).then_some((*sym, ImportId(path)))
        })
        .collect();
    Ok(Module { id, defs, imports })
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct HirConstraintExpressionRoot {
    pub expr: IdxExpression<HirConstraint>,
    pub data: ShapedData<Vec<SpanIndex>, HirConstraint>,
    pub has_no_eof: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HirConstraint;

impl ExprKind for HirConstraint {
    type NiladicOp = ConstraintAtom;
    type MonadicOp = ConstraintUnOp;
    type DyadicOp = ConstraintBinOp;
    type VariadicOp = Unused;
}

pub type HirConstraintSpanned = expr::KindWithData<HirConstraint, SpanIndex>;

pub type ConstraintExpression = Expression<HirConstraintSpanned>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirVal;

impl ExprKind for HirVal {
    type NiladicOp = ParserAtom;
    type MonadicOp = expr::ValUnOp<HirConstraintId>;
    type DyadicOp = expr::ValBinOp;
    type VariadicOp = expr::ValVarOp;
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HirType;

impl ExprKind for HirType {
    type NiladicOp = TypeAtom;
    type MonadicOp = expr::TypeUnOp<HirConstraintId>;
    type DyadicOp = expr::TypeBinOp;
    type VariadicOp = expr::TypeVarOp;
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ValExpression {
    pub id: ExprId,
    pub expr: DataExpr<HirVal, SpanIndex>,
    pub children: Vec<DefId>,
    pub parent_context: Option<ContextId>,
}

impl ValExpression {
    fn children(&self) -> Vec<DefId> {
        self.children.clone()
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Import {
    pub id: ImportId,
    pub name: Identifier,
    pub mod_ref: ModuleId,
}

impl Import {
    fn children(&self) -> Vec<DefId> {
        vec![]
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct TypeExpression {
    pub id: TExprId,
    pub expr: DataExpr<HirType, SpanIndex>,
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
pub struct ArgDef {
    pub id: ArgDefId,
    pub name: Identifier,
    pub ty: Option<TExprId>,
}

impl ArgDef {
    fn children(&self) -> Vec<DefId> {
        self.ty.into_iter().map(|x| x.0).collect()
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Lambda {
    pub id: LambdaId,
    pub enclosing_expr: ExprId,
    pub expr: ExprId,
    pub args: Vec<ArgDefId>,
}

impl Lambda {
    fn children(&self) -> Vec<DefId> {
        let mut children = vec![self.expr.0];
        children.extend(self.args.iter().map(|x| x.0));
        children
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum DefKind {
    Fun,
    Def,
    Static,
}

impl DefKind {
    pub fn thunky(self) -> bool {
        matches!(self, DefKind::Def)
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDef {
    pub id: ParserDefId,
    pub qualifier: Qualifier,
    pub kind: DefKind,
    pub from: Option<TExprId>,
    pub generics: Option<Vec<Identifier>>,
    pub args: Option<Vec<ArgDefId>>,
    pub to: ExprId,
    pub ret_ty: Option<TExprId>,
}

impl ParserDef {
    pub fn children(&self) -> Vec<DefId> {
        let mut child: Vec<DefId> = self.from.into_iter().map(|x| x.0).collect();
        child.push(self.to.0);
        if let Some(args) = &self.args {
            child.extend(args.iter().map(|x| x.0));
        }
        if let Some(ret_ty) = self.ret_ty {
            child.push(ret_ty.0);
        }
        child
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
struct ParentInfo {
    parent_choice: Option<ChoiceId>,
    parent_context: Option<ContextId>,
    block_id: BlockId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BlockKind {
    Parser,
    Inline,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BlockReturnKind {
    Fields,
    Returns,
    Nothing,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Block {
    pub id: BlockId,
    pub root_context: ContextId,
    pub super_context: Option<ContextId>,
    pub enclosing_expr: ExprId,
    pub returns: BlockReturnKind,
    pub kind: BlockKind,
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
    pub endpoints: Option<[ParserPredecessor; 2]>,
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
    Placeholder,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum TypePrimitive {
    Int,
    Bit,
    Char,
    U8,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefRef {
    pub path: Vec<IndexSpanned<Identifier>>,
    pub args: Vec<DataExpr<HirType, SpanIndex>>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ParserAtom {
    Atom(Atom),
    Single,
    ArrayFill,
    Span(FieldName, FieldName),
    Regex(Regex),
    String(String),
    Block(BlockId, BlockKind),
    Lambda(LambdaId),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct TypeArray {
    pub direction: ArrayKind,
    pub expr: DataExpr<HirType, SpanIndex>,
}

/// extension trait for Contexts to get a parser by name for testing
pub trait Parser {
    fn parser(&self, s: &str) -> ParserDefId;
}

impl<DB: Hirs + Default> Parser for Context<DB> {
    fn parser(&self, s: &str) -> ParserDefId {
        use yaboc_ast::import::Import;
        let fd = FileId::default();
        let module = self.db.intern_hir_path(DefinitionPath::Module(fd));
        let pd = self.db.intern_hir_path(DefinitionPath::Path(
            PathComponent::Named(FieldName::Ident(self.id(s))),
            module,
        ));
        ParserDefId(pd)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use yaboc_ast::{import::Import, AstDatabase};
    use yaboc_base::{config::ConfigDatabase, interner::InternerDatabase, source::FileDatabase};
    #[salsa::database(
        InternerDatabase,
        ConfigDatabase,
        AstDatabase,
        FileDatabase,
        HirDatabase
    )]
    #[derive(Default)]
    pub struct HirTestDatabase {
        storage: salsa::Storage<HirTestDatabase>,
    }

    impl salsa::Database for HirTestDatabase {}
    #[test]
    fn eval_test() {
        let ctx = <Context<HirTestDatabase> as Import>::mock(
            r#"
def [u8] ~> expr1 = {
  | a: u64
    b: u32
  | a: u64,
}
        "#,
        );
        eprintln!("{:?}", ctx.db.all_def_ids());
    }
}
