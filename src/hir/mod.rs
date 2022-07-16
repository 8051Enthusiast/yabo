mod convert;
pub mod error;
pub mod recursion;
pub mod refs;
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
    interner::{FieldName, DefId, HirPath, Identifier, PathComponent, TypeVar},
    source::{FileId, Span},
};

use crate::source::IndexSpanned;
use crate::source::SpanIndex;
use enumflags2::{bitflags, BitFlags};
use variable_set::VariableSet;

use convert::hir_parser_collection;
use recursion::{mod_parser_ssc, parser_ssc, FunctionSscId};

use self::convert::HirConversionErrors;

#[salsa::query_group(HirDatabase)]
pub trait Hirs: crate::ast::Asts + crate::types::TypeInterner {
    fn hir_parser_collection(&self, did: DefId) -> SResult<Option<HirParserCollection>>;
    fn hir_node(&self, id: DefId) -> SResult<HirNode>;
    #[salsa::interned]
    fn intern_recursion_scc(&self, functions: Vec<ParserDefId>) -> recursion::FunctionSscId;
    fn mod_parser_ssc(
        &self,
        module: ModuleId,
    ) -> SResult<Arc<BTreeMap<ParserDefId, FunctionSscId>>>;
    fn parser_ssc(&self, parser: ParserDefId) -> SResult<FunctionSscId>;
    fn root_id(&self) -> ModuleId;
    fn all_def_ids(&self) -> Vec<DefId>;
    fn all_parserdefs(&self) -> Vec<ParserDefId>;
    fn hir_parent_module(&self, id: DefId) -> SResult<ModuleId>;
    fn hir_parent_parserdef(&self, id: DefId) -> SResult<ParserDefId>;
    fn hir_parent_block(&self, id: DefId) -> SResult<Option<BlockId>>;
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

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDef {
    pub id: ParserDefId,
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
    pub array: Option<ArrayKind>,
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
    pub ty: TExprId,
    pub expr: ExprId,
    pub context: ContextId,
}

impl LetStatement {
    fn children(&self) -> Vec<DefId> {
        vec![self.ty.0, self.expr.0]
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
    (
        a: u64,
        b: u32,
    ;
        a: u64,
    )
}
        "#,
        );
        eprintln!("{:?}", ctx.db.all_def_ids());
    }
    #[test]
    fn recursion_ssc() {
        let ctx = Context::mock(
            r#"
def for [u8] *> a = {x: c, y: {b, z: d,},}
def for [u8] *> b = {x: a, y: c,}
def for [u8] *> c = {x: c,}
def for [u8] *> d = {let a: u64 = 1, let b: u64 = a + 1,}
def for [u8] *> e = {}
            "#,
        );
        let a = ctx.parser("a");
        let b = ctx.parser("b");
        let c = ctx.parser("c");
        let d = ctx.parser("d");
        let e = ctx.parser("d");
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
