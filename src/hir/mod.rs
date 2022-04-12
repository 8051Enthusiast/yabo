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
    ast::ArrayKind,
    dbpanic,
    error::{SResult, SilencedError},
    expr::{self, Atom, ExprConverter, Expression, ExpressionComponent, ExpressionKind},
    interner::{FieldName, HirId, HirPath, Identifier, PathComponent, TypeVar},
    source::{FileId, Span, Spanned},
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
    fn hir_parser_collection(&self, hid: HirId) -> SResult<Option<HirParserCollection>>;
    fn hir_node(&self, id: HirId) -> SResult<HirNode>;
    #[salsa::interned]
    fn intern_recursion_scc(&self, functions: Vec<ParserDefId>) -> recursion::FunctionSscId;
    fn mod_parser_ssc(
        &self,
        module: ModuleId,
    ) -> SResult<Arc<BTreeMap<ParserDefId, FunctionSscId>>>;
    fn parser_ssc(&self, parser: ParserDefId) -> SResult<FunctionSscId>;
    fn root_id(&self) -> ModuleId;
    fn all_hir_ids(&self) -> Vec<HirId>;
    fn all_parserdefs(&self) -> Vec<ParserDefId>;
    fn hir_parent_module(&self, id: HirId) -> SResult<ModuleId>;
    fn hir_parent_parserdef(&self, id: HirId) -> SResult<ParserDefId>;
}

fn hir_node(db: &dyn Hirs, id: HirId) -> SResult<HirNode> {
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

fn all_parserdefs(db: &dyn Hirs) -> Vec<ParserDefId> {
    let root = db.root_id();
    let module = match root.lookup(db) {
        Ok(m) => m,
        Err(_) => return Vec::new(),
    };
    let ret: Vec<_> = module.defs.values().cloned().collect();
    ret
}

fn hir_parent_module(db: &dyn Hirs, id: HirId) -> SResult<ModuleId> {
    // todo
    Ok(ModuleId(db.intern_hir_path(HirPath::new_file(
        db.lookup_intern_hir_path(id).path()[0].unwrap_file(),
    ))))
}

fn hir_parent_parserdef(db: &dyn Hirs, id: HirId) -> SResult<ParserDefId> {
    let path = db.lookup_intern_hir_path(id);
    // todo
    Ok(ParserDefId(db.intern_hir_path(HirPath::new_fid(
        path.path()[0].unwrap_file(),
        path.path()[1].unwrap_named(),
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
            pub fn children(&self) -> Vec<HirId> {
                match self {
                    $(HirNode::$variant(x) => x.children()),*
                }
            }
            pub fn is_kind(&self, kind: BitFlags<HirNodeKind>) -> bool {
                match self {
                    $(HirNode::$variant(_) => kind.contains(HirNodeKind::$variant)),*
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
    fn id(self) -> HirId;
    fn extract(node: HirNode) -> Self::Inner;
    fn child<DB: Hirs + ?Sized>(self, db: &DB, add: PathComponent) -> HirId {
        self.id().child(db, add)
    }
    fn lookup<DB: Hirs + ?Sized>(self, db: &DB) -> SResult<Self::Inner> {
        let id = self.id();
        Ok(Self::extract(db.hir_node(id)?))
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug, Default)]
pub struct HirParserCollection {
    map: BTreeMap<HirId, HirNode>,
    spans: BTreeMap<HirId, Vec<Span>>,
    errors: HirConversionErrors,
}

impl HirParserCollection {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Module {
    pub id: ModuleId,
    pub defs: BTreeMap<Identifier, ParserDefId>,
    pub submods: BTreeMap<Identifier, ModuleId>,
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
            TypeAtom::Array(a) => vec![&a.expr],
            _ => vec![],
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
    pub id: ExprId,
    pub expr: Expression<HirVal>,
    pub children: Vec<HirId>,
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
    pub id: TExprId,
    pub expr: Expression<HirType>,
}

impl TypeExpression {
    fn children(&self) -> Vec<HirId> {
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
    pub fn children(&self) -> Vec<HirId> {
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
    pub fn children(&self) -> Vec<HirId> {
        vec![self.root_context.0]
    }
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

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum TypeAtom {
    Primitive(TypePrimitive),
    ParserDef(Box<ParserDefRef>),
    Array(Box<TypeArray>),
    TypeVar(TypeVar),
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum TypePrimitive {
    Mem,
    Int,
    Bit,
    Char,
}
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct ParserDefRef {
    pub from: Option<Expression<HirType>>,
    pub name: IndexSpanned<Identifier>,
    pub args: Vec<Expression<HirType>>,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum ParserAtom {
    Atom(Atom),
    Single,
    Array(ArrayId),
    Block(BlockId),
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct TypeArray {
    pub direction: ArrayKind,
    pub expr: Expression<HirType>,
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
    pub fn children(&self) -> Vec<HirId> {
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
        eprintln!("{:?}", ctx.db.all_hir_ids());
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
