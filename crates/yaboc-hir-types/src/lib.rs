pub mod error;
mod full;
pub mod id_cursor;
mod public;
mod returns;
mod signature;

use std::{collections::BTreeMap, fmt::Debug, hash::Hash, rc::Rc, sync::Arc};

use bumpalo::Bump;
use fxhash::FxHashMap;

use hir::{BlockKind, DefKind, HirConstraint};
use resolve::expr::Resolved;
use yaboc_ast::expr::{self, Atom, TypeBinOp, TypeUnOp};
use yaboc_ast::{ArrayKind, ConstraintAtom};
use yaboc_base::{
    error::{IsSilenced, SResult, Silencable, SilencedError},
    interner::{DefId, FieldName, TypeVar, TypeVarName},
    source::{IndirectSpan, SpanIndex},
};
use yaboc_expr::{
    DataRefExpr, ExprHead, ExprIdx, Expression as NewExpression, IdxExpression, IndexExpr,
    ShapedData, TakeRef,
};
use yaboc_hir::{
    self as hir, walk::ChildIter, ExprId, HirIdWrapper, HirNodeKind, ParseStatement, ParserDefRef,
};
use yaboc_resolve::{
    self as resolve,
    expr::{ResolvedAtom, ValBinOp, ValUnOp, ValVarOp},
};
use yaboc_types::inference::Application;
use yaboc_types::{
    inference::{
        InfTypeId, InfTypeInterner, InferenceContext, InferenceType, NominalInfHead, TypeResolver,
    },
    EitherType, NominalKind, NominalTypeHead, PrimitiveType, Signature, Type, TypeError, TypeId,
};

use yaboc_hir::{self, Hirs};

pub struct FullTypeId;
pub struct PubTypeId;

use full::{parser_expr_at, parser_full_types, parser_type_at, ParserFullTypes};
use public::{ambient_type, public_expr_type, public_type};
pub use returns::DerefLevel;
use returns::{
    deref_level, deref_type, least_deref_type, normalize_head, parser_returns, parser_returns_ssc,
    ParserDefType,
};
pub use returns::{NOBACKTRACK_BIT, VTABLE_BIT};
use signature::{fun_arg_count, get_signature, parser_args, parser_args_error};

#[salsa::query_group(HirTypesDatabase)]
pub trait TyHirs: Hirs + yaboc_types::TypeInterner + resolve::Resolves {
    fn parser_args(&self, id: hir::ParserDefId) -> SResult<Signature>;
    fn parser_args_error(&self, id: hir::ParserDefId) -> Result<Signature, SpannedTypeError>;
    fn parser_returns(&self, id: hir::ParserDefId) -> SResult<ParserDefType>;
    fn parser_returns_ssc(
        &self,
        id: resolve::parserdef_ssc::FunctionSscId,
    ) -> Vec<Result<ParserDefType, SpannedTypeError>>;
    fn deref_type(&self, ty: TypeId) -> SResult<Option<TypeId>>;
    fn deref_level(&self, ty: TypeId) -> SResult<DerefLevel>;
    fn normalize_head(&self, ty: TypeId) -> SResult<TypeId>;
    fn least_deref_type(&self, ty: TypeId) -> SResult<TypeId>;
    fn fun_arg_count(&self, ty: TypeId) -> SResult<Option<u32>>;
    fn public_type(&self, loc: DefId) -> SResult<TypeId>;
    fn parser_type_at(&self, loc: DefId) -> SResult<TypeId>;
    fn parser_expr_at(&self, loc: hir::ExprId) -> SResult<Arc<ExprTypeData>>;
    fn public_expr_type(&self, loc: hir::ExprId) -> SResult<(Arc<ExprTypeData>, TypeId)>;
    fn ambient_type(&self, id: DefId) -> SResult<Option<TypeId>>;
    fn parser_full_types(
        &self,
        id: hir::ParserDefId,
    ) -> Result<Arc<ParserFullTypes>, SpannedTypeError>;
    fn head_discriminant(&self, ty: TypeId) -> i64;
}

#[derive(Clone, Copy)]
pub enum HeadDiscriminant {
    Int = 0x100,
    Bit = 0x200,
    Char = 0x300,
    Loop = 0x400,
    Parser = 0x500,
    FunctionArgs = 0x600,
    Block = 0x700,
    Unit = 0x800,
    U8 = 0x900,
}

pub const DISCRIMINANT_MASK: i64 = !0xff;

pub fn head_discriminant(db: &dyn TyHirs, ty: TypeId) -> i64 {
    match db.lookup_intern_type(ty) {
        Type::Primitive(PrimitiveType::Int) => HeadDiscriminant::Int as i64,
        Type::Primitive(PrimitiveType::Bit) => HeadDiscriminant::Bit as i64,
        Type::Primitive(PrimitiveType::Char) => HeadDiscriminant::Char as i64,
        Type::Primitive(PrimitiveType::Unit) => HeadDiscriminant::Unit as i64,
        Type::Primitive(PrimitiveType::U8) => HeadDiscriminant::U8 as i64,
        Type::Loop(_, _) => HeadDiscriminant::Loop as i64,
        Type::ParserArg { .. } => HeadDiscriminant::Parser as i64,
        Type::FunctionArg(_, _) => HeadDiscriminant::FunctionArgs as i64,
        Type::Nominal(NominalTypeHead {
            kind: NominalKind::Block,
            ..
        }) => HeadDiscriminant::Block as i64,
        Type::Nominal(NominalTypeHead {
            kind: NominalKind::Def | NominalKind::Fun | NominalKind::Static,
            def,
            ..
        }) => {
            let def_hash: [u8; 8] = db.def_hash(def)[0..8].try_into().unwrap();
            // highest bit is set for nominal types (so that it is negative)
            // and the rest is derived from the first 8 bytes of the hash
            //
            // the lowest eight bits are for flags so they get zeroed out
            i64::from_le_bytes(def_hash) & DISCRIMINANT_MASK | i64::MIN
        }
        Type::TypeVarRef(_, _) | Type::Any | Type::Bot | Type::Unknown => 0,
    }
}
pub type ExprTypeData = ShapedData<Vec<TypeId>, Resolved>;
pub type ExprInfTypeData<'a> = ShapedData<Vec<InfTypeId<'a>>, Resolved>;

pub struct TypingContext<'a, 'intern, TR: TypeResolver<'intern>> {
    db: &'a dyn TyHirs,
    infctx: InferenceContext<'intern, TR>,
    loc: TypingLocation,
    inftypes: Rc<FxHashMap<DefId, InfTypeId<'intern>>>,
    inf_expressions: FxHashMap<hir::ExprId, Rc<ExprInfTypeData<'intern>>>,
    current_ambient: Option<InfTypeId<'intern>>,
    recurse_blocks: bool,
}

impl<'a, 'intern, TR: TypeResolver<'intern>> TypingContext<'a, 'intern, TR> {
    fn new(
        db: &'a dyn TyHirs,
        tr: TR,
        loc: TypingLocation,
        bump: &'intern Bump,
        recurse_blocks: bool,
    ) -> Self {
        Self {
            db,
            infctx: InferenceContext::new(tr, bump),
            loc,
            inftypes: Default::default(),
            inf_expressions: Default::default(),
            current_ambient: None,
            recurse_blocks,
        }
    }

    pub fn resolve_type_expr(
        &mut self,
        expr: DataRefExpr<hir::HirType, SpanIndex>,
        id: hir::TExprId,
    ) -> Result<InfTypeId<'intern>, SpannedTypeError> {
        let root = expr.root();
        self.resolve_type_expr_impl(expr, root, id)
    }

    fn resolve_type_expr_impl(
        &mut self,
        expr: DataRefExpr<hir::HirType, SpanIndex>,
        idx: ExprIdx<hir::HirType>,
        id: hir::TExprId,
    ) -> Result<InfTypeId<'intern>, SpannedTypeError> {
        let span = IndirectSpan::new(id.0, *expr.data.index_expr(idx));
        let ret = match &expr.expr[idx] {
            ExprHead::Dyadic(TypeBinOp::Ref, _) => unimplemented!(),
            ExprHead::Dyadic(TypeBinOp::ParseArg, [lhs, rhs]) => {
                let from = self.resolve_type_expr_impl(expr, *lhs, id)?;
                let inner = match &expr.expr[*rhs] {
                    ExprHead::Niladic(hir::TypeAtom::ParserDef(pd)) => {
                        self.resolve_type_expr_parserdef_ref(pd, Some(from), span, id)?
                    }
                    _ => self.resolve_type_expr_impl(expr, *rhs, id)?,
                };
                self.infctx.parser(inner, from)
            }
            ExprHead::Monadic(TypeUnOp::Wiggle(_), inner) => {
                self.resolve_type_expr_impl(expr, *inner, id)?
            }
            ExprHead::Monadic(TypeUnOp::ByteParser, inner) => {
                let u8 = self.infctx.u8();
                let from = self.infctx.array(ArrayKind::Each, u8);
                let inner = match &expr.expr[*inner] {
                    ExprHead::Niladic(hir::TypeAtom::ParserDef(pd)) => {
                        self.resolve_type_expr_parserdef_ref(pd, Some(from), span, id)?
                    }
                    _ => self.resolve_type_expr_impl(expr, *inner, id)?,
                };
                self.infctx.parser(inner, from)
            }
            ExprHead::Niladic(hir::TypeAtom::Primitive(p)) => match p {
                hir::TypePrimitive::Int => self.infctx.int(),
                hir::TypePrimitive::Bit => self.infctx.bit(),
                hir::TypePrimitive::Char => self.infctx.char(),
                hir::TypePrimitive::U8 => self.infctx.u8(),
            },
            ExprHead::Niladic(hir::TypeAtom::ParserDef(pd)) => {
                self.resolve_type_expr_parserdef_ref(pd, None, span, id)?
            }
            ExprHead::Niladic(hir::TypeAtom::Array(a)) => {
                let inner = self.resolve_type_expr(a.expr.take_ref(), id)?;
                self.infctx
                    .intern_infty(InferenceType::Loop(a.direction, inner))
            }
            ExprHead::Niladic(hir::TypeAtom::TypeVar(v)) => {
                let var_idx =
                    self.loc.vars.get_var(*v).ok_or_else(|| {
                        SpannedTypeError::new(TypeError::UnknownTypeVar(*v), span)
                    })?;
                self.infctx
                    .intern_infty(InferenceType::TypeVarRef(self.loc.pd.0, var_idx))
            }
            ExprHead::Variadic(expr::TypeVarOp::Call, inner) => {
                let mut inner_ty = Vec::with_capacity(inner.len());
                for arg in inner {
                    inner_ty.push(self.resolve_type_expr_impl(expr, *arg, id)?);
                }
                let args = self.infctx.intern_infty_slice(&inner_ty[1..]);
                let result = inner_ty[0];
                self.infctx.function(result, args, Application::Full)
            }
        };
        Ok(ret)
    }
    fn resolve_type_expr_parserdef_ref(
        &mut self,
        pd: &ParserDefRef,
        parserarg_from: Option<InfTypeId<'intern>>,
        span: IndirectSpan,
        id: hir::TExprId,
    ) -> Result<InfTypeId<'intern>, SpannedTypeError> {
        if let [name] = &*pd.path {
            let name = self.db.lookup_intern_identifier(name.atom);
            // we don't want u8 to be a keyword, but the implementation is actually a fun and not
            // a def, so we cannot refer to it by parserdef type (and it would
            // result in an infinite loop anyway)
            // in non-type contexts, we just refer to the fun u8, while in type contexts, we mean
            // the primitive type u8
            if name.name == "u8" {
                return Ok(self.infctx.u8());
            }
        }
        let def = self
            .db
            .parserdef_ref(self.loc.loc, pd.path.iter().map(|x| x.atom).collect())?
            .ok_or_else(|| {
                SpannedTypeError::new(
                    TypeError::UnknownParserdefName(pd.path.last().unwrap().atom),
                    span,
                )
            })?;
        let parserdef = def.lookup(self.db)?;
        if !parserdef.kind.thunky() {
            return Err(SpannedTypeError::new(
                TypeError::NonThunkReference(pd.path.last().unwrap().atom),
                span,
            ));
        }
        let definition = self.db.parser_args(def)?;
        let mut parse_arg = pd
            .from
            .as_ref()
            .map(|x| self.resolve_type_expr(x.take_ref(), id))
            .transpose()?
            .or(parserarg_from);
        let mut fun_args = pd
            .args
            .iter()
            .map(|x| self.resolve_type_expr(x.take_ref(), id))
            .collect::<Result<Vec<_>, _>>()?;
        match (parse_arg, definition.from) {
            (None, Some(_)) => parse_arg = Some(self.infctx.var()),
            (Some(_), None) => {
                return Err(SpannedTypeError::new(TypeError::ParseDefFromMismatch, span))
            }
            _ => {}
        }
        let def_args_len = definition.args.as_ref().map(|x| x.len()).unwrap_or(0);
        if fun_args.len() > def_args_len {
            return Err(SpannedTypeError::new(
                TypeError::ParserDefArgCountMismatch(def_args_len, fun_args.len()),
                span,
            ));
        }
        while fun_args.len() < def_args_len {
            fun_args.push(self.infctx.var());
        }
        let ty_args = (0..definition.ty_args.len())
            .map(|_| self.infctx.var())
            .collect::<Vec<InfTypeId>>();
        let def_type = self.db.intern_type(Type::Nominal(NominalTypeHead {
            kind: NominalKind::Def,
            def: def.0,
            parse_arg: definition.from,
            fun_args: definition.args.unwrap_or_default(),
            ty_args: Arc::new(n_type_vars(self.db, def, definition.ty_args.len() as u32)),
        }));
        let inferred_def = self
            .infctx
            .convert_type_into_inftype_with_args(def_type, &ty_args);
        let fun_args = self.infctx.slice_interner.intern_slice(&fun_args);
        let ty_args = self.infctx.slice_interner.intern_slice(&ty_args);
        let ret = self
            .infctx
            .intern_infty(InferenceType::Nominal(NominalInfHead {
                kind: NominalKind::Def,
                def: def.0,
                parse_arg,
                fun_args,
                ty_args,
                internal: false,
            }));
        self.infctx
            .equal(ret, inferred_def)
            .map_err(|e| SpannedTypeError::new(e, span))?;
        Ok(ret)
    }
    fn constr_expression_type(
        &mut self,
        expr: &IdxExpression<HirConstraint>,
        ty: InfTypeId<'intern>,
    ) -> Result<(), TypeError> {
        expr.take_ref().try_for_each(|expr| match expr {
            ExprHead::Niladic(f) => match f {
                ConstraintAtom::Atom(Atom::Number(_)) | ConstraintAtom::Range(_, _) => {
                    let int = self.infctx.int();
                    self.infctx.constrain(ty, int)
                }
                ConstraintAtom::Atom(Atom::Char(_)) => {
                    let char = self.infctx.char();
                    self.infctx.constrain(ty, char)
                }
                ConstraintAtom::Atom(Atom::Bool(_)) => {
                    let bool = self.infctx.bit();
                    self.infctx.constrain(ty, bool)
                }
                ConstraintAtom::Atom(Atom::Field(name)) => {
                    self.infctx.access_field(ty, name).map(|_| ())
                }
                ConstraintAtom::NotEof => Ok(()),
            },
            _ => Ok(()),
        })
    }

    fn infer_block(
        &mut self,
        b: hir::BlockId,
        kind: BlockKind,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let block = b.lookup(self.db)?;
        Ok(if block.returns {
            let to_id = block.root_context.0.child_field(self.db, FieldName::Return);
            let result = self.inftypes[&to_id];
            match kind {
                BlockKind::Parser => {
                    let arg = self.infctx.var();
                    self.infctx.parser(result, arg)
                }
                BlockKind::Inline => self.infctx.zero_arg_function(result),
            }
        } else {
            let pd = self.db.hir_parent_parserdef(b.0)?;
            let ty_vars = (0..self.loc.vars.defs.len() as u32)
                .map(|i| self.infctx.intern_infty(InferenceType::TypeVarRef(pd.0, i)))
                .collect::<Vec<_>>();
            self.infctx
                .block(b.0, kind == BlockKind::Parser, &ty_vars)?
        })
    }
    pub fn val_expression_type(
        &mut self,
        expr: &resolve::ResolvedExpr,
        id: hir::ExprId,
    ) -> Result<Rc<ExprInfTypeData<'intern>>, SpannedTypeError> {
        use ValBinOp::*;
        let expr_ref = expr.take_ref();
        let span = IndirectSpan::new(id.0, expr.data[expr_ref.root()]);
        let expr: ExprInfTypeData<'intern> = expr
            .take_ref()
            .try_scan(|(expr, _)| {
                Ok(match expr {
                    ExprHead::Dyadic(op, [&left, &right]) => match op {
                        And | Xor | Or | ShiftR | ShiftL | Minus | Plus | Div | Modulo | Mul => {
                            let int = self.infctx.int();
                            self.infctx.constrain(left, int)?;
                            self.infctx.constrain(right, int)?;
                            int
                        }
                        LesserEq | Lesser | GreaterEq | Greater | Uneq | Equals => {
                            let bit = self.infctx.bit();
                            let int = self.infctx.int();
                            self.infctx.constrain(left, int)?;
                            self.infctx.constrain(right, int)?;
                            bit
                        }
                        Else => self.infctx.one_of(&[left, right])?,
                        Then => right,
                        ParserApply => self.infctx.parser_apply(right, left)?,
                    },
                    ExprHead::Monadic(op, &inner) => match &op {
                        ValUnOp::Neg | ValUnOp::Not => {
                            let int = self.infctx.int();
                            self.infctx.constrain(inner, int)?;
                            int
                        }
                        ValUnOp::Size => self.infctx.check_size_of(inner)?,
                        ValUnOp::Wiggle(c, _) => {
                            let (inner, cont) = self.infctx.if_checked(inner)?;
                            let expr = self.db.lookup_intern_hir_constraint(*c);
                            self.constr_expression_type(&expr.expr, inner)?;
                            if expr.has_no_eof {
                                self.infctx.check_parser(cont)?;
                            }
                            cont
                        }
                        // TODO: we should make a variable that enforces dereference level 0 here
                        // so that backtracking can be toggled
                        ValUnOp::BtMark(_) => inner,
                        ValUnOp::Dot(name, _) => self.infctx.access_field(inner, *name)?,
                        ValUnOp::EvalFun => {
                            self.infctx.function_apply(inner, &[], Application::Full)?
                        }
                        ValUnOp::GetAddr => {
                            let int = self.infctx.int();
                            self.infctx.constrain(inner, int)?;
                            let u8 = self.infctx.u8();
                            self.infctx.array(ArrayKind::Each, u8)
                        }
                    },
                    ExprHead::Niladic(a) => match &a {
                        ResolvedAtom::Char(_) => self.infctx.char(),
                        ResolvedAtom::Number(_) => self.infctx.int(),
                        ResolvedAtom::Bool(_) => self.infctx.bit(),
                        ResolvedAtom::Single => self.infctx.single(),
                        ResolvedAtom::Nil => self.infctx.nil(),
                        ResolvedAtom::Array => self.infctx.array_parser(),
                        ResolvedAtom::ArrayFill => self.infctx.array_fill_parser(),
                        // the ambient type can only be none if we had another error before it,
                        // since we are necessarily referencing local parse statements for it
                        // to resolve correctly
                        ResolvedAtom::Span(..) => self.ambient_type().ok_or(SilencedError::new())?,
                        ResolvedAtom::Regex(..) => self.infctx.regex(),
                        ResolvedAtom::Block(b, kind) => self.infer_block(*b, *kind)?,
                        ResolvedAtom::ParserDef(pd) | ResolvedAtom::Global(pd) => {
                            self.infctx.parserdef(pd.0)?
                        }
                        ResolvedAtom::Val(v) | ResolvedAtom::Captured(v) => {
                            self.infctx.lookup(*v)?
                        }
                    },
                    ExprHead::Variadic(ValVarOp::PartialApply(_), inner) => {
                        self.infctx.function_apply(
                            *inner[0],
                            &inner[1..].iter().copied().copied().collect::<Vec<_>>(),
                            Application::Partial,
                        )?
                    }
                })
            })
            .map_err(|x| SpannedTypeError::new(x, span))?;
        let rc_expr = Rc::new(expr);
        self.inf_expressions.insert(id, rc_expr.clone());
        Ok(rc_expr)
    }
    fn inftype_to_concrete_type(&mut self, infty: InfTypeId<'intern>) -> Result<TypeId, TypeError> {
        self.infctx.to_type(infty)
    }
    fn expr_to_concrete_type(
        &mut self,
        expr: &ExprInfTypeData<'intern>,
        id: ExprId,
    ) -> Result<ExprTypeData, SpannedTypeError> {
        let spans = self.db.resolve_expr(id)?;
        expr.as_slice()
            .zip(spans.data.as_slice())
            .map(|(ty, span)| {
                self.inftype_to_concrete_type(*ty)
                    .map_err(|e| SpannedTypeError::new(e, IndirectSpan::new(id.0, *span)))
            })
            .try_collect()
    }
    fn set_current_loc(&mut self, loc: DefId) {
        // sets loc to loc
        self.loc.loc = loc;
    }
    fn let_statement_types(
        &mut self,
        let_statement: &hir::LetStatement,
    ) -> Result<Option<InfTypeId<'intern>>, SpannedTypeError> {
        if let Some(ty) = let_statement.ty {
            let ty_expr = ty.lookup(self.db)?.expr;
            let infty = self.resolve_type_expr(ty_expr.take_ref(), ty)?;
            Ok(Some(infty))
        } else {
            Ok(None)
        }
    }
    fn initialize_vars_at(
        &mut self,
        root: DefId,
        vars: &mut FxHashMap<DefId, InfTypeId<'intern>>,
    ) -> Result<(), SpannedTypeError> {
        for child in ChildIter::new(root, self.db).without_kinds(HirNodeKind::Block) {
            let id = child.id();
            let ty = match child {
                hir::HirNode::Let(l) => {
                    self.set_current_loc(id);
                    if let Some(ty) = self.let_statement_types(&l)? {
                        ty
                    } else {
                        self.infctx.var()
                    }
                }
                hir::HirNode::Parse(_) | hir::HirNode::ChoiceIndirection(_) => {
                    self.set_current_loc(id);
                    self.infctx.var()
                }
                hir::HirNode::ParserDef(pd) => {
                    self.set_current_loc(id);
                    if let Some(texpr) = pd.ret_ty {
                        let texpr = texpr.lookup(self.db)?;
                        self.resolve_type_expr(texpr.expr.take_ref(), texpr.id)?
                    } else {
                        self.infctx.var()
                    }
                }
                hir::HirNode::Block(block) => {
                    if block.returns {
                        self.initialize_vars_at(block.root_context.0, vars)?;
                    }
                    continue;
                }
                _ => continue,
            };
            vars.insert(id, ty);
        }
        Ok(())
    }
    fn initialize_parserdef_args(
        &mut self,
        pd: hir::ParserDefId,
        vars: &mut FxHashMap<DefId, InfTypeId<'intern>>,
    ) -> Result<(), SpannedTypeError> {
        let pd = pd.lookup(self.db)?;
        let sig = self.db.parser_args(pd.id)?;
        for (ty, id) in sig
            .args
            .iter()
            .flat_map(|x| x.iter())
            .zip(pd.args.into_iter().flatten())
        {
            let ty = self.infctx.convert_type_into_inftype(*ty);
            vars.insert(id.0, ty);
        }
        Ok(())
    }
    fn set_ambient_type(&mut self, infty: Option<InfTypeId<'intern>>) {
        self.current_ambient = infty;
    }
    fn ambient_type(&self) -> Option<InfTypeId<'intern>> {
        self.current_ambient
    }
    fn infty_at(&mut self, id: DefId) -> InfTypeId<'intern> {
        self.inftypes[&id]
    }
    fn with_ambient_type<T>(
        &mut self,
        infty: Option<InfTypeId<'intern>>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let old_ambient = self.ambient_type();
        self.set_ambient_type(infty);
        let ret = f(self);
        self.set_ambient_type(old_ambient);
        ret
    }
    fn with_loc<T>(&mut self, loc: DefId, f: impl FnOnce(&mut Self) -> T) -> T {
        let old_loc = self.loc.loc;
        self.loc.loc = loc;
        let ret = f(self);
        self.loc.loc = old_loc;
        ret
    }
    fn type_parserdef(&mut self, pd: hir::ParserDefId) -> Result<(), SpannedTypeError> {
        let parserdef = pd.lookup(self.db)?;
        let sig = self.db.parser_args(pd)?;
        let ambient = sig.from.map(|ty| self.infctx.convert_type_into_inftype(ty));
        self.set_ambient_type(ambient);

        let expr = parserdef.to.lookup(self.db)?;
        let mut ret = self.type_expr(&expr)?;
        let return_spanned = |e| SpannedTypeError::new(e, IndirectSpan::default_span(pd.0));
        if let Some(am) = ambient {
            ret = self.infctx.parser_apply(ret, am).map_err(return_spanned)?
        }
        let previous_ret = self.infty_at(pd.0);
        self.infctx
            .constrain(ret, previous_ret)
            .map_err(return_spanned)?;
        Ok(())
    }
    fn type_expr(
        &mut self,
        expr: &hir::ValExpression,
    ) -> Result<InfTypeId<'intern>, SpannedTypeError> {
        let resolved_expr = self.db.resolve_expr(expr.id)?;
        let inf_expression = self.val_expression_type(&resolved_expr, expr.id)?;
        let ret = inf_expression[inf_expression.root()];
        for (part, (span, ty)) in resolved_expr
            .take_ref()
            .zip(inf_expression.as_ref())
            .iter_parts()
        {
            let ExprHead::Niladic(ResolvedAtom::Block(block_id, _)) = &part else {
                continue;
            };
            let block = block_id.lookup(self.db)?;
            let spanned = |e| SpannedTypeError::new(e, IndirectSpan::new(expr.id.0, *span));
            let ambient = if block.kind == BlockKind::Parser {
                Some(self.infctx.reuse_parser_arg(*ty).map_err(spanned)?)
            } else {
                None
            };
            if block.returns || self.recurse_blocks {
                self.with_ambient_type(ambient, |ctx| ctx.type_block(&block))?;
            }
        }
        Ok(ret)
    }
    fn type_block(&mut self, block: &hir::Block) -> Result<(), SpannedTypeError> {
        let root_ctx = block.root_context.lookup(self.db)?;
        self.type_context(&root_ctx)
    }
    fn type_context(&mut self, context: &hir::StructCtx) -> Result<(), SpannedTypeError> {
        self.with_loc(context.id.0, |ctx| {
            for child in context.children.iter() {
                ctx.type_block_component(*child)?;
            }
            Ok(())
        })
    }
    fn type_block_component(&mut self, id: DefId) -> Result<(), SpannedTypeError> {
        match self.db.hir_node(id)? {
            hir::HirNode::Let(l) => self.type_let(&l),
            hir::HirNode::Parse(parse) => self.type_parse(&parse),
            hir::HirNode::Choice(choice) => self.type_choice(&choice),
            hir::HirNode::Context(context) => self.type_context(&context),
            hir::HirNode::ChoiceIndirection(ci) => self.type_choice_indirection(&ci),
            _ => unreachable!("Invalid type block component"),
        }
    }
    fn type_choice(&mut self, choice: &hir::StructChoice) -> Result<(), SpannedTypeError> {
        for child in choice.subcontexts.iter() {
            let context = child.lookup(self.db)?;
            self.type_context(&context)?;
        }
        Ok(())
    }
    fn type_choice_indirection(
        &mut self,
        choice_ind: &hir::ChoiceIndirection,
    ) -> Result<(), SpannedTypeError> {
        let current = self.infty_at(choice_ind.id.0);
        for (_, choice_id) in choice_ind.choices.iter() {
            let choice = self.infty_at(*choice_id);
            self.infctx.constrain(choice, current).map_err(|e| {
                SpannedTypeError::new(e, IndirectSpan::default_span(choice_ind.id.0))
            })?;
        }
        Ok(())
    }
    fn type_parse(&mut self, parse: &ParseStatement) -> Result<(), SpannedTypeError> {
        let expr = parse.expr.lookup(self.db)?;
        let with_span = |e| SpannedTypeError::new(e, IndirectSpan::default_span(parse.id.0));
        let ty = self.type_expr(&expr)?;
        let ret = self
            .infctx
            .parser_apply(ty, self.ambient_type().unwrap())
            .map_err(with_span)?;
        let self_ty = self.infty_at(parse.id.0);
        self.infctx.constrain(ret, self_ty).map_err(with_span)
    }
    fn type_let(&mut self, let_statement: &hir::LetStatement) -> Result<(), SpannedTypeError> {
        let expr = let_statement.expr.lookup(self.db)?;
        let ty = self.type_expr(&expr)?;
        let self_ty = self.infty_at(let_statement.id.0);
        self.infctx
            .constrain(ty, self_ty)
            .map_err(|e| SpannedTypeError::new(e, IndirectSpan::default_span(let_statement.id.0)))
    }
}

#[derive(Clone, Debug)]
pub struct TypeVarCollection {
    pub defs: Vec<TypeVar>,
    names: FxHashMap<TypeVar, u32>,
    frozen: bool,
}

impl TypeVarCollection {
    pub fn new_empty() -> Self {
        TypeVarCollection {
            defs: vec![],
            names: FxHashMap::default(),
            frozen: false,
        }
    }
    pub fn get_var(&mut self, var_name: TypeVar) -> Option<u32> {
        if let Some(idx) = self.names.get(&var_name) {
            return Some(*idx);
        }
        if self.frozen {
            return None;
        }
        let new_index = self.defs.len() as u32;
        self.defs.push(var_name);
        self.names.insert(var_name, new_index);
        Some(new_index)
    }
    pub fn freeze(&mut self) {
        self.frozen = true
    }
    pub fn at_id(db: &(impl TyHirs + ?Sized), id: hir::ParserDefId) -> SResult<Self> {
        let tys = db.parser_args(id)?.ty_args;
        let mut ret = Self::new_empty();
        for ty in tys.iter() {
            ret.get_var(*ty);
        }
        ret.freeze();
        Ok(ret)
    }
    pub fn var_types(&self, db: &(impl TyHirs + ?Sized), id: hir::ParserDefId) -> Vec<TypeId> {
        n_type_vars(db, id, self.defs.len() as u32)
    }
    pub fn fill_anon_vars(&mut self, db: &(impl TyHirs + ?Sized), target_count: u32) {
        for i in 1..=(target_count - self.defs.len() as u32) {
            let nth_name = format!("'{i}");
            let tvar = db.intern_type_var(TypeVarName::new(nth_name));
            self.defs.push(tvar);
        }
    }
}

fn n_type_vars(db: &(impl TyHirs + ?Sized), id: hir::ParserDefId, n: u32) -> Vec<TypeId> {
    (0..n)
        .map(|i| db.intern_type(Type::TypeVarRef(id.0, i)))
        .collect()
}

#[derive(Clone)]
pub struct TypingLocation {
    vars: TypeVarCollection,
    loc: DefId,
    pd: hir::ParserDefId,
}

impl TypingLocation {
    pub fn at_id(db: &(impl TyHirs + ?Sized), loc: DefId) -> SResult<Self> {
        let pd = db.hir_parent_parserdef(loc)?;
        let vars = TypeVarCollection::at_id(db, pd)?;
        Ok(TypingLocation { vars, loc, pd })
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct TypeInfo<Id> {
    ty: Id,
    span_index: SpanIndex,
}

pub enum NominalId {
    Def(hir::ParserDefId),
    Block(hir::BlockId),
}

impl NominalId {
    pub fn from_nominal_inf_head(head: &NominalInfHead) -> Self {
        match head.kind {
            NominalKind::Def | NominalKind::Fun | NominalKind::Static => {
                NominalId::Def(hir::ParserDefId(head.def))
            }
            NominalKind::Block => NominalId::Block(hir::BlockId(head.def)),
        }
    }
    pub fn from_nominal_head(head: &NominalTypeHead) -> Self {
        match head.kind {
            NominalKind::Def | NominalKind::Fun | NominalKind::Static => {
                NominalId::Def(hir::ParserDefId(head.def))
            }
            NominalKind::Block => NominalId::Block(hir::BlockId(head.def)),
        }
    }
    pub fn unwrap_block(self) -> hir::BlockId {
        match self {
            NominalId::Block(b) => b,
            _ => panic!("Expected block id"),
        }
    }
    pub fn unwrap_def(self) -> hir::ParserDefId {
        match self {
            NominalId::Def(b) => b,
            _ => panic!("Expected def id"),
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum SpannedTypeError {
    Spanned(TypeError, IndirectSpan),
    Silenced(SilencedError),
}

impl SpannedTypeError {
    fn new(err: TypeError, span: IndirectSpan) -> Self {
        SpannedTypeError::Spanned(err, span)
    }
}

impl From<SilencedError> for SpannedTypeError {
    fn from(s: SilencedError) -> Self {
        SpannedTypeError::Silenced(s)
    }
}

impl Silencable for SpannedTypeError {
    type Out = SilencedError;

    fn silence(self) -> Self::Out {
        match self {
            SpannedTypeError::Spanned(inner, _) => match inner {
                TypeError::Silenced(s) => s,
                _ => SilencedError::new(),
            },
            SpannedTypeError::Silenced(s) => s,
        }
    }
}

impl IsSilenced for SpannedTypeError {
    fn is_silenced(&self) -> bool {
        matches!(
            self,
            SpannedTypeError::Silenced(_) | SpannedTypeError::Spanned(TypeError::Silenced(_), _)
        )
    }
}

#[cfg(test)]
pub mod tests {
    use yaboc_ast::AstDatabase;
    use yaboc_base::{config::ConfigDatabase, interner::InternerDatabase, source::FileDatabase};
    use yaboc_hir::HirDatabase;
    use yaboc_resolve::ResolveDatabase;
    use yaboc_types::TypeInternerDatabase;

    use crate::HirTypesDatabase;
    #[salsa::database(
        InternerDatabase,
        ConfigDatabase,
        AstDatabase,
        FileDatabase,
        HirDatabase,
        ResolveDatabase,
        TypeInternerDatabase,
        HirTypesDatabase
    )]
    #[derive(Default)]
    pub struct HirTypesTestDatabase {
        storage: salsa::Storage<HirTypesTestDatabase>,
    }

    impl salsa::Database for HirTypesTestDatabase {}
}
