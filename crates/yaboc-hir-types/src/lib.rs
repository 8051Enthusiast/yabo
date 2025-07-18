pub mod error;
//mod full;
pub mod id_cursor;
//mod public;
mod returns;
mod signature;

use std::{collections::BTreeMap, fmt::Debug, hash::Hash, rc::Rc, sync::Arc};

use bumpalo::Bump;
use fxhash::FxHashMap;

use hir::{BlockKind, DefKind, HirConstraint};
use resolve::expr::Resolved;
use resolve::parserdef_ssc::FunctionSscId;
use yaboc_ast::expr::{self, Atom, TypeBinOp, TypeUnOp};
use yaboc_ast::{ArrayKind, ConstraintAtom};
use yaboc_base::interner::Identifier;
use yaboc_base::{
    error::{IsSilenced, SResult, Silencable, SilencedError},
    interner::{DefId, FieldName},
    source::{IndirectSpan, SpanIndex},
};
use yaboc_expr::{
    DataRefExpr, ExprHead, ExprIdx, Expression as NewExpression, FetchExpr as _, IdxExpression,
    IndexExpr, ShapedData, TakeRef,
};
use yaboc_hir::{self as hir, walk::ChildIter, ExprId, HirIdWrapper, ParseStatement, ParserDefRef};
use yaboc_resolve::{
    self as resolve,
    expr::{ResolvedAtom, ValBinOp, ValUnOp, ValVarOp},
};
use yaboc_types::inference::{Application, InternedNomHead};
use yaboc_types::{
    inference::{InfTypeId, InfTypeInterner, InferenceContext, InferenceType, TypeResolver},
    EitherType, NominalKind, NominalTypeHead, PrimitiveType, Signature, Type, TypeError, TypeId,
};
use yaboc_types::{TypeConvError, TypeVarRef};

use yaboc_hir::{self, BlockReturnKind, Hirs};

pub struct FullTypeId;
pub struct PubTypeId;

pub use returns::DerefLevel;
use returns::{
    deref_level, deref_type, least_deref_type, normalize_head, parser_expr_at, parser_returns,
    parser_type_at, ssc_types, ParserDefType, SscTypes,
};
pub use returns::{NOBACKTRACK_BIT, VTABLE_BIT};
use signature::{bound_args, fun_arg_count, get_signature, parser_args, parser_args_error};

#[salsa::query_group(HirTypesDatabase)]
pub trait TyHirs: Hirs + yaboc_types::TypeInterner + resolve::Resolves {
    fn parser_args(&self, id: hir::ParserDefId) -> SResult<Signature>;
    fn parser_args_error(&self, id: hir::ParserDefId) -> Result<Signature, SpannedTypeError>;
    fn parser_returns(&self, id: hir::ParserDefId) -> SResult<ParserDefType>;
    fn ssc_types(&self, id: FunctionSscId) -> Result<SscTypes, SpannedTypeError>;
    fn deref_type(&self, ty: TypeId) -> SResult<Option<TypeId>>;
    fn deref_level(&self, ty: TypeId) -> SResult<DerefLevel>;
    fn normalize_head(&self, ty: TypeId) -> SResult<TypeId>;
    fn least_deref_type(&self, ty: TypeId) -> SResult<TypeId>;
    fn fun_arg_count(&self, ty: TypeId) -> SResult<Option<u32>>;
    fn parser_type_at(&self, loc: DefId) -> SResult<TypeId>;
    fn parser_expr_at(&self, loc: hir::ExprId) -> SResult<Arc<ExprTypeData>>;
    fn bound_args(&self, id: DefId) -> SResult<Arc<[TypeId]>>;
    fn lambda_type(&self, id: hir::LambdaId) -> SResult<TypeId>;
    fn block_type(&self, id: hir::BlockId) -> SResult<TypeId>;
    fn validate_export_arguments(&self) -> SResult<Vec<SpannedTypeError>>;
}

#[derive(Clone, Copy)]
pub enum HeadDiscriminant {
    Int = 0x100,
    Bit = 0x200,
    Char = 0x300,
    Loop = 0x400,
    SlicePtr = 0x401,
    Range = 0x402,
    Parser = 0x500,
    FunctionArgs = 0x600,
    Block = 0x700,
    Unit = 0x800,
    U8 = 0x900,
}

pub const DISCRIMINANT_MASK: i64 = !0xff;

pub fn ty_head_discriminant<DB: TyHirs + ?Sized>(db: &DB, ty: TypeId) -> i64 {
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
        Type::TypeVarRef(_) | Type::Unknown => 0,
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
}

impl<'a, 'intern, TR: TypeResolver<'intern>> TypingContext<'a, 'intern, TR> {
    fn new(db: &'a dyn TyHirs, tr: TR, loc: TypingLocation, bump: &'intern Bump) -> Self {
        Self {
            db,
            infctx: InferenceContext::new(tr, bump),
            loc,
            inftypes: Default::default(),
            inf_expressions: Default::default(),
            current_ambient: None,
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
                let inner = self.resolve_type_expr_impl(expr, *rhs, id)?;
                self.infctx.parser(inner, from)
            }
            ExprHead::Monadic(TypeUnOp::Wiggle(_), inner) => {
                self.resolve_type_expr_impl(expr, *inner, id)?
            }
            ExprHead::Monadic(TypeUnOp::ByteParser, inner) => {
                let u8 = self.infctx.u8();
                let from = self.infctx.array(ArrayKind::Each, u8);
                let inner = self.resolve_type_expr_impl(expr, *inner, id)?;
                self.infctx.parser(inner, from)
            }
            ExprHead::Niladic(hir::TypeAtom::Primitive(p)) => match p {
                hir::TypePrimitive::Int => self.infctx.int(),
                hir::TypePrimitive::Bit => self.infctx.bit(),
                hir::TypePrimitive::Char => self.infctx.char(),
                hir::TypePrimitive::U8 => self.infctx.u8(),
            },
            ExprHead::Niladic(hir::TypeAtom::ParserDef(pd)) => {
                self.resolve_type_expr_parserdef_ref(pd, span, id)?
            }
            ExprHead::Niladic(hir::TypeAtom::Array(a)) => {
                let inner = self.resolve_type_expr(a.expr.take_ref(), id)?;
                self.infctx
                    .intern_infty(InferenceType::Loop(a.direction, inner))
            }
            ExprHead::Niladic(hir::TypeAtom::Placeholder) => self.infctx.var(),
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
        span: IndirectSpan,
        id: hir::TExprId,
    ) -> Result<InfTypeId<'intern>, SpannedTypeError> {
        if let [name] = &*pd.path {
            let real_name = self.db.lookup_intern_identifier(name.atom);
            // we don't want u8 to be a keyword, but the implementation is actually a fun and not
            // a def, so we cannot refer to it by parserdef type (and it would
            // result in an infinite loop anyway)
            // in non-type contexts, we just refer to the fun u8, while in type contexts, we mean
            // the primitive type u8
            if real_name.name == "u8" {
                return Ok(self.infctx.u8());
            }

            if let Some(arg) = self.loc.vars.get_var(name.atom) {
                return Ok(self
                    .infctx
                    .intern_infty(InferenceType::TypeVarRef(TypeVarRef(self.loc.pd.0, arg))));
            }
        }
        let def = self
            .db
            .parserdef_ref(self.loc.loc, pd.path.iter().map(|x| x.atom).collect())?
            .ok_or_else(|| {
                SpannedTypeError::new(TypeError::UnknownName(pd.path.last().unwrap().atom), span)
            })?;
        let parserdef = def.lookup(self.db)?;
        if !parserdef.kind.thunky() {
            return Err(SpannedTypeError::new(
                TypeError::NonThunkReference(pd.path.last().unwrap().atom),
                span,
            ));
        }
        let definition = self.db.parser_args(def)?;
        let mut generic_args = pd
            .args
            .iter()
            .map(|x| self.resolve_type_expr(x.take_ref(), id))
            .collect::<Result<Vec<_>, _>>()?;
        let ty_args_len = definition.ty_args.len();
        if generic_args.len() > ty_args_len {
            return Err(SpannedTypeError::new(
                TypeError::ParserDefArgCountMismatch(ty_args_len, generic_args.len()),
                span,
            ));
        }
        while generic_args.len() < ty_args_len {
            generic_args.push(self.infctx.var());
        }
        let def_type = self.db.intern_type(Type::Nominal(definition.thunk));
        let inferred_def = self
            .infctx
            .convert_type_into_inftype_with_args(def_type, &generic_args);
        Ok(inferred_def)
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

    fn wrap_block_return(
        &mut self,
        kind: BlockKind,
        result: InfTypeId<'intern>,
    ) -> InfTypeId<'intern> {
        match kind {
            BlockKind::Parser => {
                let arg = self.infctx.var();
                self.infctx.parser(result, arg)
            }
            BlockKind::Inline => self.infctx.zero_arg_function(result),
        }
    }

    fn infer_block(
        &mut self,
        b: hir::BlockId,
        kind: BlockKind,
    ) -> Result<InfTypeId<'intern>, TypeError> {
        let block = b.lookup(self.db)?;
        Ok(match block.returns {
            BlockReturnKind::Returns => {
                let to_id = block.root_context.0.child_field(self.db, FieldName::Return);
                let result = self.inftypes[&to_id];
                self.wrap_block_return(kind, result)
            }
            BlockReturnKind::Fields => {
                let pd = self.db.hir_parent_parserdef(b.0)?;
                let ty_vars = (0..self.loc.vars.defs.len() as u32)
                    .map(|i| {
                        self.infctx
                            .intern_infty(InferenceType::TypeVarRef(TypeVarRef(pd.0, i)))
                    })
                    .collect::<Vec<_>>();
                self.infctx
                    .block(b.0, kind == BlockKind::Parser, &ty_vars)?
            }
            BlockReturnKind::Nothing => {
                let result = self
                    .infctx
                    .intern_infty(InferenceType::Primitive(PrimitiveType::Unit));
                self.wrap_block_return(kind, result)
            }
        })
    }

    pub fn val_expression_type(
        &mut self,
        expr: &resolve::ResolvedExpr,
        id: hir::ExprId,
    ) -> Result<Rc<ExprInfTypeData<'intern>>, SpannedTypeError> {
        use ValBinOp::*;
        let expr_ref = expr.take_ref();
        let int = self.infctx.int();
        let bit = self.infctx.bit();
        let expr = expr_ref.try_scan(|(expr, idx)| -> Result<_, SpannedTypeError> {
            let span = IndirectSpan::new(id.0, *idx);
            let spanned = |x| SpannedTypeError::new(x, span);
            let mut constrain = |lower, upper| self.infctx.constrain(lower, upper).map_err(spanned);
            Ok(match expr {
                ExprHead::Dyadic(op, [&left, &right]) => match op {
                    And | Xor | Or | ShiftR | ShiftL | Minus | Plus | Div | Modulo | Mul => {
                        constrain(left, int)?;
                        constrain(right, int)?;
                        int
                    }
                    LesserEq | Lesser | GreaterEq | Greater | Uneq | Equals => {
                        constrain(left, int)?;
                        constrain(right, int)?;
                        bit
                    }
                    Else => self.infctx.one_of(&[left, right]).map_err(spanned)?,
                    Then => right,
                    Range => {
                        constrain(left, int)?;
                        constrain(right, int)?;
                        self.infctx.range()
                    }
                    ParserApply => self.infctx.parser_apply(right, left).map_err(spanned)?,
                },
                ExprHead::Monadic(op, &inner) => match &op {
                    ValUnOp::Neg | ValUnOp::Not => {
                        constrain(inner, int)?;
                        int
                    }
                    ValUnOp::Size => self.infctx.check_size_of(inner).map_err(spanned)?,
                    ValUnOp::Wiggle(c, _) => {
                        let (inner, cont) = self.infctx.if_checked(inner).map_err(spanned)?;
                        let expr = self.db.lookup_intern_hir_constraint(*c);
                        self.constr_expression_type(&expr.expr, inner)
                            .map_err(spanned)?;
                        if expr.has_no_eof {
                            self.infctx.check_parser(cont).map_err(spanned)?;
                        }
                        cont
                    }
                    // TODO: we should make a variable that enforces dereference level 0 here
                    // so that backtracking can be toggled
                    ValUnOp::BtMark(_) => inner,
                    ValUnOp::Dot(name, _) => {
                        self.infctx.access_field(inner, *name).map_err(spanned)?
                    }
                    ValUnOp::EvalFun => self
                        .infctx
                        .function_apply(inner, &[], Application::Full)
                        .map_err(spanned)?,
                    ValUnOp::GetAddr => {
                        constrain(inner, int)?;
                        let u8 = self.infctx.u8();
                        self.infctx.array(ArrayKind::Each, u8)
                    }
                },
                ExprHead::Niladic(a) => match &a {
                    ResolvedAtom::Char(_) => self.infctx.char(),
                    ResolvedAtom::Number(_) => self.infctx.int(),
                    ResolvedAtom::Bool(_) => self.infctx.bit(),
                    ResolvedAtom::Single => self.infctx.single(),
                    ResolvedAtom::Array => self.infctx.array_parser(),
                    ResolvedAtom::ArrayFill => self.infctx.array_fill_parser(),
                    // the ambient type can only be none if we had another error before it,
                    // since we are necessarily referencing local parse statements for it
                    // to resolve correctly
                    ResolvedAtom::Span(..) => self.ambient_type().ok_or(SilencedError::new())?,
                    ResolvedAtom::Regex(..) => self.infctx.regex(),
                    ResolvedAtom::String(_) => self.infctx.byte_array(),
                    ResolvedAtom::Block(b, kind) => self.infer_block(*b, *kind).map_err(spanned)?,
                    ResolvedAtom::Lambda(l) => {
                        let lambda = l.lookup(self.db)?;
                        self.type_lambda(&lambda)?
                    }
                    ResolvedAtom::ParserDef(pd) | ResolvedAtom::Global(pd) => {
                        self.infctx.parserdef(pd.0)?
                    }
                    ResolvedAtom::Val(v) | ResolvedAtom::Captured(v) => {
                        self.infctx.lookup(*v).map_err(spanned)?
                    }
                },
                ExprHead::Variadic(ValVarOp::PartialApply(_), inner) => self
                    .infctx
                    .function_apply(
                        *inner[0],
                        &inner[1..].iter().copied().copied().collect::<Vec<_>>(),
                        Application::Partial,
                    )
                    .map_err(spanned)?,
            })
        })?;
        let rc_expr = Rc::new(expr);
        self.inf_expressions.insert(id, rc_expr.clone());
        Ok(rc_expr)
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

    fn arg_type(
        &mut self,
        arg: &hir::ArgDef,
    ) -> Result<Option<InfTypeId<'intern>>, SpannedTypeError> {
        if let Some(ty) = arg.ty {
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
        for child in ChildIter::new(root, self.db) {
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
                hir::HirNode::Lambda(lambda) => {
                    for arg in lambda.args.iter() {
                        let arg = arg.lookup(self.db)?;
                        let ty = self.arg_type(&arg)?;
                        vars.insert(arg.id.0, ty.unwrap_or_else(|| self.infctx.var()));
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
            self.with_ambient_type(ambient, |ctx| ctx.type_block(&block))?;
        }
        Ok(ret)
    }

    fn type_block(&mut self, block: &hir::Block) -> Result<(), SpannedTypeError> {
        let root_ctx = block.root_context.lookup(self.db)?;
        self.type_context(&root_ctx)
    }

    fn type_lambda(
        &mut self,
        lambda: &hir::Lambda,
    ) -> Result<InfTypeId<'intern>, SpannedTypeError> {
        let expr = lambda.expr.lookup(self.db)?;
        let ret = self.type_expr(&expr)?;
        let args = lambda
            .args
            .iter()
            .map(|arg| self.inftypes[&arg.0])
            .collect::<Vec<_>>();
        let args = self.infctx.slice_interner.intern_slice(&args);
        Ok(self.infctx.function(ret, args, Application::Full))
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
    pub defs: Vec<Identifier>,
    names: FxHashMap<Identifier, u32>,
    frozen: bool,
}

impl TypeVarCollection {
    pub fn get_var(&mut self, var_name: Identifier) -> Option<u32> {
        self.names.get(&var_name).copied()
    }
    pub fn freeze(&mut self) {
        self.frozen = true
    }
    pub fn at_id(db: &(impl TyHirs + ?Sized), id: hir::ParserDefId) -> SResult<Self> {
        let parserdef = id.lookup(db)?;
        let mut ret = TypeVarCollection {
            defs: vec![],
            names: FxHashMap::default(),
            frozen: false,
        };
        for (i, id) in parserdef.generics.iter().flatten().enumerate() {
            ret.names.insert(*id, i as u32);
            ret.defs.push(*id);
        }
        ret.freeze();
        Ok(ret)
    }
    pub fn var_types(&self, db: &(impl TyHirs + ?Sized), id: hir::ParserDefId) -> Vec<TypeId> {
        n_type_vars(db, id, self.defs.len() as u32)
    }
}

fn n_type_vars(db: &(impl TyHirs + ?Sized), id: hir::ParserDefId, n: u32) -> Vec<TypeId> {
    (0..n)
        .map(|i| db.intern_type(Type::TypeVarRef(TypeVarRef(id.0, i))))
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

pub enum NominalId {
    Def(hir::ParserDefId),
    Block(hir::BlockId),
}

impl NominalId {
    pub fn from_nominal_inf_head(head: &InternedNomHead) -> Self {
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
    Conv(TypeConvError),
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

impl From<TypeConvError> for SpannedTypeError {
    fn from(e: TypeConvError) -> Self {
        SpannedTypeError::Conv(e)
    }
}

impl Silencable for SpannedTypeError {
    type Out = SilencedError;

    fn silence(self) -> Self::Out {
        match self {
            SpannedTypeError::Spanned(inner, _) => inner.silence(),
            SpannedTypeError::Conv(e) => e.silence(),
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

fn lambda_type(db: &dyn TyHirs, id: hir::LambdaId) -> SResult<TypeId> {
    let lambda = id.lookup(db)?;
    Ok(
        Resolved::expr_with_data::<FullTypeId>(db, lambda.enclosing_expr)?
            .take_ref()
            .iter_parts()
            .find_map(|(x, ty)| match &x {
                ExprHead::Niladic(ResolvedAtom::Lambda(l)) if *l == id => Some(*ty),
                _ => None,
            })
            .unwrap(),
    )
}

fn block_type(db: &dyn TyHirs, id: hir::BlockId) -> SResult<TypeId> {
    let block = id.lookup(db)?;
    Ok(
        Resolved::expr_with_data::<FullTypeId>(db, block.enclosing_expr)?
            .take_ref()
            .iter_parts()
            .find_map(|(x, ty)| match &x {
                ExprHead::Niladic(ResolvedAtom::Block(b, _)) if *b == id => Some(*ty),
                _ => None,
            })
            .unwrap(),
    )
}

fn validate_export_arguments(db: &dyn TyHirs) -> SResult<Vec<SpannedTypeError>> {
    let exported_parserdefs = db.all_exported_parserdefs();
    let int = db.int();
    let mut errors = Vec::new();

    for pd in exported_parserdefs {
        let args = pd.lookup(db)?.args.unwrap_or_default();
        let arg_tys = db.parser_args(pd)?.args.unwrap_or_default();

        for (arg, ty) in args.iter().zip(arg_tys.iter()) {
            if *ty == int {
                continue;
            }

            errors.push(SpannedTypeError::new(
                TypeError::UnsupportedExportArgument {
                    def_id: arg.0,
                    arg_name: arg.lookup(db)?.name,
                },
                IndirectSpan::default_span(arg.0),
            ));
        }
    }

    Ok(errors)
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
