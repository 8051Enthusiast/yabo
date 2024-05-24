mod convert;
mod expr;
mod len;
mod represent;
mod strictness;

use std::num::NonZeroU32;

use fxhash::FxHashMap;

use len::LenMirCtx;
pub use strictness::Strictness;
use yaboc_ast::expr::WiggleKind;
use yaboc_ast::ConstraintAtom;
use yaboc_base::dbpanic;
use yaboc_base::interner::Regex;
use yaboc_base::{
    error::{SResult, Silencable},
    interner::{DefId, FieldName},
};
use yaboc_constraint::{Constraints, Origin};
use yaboc_dependents::{Dependents, SubValue};
use yaboc_expr::{ExprHead, ExprIdx, Expression, FetchExpr, TakeRef};
use yaboc_hir::{Block, BlockId, ExprId, HirConstraintId, HirIdWrapper, ParserDefId};
use yaboc_hir_types::FullTypeId;
use yaboc_req::{NeededBy, RequirementSet};
use yaboc_resolve::expr::{Resolved, ResolvedAtom};
use yaboc_resolve::expr::{ValBinOp, ValUnOp};
use yaboc_types::{Type, TypeId};

use self::convert::ConvertCtx;

pub use represent::{print_all_mir, print_all_mir_graphs};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum MirKind {
    Call(RequirementSet),
    Len,
}

#[salsa::query_group(MirDatabase)]
pub trait Mirs: Dependents + Constraints {
    fn mir(&self, kind: FunKind, mir_kind: MirKind) -> SResult<Function>;
    fn strictness(&self, kind: FunKind, mir_kind: MirKind) -> SResult<Vec<Strictness>>;
}

fn mir(db: &dyn Mirs, kind: FunKind, mir_kind: MirKind) -> SResult<Function> {
    match (kind, mir_kind) {
        (FunKind::Block(block), MirKind::Call(req)) => mir_block(db, block, req),
        (FunKind::ParserDef(pd), MirKind::Call(req)) => mir_pd(db, pd, req),
        (FunKind::If(constraint, ty, wiggle), MirKind::Call(req)) => {
            mir_if(db, constraint, ty, wiggle, req)
        }
        (FunKind::Block(block), MirKind::Len) => LenMirCtx::new_block(db, block),
        (FunKind::ParserDef(pd), MirKind::Len) => LenMirCtx::new_pd(db, pd),
        (FunKind::If(_, ty, _), MirKind::Len) => LenMirCtx::new_if(db, ty),
    }
}

fn strictness(db: &dyn Mirs, kind: FunKind, mir_kind: MirKind) -> SResult<Vec<Strictness>> {
    let fun = db.mir(kind, mir_kind)?;
    strictness::StrictnessCtx::new(&fun, db)?.run()
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum FunKind {
    Block(BlockId),
    ParserDef(ParserDefId),
    If(HirConstraintId, TypeId, WiggleKind),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
#[repr(i64)]
pub enum ReturnStatus {
    Ok,
    Error,
    Eof,
    Backtrack,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum IntBinOp {
    And,
    Xor,
    Or,
    ShiftR,
    ShiftL,
    Minus,
    Plus,
    Div,
    Modulo,
    Mul,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Comp {
    LesserEq,
    Lesser,
    GreaterEq,
    Greater,
    Uneq,
    Equals,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum IntUnOp {
    Not,
    Neg,
}

impl<C> TryFrom<&ValUnOp<C>> for IntUnOp {
    type Error = ();
    fn try_from(value: &ValUnOp<C>) -> Result<Self, Self::Error> {
        Ok(match value {
            ValUnOp::Not => IntUnOp::Not,
            ValUnOp::Neg => IntUnOp::Neg,
            _ => return Err(()),
        })
    }
}

impl TryFrom<&ValBinOp> for IntBinOp {
    type Error = ();
    fn try_from(value: &ValBinOp) -> Result<Self, Self::Error> {
        Ok(match value {
            ValBinOp::And => IntBinOp::And,
            ValBinOp::Xor => IntBinOp::Xor,
            ValBinOp::Or => IntBinOp::Or,
            ValBinOp::ShiftR => IntBinOp::ShiftR,
            ValBinOp::ShiftL => IntBinOp::ShiftL,
            ValBinOp::Minus => IntBinOp::Minus,
            ValBinOp::Plus => IntBinOp::Plus,
            ValBinOp::Div => IntBinOp::Div,
            ValBinOp::Modulo => IntBinOp::Modulo,
            ValBinOp::Mul => IntBinOp::Mul,
            ValBinOp::LesserEq
            | ValBinOp::Lesser
            | ValBinOp::GreaterEq
            | ValBinOp::Greater
            | ValBinOp::Uneq
            | ValBinOp::Equals
            | ValBinOp::ParserApply
            | ValBinOp::Else
            | ValBinOp::Then
            | ValBinOp::Range => return Err(()),
        })
    }
}

impl TryFrom<&ValBinOp> for Comp {
    type Error = ();

    fn try_from(value: &ValBinOp) -> Result<Self, Self::Error> {
        Ok(match value {
            ValBinOp::LesserEq => Comp::LesserEq,
            ValBinOp::Lesser => Comp::Lesser,
            ValBinOp::GreaterEq => Comp::GreaterEq,
            ValBinOp::Greater => Comp::Greater,
            ValBinOp::Uneq => Comp::Uneq,
            ValBinOp::Equals => Comp::Equals,
            ValBinOp::And
            | ValBinOp::Xor
            | ValBinOp::Or
            | ValBinOp::ShiftR
            | ValBinOp::ShiftL
            | ValBinOp::Minus
            | ValBinOp::Plus
            | ValBinOp::Div
            | ValBinOp::Modulo
            | ValBinOp::Mul
            | ValBinOp::ParserApply
            | ValBinOp::Else
            | ValBinOp::Then
            | ValBinOp::Range => return Err(()),
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ZstVal {
    Nil,
    Single,
    Array,
    ArrayFill,
    Regex(Regex),
    ParserDef(ParserDefId),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Val {
    Char(u32),
    Int(i64),
    Bool(bool),
    Parser(ZstVal),
    Undefined,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ControlFlow {
    pub next: BBRef,
    pub backtrack: Option<BBRef>,
    pub error: Option<BBRef>,
    pub eof: Option<BBRef>,
}

impl ControlFlow {
    pub fn new(next: BBRef) -> Self {
        Self {
            next,
            backtrack: None,
            error: None,
            eof: None,
        }
    }
    pub fn new_with_error(next: BBRef, error: BBRef) -> Self {
        Self {
            next,
            backtrack: None,
            error: Some(error),
            eof: None,
        }
    }
    pub fn new_with_exc(next: BBRef, exc: ExceptionRetreat) -> Self {
        Self {
            next,
            backtrack: Some(exc.backtrack),
            error: Some(exc.error),
            eof: Some(exc.eof),
        }
    }
    pub fn successors(&self) -> impl Iterator<Item = BBRef> {
        [self.backtrack, self.error, self.eof]
            .into_iter()
            .flatten()
            .chain(std::iter::once(self.next))
    }
    pub fn map_bb(&self, mut f: impl FnMut(BBRef) -> BBRef) -> Self {
        Self {
            next: f(self.next),
            backtrack: self.backtrack.map(&mut f),
            error: self.error.map(&mut f),
            eof: self.eof.map(f),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct CallMeta {
    pub req: RequirementSet,
    pub tail: bool,
}

impl CallMeta {
    pub fn new(req: RequirementSet, tail: bool) -> Self {
        Self { req, tail }
    }
    pub fn with_req(self, f: impl FnOnce(RequirementSet) -> RequirementSet) -> Self {
        Self {
            req: f(self.req),
            tail: self.tail,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct InsRef(pub BBRef, pub u32);

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum MirInstr {
    IntBin(PlaceRef, IntBinOp, PlaceRef, PlaceRef),
    IntUn(PlaceRef, IntUnOp, PlaceRef),
    Comp(PlaceRef, Comp, PlaceRef, PlaceRef),
    StoreVal(PlaceRef, Val),
    SetDiscriminant(PlaceRef, FieldName, bool),
    Range(PlaceRef, PlaceRef, PlaceRef, ControlFlow),
    GetAddr(PlaceRef, PlaceRef, ControlFlow),
    ApplyArgs(PlaceRef, PlaceRef, Vec<(PlaceRef, bool)>, u64, ControlFlow),
    Copy(PlaceRef, PlaceRef, ControlFlow),
    EvalFun(PlaceRef, PlaceRef, ControlFlow),
    ParseCall(
        Option<PlaceRef>,
        Option<PlaceRef>,
        CallMeta,
        PlaceRef,
        PlaceRef,
        Option<ControlFlow>,
    ),
    LenCall(PlaceRef, PlaceRef, ControlFlow),
    Field(PlaceRef, PlaceRef, FieldName, ControlFlow),
    AssertVal(PlaceRef, ConstraintAtom, ControlFlow),
    Span(PlaceRef, PlaceRef, PlaceRef, ControlFlow),
    Branch(BBRef),
    Return(ReturnStatus),
}

impl MirInstr {
    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            MirInstr::Branch(_)
                | MirInstr::Return(_)
                | MirInstr::AssertVal(..)
                | MirInstr::Field(..)
                | MirInstr::ParseCall(..)
                | MirInstr::LenCall(..)
                | MirInstr::GetAddr(..)
                | MirInstr::ApplyArgs(..)
                | MirInstr::Copy(..)
                | MirInstr::EvalFun(..)
                | MirInstr::Span(..)
                | MirInstr::Range(..)
        )
    }
    pub fn control_flow(&self) -> Option<ControlFlow> {
        match self {
            MirInstr::Branch(next) => Some(ControlFlow {
                next: *next,
                backtrack: None,
                error: None,
                eof: None,
            }),
            MirInstr::AssertVal(_, _, control_flow)
            | MirInstr::Field(_, _, _, control_flow)
            | MirInstr::ParseCall(.., Some(control_flow))
            | MirInstr::LenCall(_, _, control_flow)
            | MirInstr::GetAddr(_, _, control_flow)
            | MirInstr::ApplyArgs(.., control_flow)
            | MirInstr::Copy(_, _, control_flow)
            | MirInstr::EvalFun(_, _, control_flow)
            | MirInstr::Span(.., control_flow)
            | MirInstr::Range(.., control_flow) => Some(*control_flow),
            MirInstr::IntBin(_, _, _, _)
            | MirInstr::IntUn(_, _, _)
            | MirInstr::Comp(_, _, _, _)
            | MirInstr::StoreVal(_, _)
            | MirInstr::SetDiscriminant(_, _, _)
            | MirInstr::ParseCall(.., None)
            | MirInstr::Return(_) => None,
        }
    }
    pub fn map_bb(&self, mut f: impl FnMut(BBRef) -> BBRef) -> Self {
        match self {
            MirInstr::Branch(bb) => MirInstr::Branch(f(*bb)),
            MirInstr::AssertVal(place, atom, control_flow) => {
                MirInstr::AssertVal(*place, atom.clone(), control_flow.map_bb(f))
            }
            MirInstr::Field(place, val, field, control_flow) => {
                MirInstr::Field(*place, *val, *field, control_flow.map_bb(f))
            }
            MirInstr::ParseCall(ret, val, meta, parser, args, Some(control_flow)) => {
                MirInstr::ParseCall(
                    *ret,
                    *val,
                    *meta,
                    *parser,
                    *args,
                    Some(control_flow.map_bb(f)),
                )
            }
            MirInstr::LenCall(ret, val, control_flow) => {
                MirInstr::LenCall(*ret, *val, control_flow.map_bb(f))
            }
            MirInstr::GetAddr(ret, val, control_flow) => {
                MirInstr::GetAddr(*ret, *val, control_flow.map_bb(f))
            }
            MirInstr::ApplyArgs(ret, val, args, offset, control_flow) => {
                MirInstr::ApplyArgs(*ret, *val, args.clone(), *offset, control_flow.map_bb(f))
            }
            MirInstr::Copy(ret, val, control_flow) => {
                MirInstr::Copy(*ret, *val, control_flow.map_bb(f))
            }
            MirInstr::EvalFun(ret, val, control_flow) => {
                MirInstr::EvalFun(*ret, *val, control_flow.map_bb(f))
            }
            MirInstr::Span(ret, start, end, control_flow) => {
                MirInstr::Span(*ret, *start, *end, control_flow.map_bb(f))
            }
            MirInstr::Range(ret, start, end, control_flow) => {
                MirInstr::Range(*ret, *start, *end, control_flow.map_bb(f))
            }
            MirInstr::IntBin(_, _, _, _)
            | MirInstr::IntUn(_, _, _)
            | MirInstr::Comp(_, _, _, _)
            | MirInstr::StoreVal(_, _)
            | MirInstr::SetDiscriminant(_, _, _)
            | MirInstr::Return(_)
            | MirInstr::ParseCall(.., None) => {
                assert!(self.control_flow().is_none());
                self.clone()
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct BBRef(NonZeroU32);

impl BBRef {
    pub fn as_index(self) -> usize {
        u32::from(self.0) as usize - 1
    }

    fn from_index(index: usize) -> Self {
        Self(u32::try_from(index + 1).unwrap().try_into().unwrap())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ExceptionRetreat {
    pub backtrack: BBRef,
    pub eof: BBRef,
    pub error: BBRef,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct PlaceRef(NonZeroU32);

impl PlaceRef {
    pub fn as_index(self) -> usize {
        u32::from(self.0) as usize - 1
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct StackRef(NonZeroU32);

impl StackRef {
    pub fn as_index(self) -> usize {
        u32::from(self.0) as usize - 1
    }
}

#[derive(Default, Clone, PartialEq, Eq, Hash, Debug)]
pub struct BasicBlock {
    ins: Vec<MirInstr>,
}

impl BasicBlock {
    pub fn append_ins(&mut self, ins: MirInstr) {
        if let Some(last_ins) = self.ins.last() {
            if last_ins.is_terminator() {
                panic!(
                    "Cannot append instruction {ins:?} to a block with a terminator {last_ins:?}",
                );
            }
        }
        self.ins.push(ins)
    }
    pub fn ins(&self) -> impl Iterator<Item = MirInstr> + '_ {
        self.ins.iter().cloned()
    }
    pub fn terminator(&self) -> MirInstr {
        self.ins.last().unwrap().clone()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Place {
    Captures,
    Arg,
    Return,
    ReturnLen,
    Stack(StackRef),
    Field(PlaceRef, DefId),
    Captured(PlaceRef, DefId),
    Front(PlaceRef),
    ModifiedBy(InsRef),
    Global(ParserDefId),
    Undefined,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum PlaceOrigin {
    Node(DefId),
    Ambient(BlockId, DefId),
    Expr(ExprId, ExprIdx<Resolved>),
    Ret,
    Arg,
    PolyLen,
}

impl From<Origin> for PlaceOrigin {
    fn from(value: Origin) -> Self {
        match value {
            Origin::Expr(id, idx) => Self::Expr(id, idx),
            Origin::Node(id) => Self::Node(id),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct PlaceInfo {
    pub place: Place,
    pub ty: TypeId,
    pub remove_bt: bool,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Function {
    bb: Vec<BasicBlock>,
    place: Vec<PlaceInfo>,
    stack: Vec<PlaceOrigin>,
    success_returns: Vec<BBRef>,
    arg: Option<PlaceRef>,
    ret: Option<PlaceRef>,
    retlen: Option<PlaceRef>,
}

const fn const_ref(r: u32) -> PlaceRef {
    PlaceRef(match NonZeroU32::new(r) {
        Some(x) => x,
        None => panic!("ref is zero"),
    })
}

const CAP_REF: PlaceRef = const_ref(1);

impl Function {
    pub fn bb(&self, bb: BBRef) -> &BasicBlock {
        &self.bb[bb.as_index()]
    }

    fn bb_mut(&mut self, bb: BBRef) -> &mut BasicBlock {
        &mut self.bb[bb.as_index()]
    }

    pub fn place(&self, place_ref: PlaceRef) -> PlaceInfo {
        self.place[place_ref.as_index()]
    }

    pub fn stack(&self, stack_ref: StackRef) -> PlaceOrigin {
        self.stack[stack_ref.as_index()]
    }

    pub fn cap(&self) -> PlaceRef {
        CAP_REF
    }

    pub fn arg(&self) -> Option<PlaceRef> {
        self.arg
    }

    pub fn ret(&self) -> Option<PlaceRef> {
        self.ret
    }

    pub fn retlen(&self) -> Option<PlaceRef> {
        self.retlen
    }

    pub fn entry(&self) -> BBRef {
        BBRef(NonZeroU32::new(1).unwrap())
    }

    pub fn ins_at(&self, ins_ref: InsRef) -> MirInstr {
        self.bb(ins_ref.0).ins[ins_ref.1 as usize].clone()
    }

    pub fn is_valid(&self) -> bool {
        self.bb
            .iter()
            .all(|x| x.ins().last().map(|x| x.is_terminator()).unwrap_or(false))
    }

    pub fn iter_stack(&self) -> impl Iterator<Item = (StackRef, PlaceOrigin)> + '_ {
        (0..self.stack.len()).map(|x| {
            (
                StackRef(NonZeroU32::new((x + 1) as u32).unwrap()),
                self.stack[x],
            )
        })
    }

    pub fn iter_places(&self) -> impl Iterator<Item = (PlaceRef, PlaceInfo)> + '_ {
        (0..self.place.len()).map(|x| {
            (
                PlaceRef(NonZeroU32::new((x + 1) as u32).unwrap()),
                self.place[x],
            )
        })
    }

    pub fn iter_bb(&self) -> impl Iterator<Item = (BBRef, &BasicBlock)> + '_ {
        (0..self.bb.len()).map(|x| (BBRef(NonZeroU32::new((x + 1) as u32).unwrap()), &self.bb[x]))
    }

    pub fn preds(&self) -> Vec<Vec<BBRef>> {
        let mut preds = vec![vec![]; self.bb.len()];
        for (bb, bb_data) in self.iter_bb() {
            let Some(controlflow) = bb_data.terminator().control_flow() else {
                continue;
            };
            for succ in controlflow.successors() {
                preds[succ.as_index()].push(bb);
            }
        }
        preds
    }

    pub fn success_returns(&self) -> &[BBRef] {
        &self.success_returns
    }

    pub fn remove_unused_bb(mut self) -> Self {
        let mut used = vec![false; self.bb.len()];
        let mut queue = vec![self.entry()];
        while let Some(bb) = queue.pop() {
            if used[bb.as_index()] {
                continue;
            }
            used[bb.as_index()] = true;
            let bb_data = self.bb(bb);
            if let Some(controlflow) = bb_data.terminator().control_flow() {
                for succ in controlflow.successors() {
                    queue.push(succ);
                }
            }
        }
        let mut current_index = 0;
        let mut new_index = Vec::with_capacity(self.bb.len());
        for is_used in used.iter() {
            new_index.push(current_index);
            current_index += *is_used as usize;
        }
        let mut new_bbs = Vec::with_capacity(current_index);

        let remap = |bbref: BBRef| -> BBRef { BBRef::from_index(new_index[bbref.as_index()]) };

        for (bb, bb_data) in self.iter_bb() {
            if !used[bb.as_index()] {
                continue;
            }
            let mut new_bb = BasicBlock::default();
            for ins in bb_data.ins() {
                let new_ins = ins.map_bb(remap);
                new_bb.ins.push(new_ins);
            }
            new_bbs.push(new_bb);
        }

        self.bb = new_bbs;
        self.success_returns = self
            .success_returns
            .into_iter()
            .filter(|x| used[x.as_index()])
            .map(remap)
            .collect();
        self
    }
}

pub struct FunctionWriter {
    fun: Function,
    place_map: FxHashMap<Place, PlaceRef>,
    current_bb: BBRef,
}

impl FunctionWriter {
    pub fn new(
        fun_ty: TypeId,
        from_ty: Option<TypeId>,
        ret_ty: TypeId,
        req: RequirementSet,
    ) -> Self {
        let fun = Function {
            bb: vec![Default::default()],
            place: Default::default(),
            stack: Default::default(),
            success_returns: Default::default(),
            arg: None,
            ret: None,
            retlen: None,
        };
        let mut builder = FunctionWriter {
            fun,
            place_map: Default::default(),
            current_bb: BBRef(NonZeroU32::new(1).unwrap()),
        };
        builder.add_place(PlaceInfo {
            place: Place::Captures,
            ty: fun_ty,
            remove_bt: false,
        });
        if let Some(from_ty) = from_ty {
            let arg = builder.add_place(PlaceInfo {
                place: Place::Arg,
                ty: from_ty,
                remove_bt: false,
            });
            builder.fun.arg = Some(arg);
        }
        if req.contains(NeededBy::Val) {
            let ret = builder.add_place(PlaceInfo {
                place: Place::Return,
                ty: ret_ty,
                remove_bt: false,
            });
            builder.fun.ret = Some(ret);
        }
        if req.contains(NeededBy::Len) {
            let retlen = builder.add_place(PlaceInfo {
                place: Place::ReturnLen,
                ty: from_ty.unwrap(),
                remove_bt: false,
            });
            builder.fun.retlen = Some(retlen);
        }
        builder
    }

    pub fn new_block(db: &dyn Mirs, block: &Block, req: RequirementSet) -> SResult<Self> {
        let id = block.id;
        let block_ty = Resolved::expr_with_data::<FullTypeId>(db, block.enclosing_expr)?
            .take_ref()
            .iter_parts()
            .find_map(|(x, ty)| match &x {
                ExprHead::Niladic(ResolvedAtom::Block(b, _)) if *b == id => Some(*ty),
                _ => None,
            })
            .expect("could not find block within enclosing expression");
        let ty = db.lookup_intern_type(block_ty);
        let (result, arg) = if let Type::ParserArg { result, arg } = ty {
            (result, Some(arg))
        } else if let Type::FunctionArg(result, _) = ty {
            (result, None)
        } else {
            dbpanic!(db, "should have been a parser type, was {}", &block_ty)
        };
        let f = FunctionWriter::new(block_ty, arg, result, req);
        Ok(f)
    }

    pub fn new_pd(db: &dyn Mirs, id: ParserDefId, req: RequirementSet) -> SResult<Self> {
        let sig = db.parser_args(id)?;
        let from = sig.from;
        let thunk = db.intern_type(Type::Nominal(sig.thunk));
        let ret_ty = db.parser_returns(id)?.deref;
        let fun_ty = if let Some(from) = from {
            db.intern_type(Type::ParserArg {
                result: thunk,
                arg: from,
            })
        } else {
            db.intern_type(Type::FunctionArg(thunk, Default::default()))
        };
        let arg_ty = from;
        let f = FunctionWriter::new(fun_ty, arg_ty, ret_ty, req);
        Ok(f)
    }

    pub fn make_top_level_retreat(&mut self) -> ExceptionRetreat {
        let backtrack = self.new_bb();
        self.set_bb(backtrack);
        self.ret(ReturnStatus::Backtrack);
        let error = self.new_bb();
        self.set_bb(error);
        self.ret(ReturnStatus::Error);
        let eof = self.new_bb();
        self.set_bb(eof);
        self.ret(ReturnStatus::Eof);
        ExceptionRetreat {
            backtrack,
            eof,
            error,
        }
    }

    pub fn add_place(&mut self, place: PlaceInfo) -> PlaceRef {
        if let Some(p) = self.place_map.get(&place.place) {
            return *p;
        }
        let placeref = PlaceRef(NonZeroU32::new(self.fun.place.len() as u32 + 1).unwrap());
        self.place_map.insert(place.place, placeref);
        self.fun.place.push(place);
        placeref
    }

    pub fn new_stack_ref(&mut self, origin: PlaceOrigin) -> StackRef {
        self.fun.stack.push(origin);
        StackRef(NonZeroU32::new(self.fun.stack.len() as u32).unwrap())
    }

    pub fn new_bb(&mut self) -> BBRef {
        let new_bb = BasicBlock::default();
        self.fun.bb.push(new_bb);
        BBRef(NonZeroU32::new(self.fun.bb.len() as u32).unwrap())
    }

    pub fn append_ins(&mut self, ins: MirInstr) {
        self.fun.bb_mut(self.current_bb).append_ins(ins)
    }

    pub fn set_bb(&mut self, bb: BBRef) {
        self.current_bb = bb;
    }

    pub fn branch(&mut self, bb: BBRef) {
        self.fun
            .bb_mut(self.current_bb)
            .append_ins(MirInstr::Branch(bb));
    }

    pub fn ret(&mut self, status: ReturnStatus) {
        self.fun
            .bb_mut(self.current_bb)
            .append_ins(MirInstr::Return(status));
        if status == ReturnStatus::Ok {
            self.fun.success_returns.push(self.current_bb);
        }
    }

    pub fn copy(&mut self, origin: PlaceRef, target: PlaceRef, error: BBRef) {
        let new_block = self.new_bb();
        self.fun.bb_mut(self.current_bb).append_ins(MirInstr::Copy(
            target,
            origin,
            ControlFlow::new_with_error(new_block, error),
        ));
        self.set_bb(new_block);
    }

    pub fn field(
        &mut self,
        origin: PlaceRef,
        field: FieldName,
        target: PlaceRef,
        error: BBRef,
        backtrack: BBRef,
    ) {
        let new_block = self.new_bb();
        self.fun.bb_mut(self.current_bb).append_ins(MirInstr::Field(
            target,
            origin,
            field,
            ControlFlow {
                next: new_block,
                backtrack: Some(backtrack),
                error: Some(error),
                eof: None,
            },
        ));
        self.set_bb(new_block);
    }

    pub fn parse_call(
        &mut self,
        call_info: CallMeta,
        arg: PlaceRef,
        fun: PlaceRef,
        ret: Option<PlaceRef>,
        retlen: Option<PlaceRef>,
        exc: ExceptionRetreat,
    ) {
        let new_block = self.new_bb();
        self.fun
            .bb_mut(self.current_bb)
            .append_ins(MirInstr::ParseCall(
                ret,
                retlen,
                call_info,
                arg,
                fun,
                Some(ControlFlow::new_with_exc(new_block, exc)),
            ));
        self.set_bb(new_block);
    }

    pub fn tail_parse_call(
        &mut self,
        call_info: CallMeta,
        arg: PlaceRef,
        fun: PlaceRef,
        ret: Option<PlaceRef>,
        retlen: Option<PlaceRef>,
    ) {
        self.fun
            .bb_mut(self.current_bb)
            .append_ins(MirInstr::ParseCall(ret, retlen, call_info, arg, fun, None));
        self.fun.success_returns.push(self.current_bb);
    }

    pub fn load_int(&mut self, n: i64, place: PlaceRef) {
        self.append_ins(MirInstr::StoreVal(place, Val::Int(n)));
    }

    pub fn load_char(&mut self, c: u32, place: PlaceRef) {
        self.append_ins(MirInstr::StoreVal(place, Val::Char(c)));
    }

    pub fn load_bool(&mut self, b: bool, place: PlaceRef) {
        self.append_ins(MirInstr::StoreVal(place, Val::Bool(b)));
    }

    pub fn load_zst(&mut self, val: ZstVal, place: PlaceRef) {
        self.append_ins(MirInstr::StoreVal(place, Val::Parser(val)));
    }

    pub fn load_undef(&mut self, place: PlaceRef) {
        self.append_ins(MirInstr::StoreVal(place, Val::Undefined));
    }

    pub fn int_bin_op(&mut self, target: PlaceRef, op: IntBinOp, left: PlaceRef, right: PlaceRef) {
        self.append_ins(MirInstr::IntBin(target, op, left, right));
    }

    pub fn int_un_op(&mut self, target: PlaceRef, op: IntUnOp, arg: PlaceRef) {
        self.append_ins(MirInstr::IntUn(target, op, arg));
    }

    pub fn comp(&mut self, target: PlaceRef, op: Comp, left: PlaceRef, right: PlaceRef) {
        self.append_ins(MirInstr::Comp(target, op, left, right));
    }

    pub fn len_call(&mut self, fun: PlaceRef, ret: PlaceRef, exc: ExceptionRetreat) {
        let new_block = self.new_bb();
        self.fun
            .bb_mut(self.current_bb)
            .append_ins(MirInstr::LenCall(
                ret,
                fun,
                ControlFlow::new_with_exc(new_block, exc),
            ));
        self.set_bb(new_block);
    }

    pub fn apply_args(
        &mut self,
        fun: PlaceRef,
        args: Vec<(PlaceRef, bool)>,
        target: PlaceRef,
        first_arg_index: u64,
        error: BBRef,
    ) {
        let new_block = self.new_bb();
        self.fun
            .bb_mut(self.current_bb)
            .append_ins(MirInstr::ApplyArgs(
                target,
                fun,
                args,
                first_arg_index,
                ControlFlow::new_with_error(new_block, error),
            ));
        self.set_bb(new_block);
    }

    pub fn eval_fun(&mut self, fun: PlaceRef, ret: PlaceRef, exc: ExceptionRetreat) {
        let new_block = self.new_bb();
        self.fun
            .bb_mut(self.current_bb)
            .append_ins(MirInstr::EvalFun(
                ret,
                fun,
                ControlFlow::new_with_exc(new_block, exc),
            ));
        self.set_bb(new_block);
    }

    pub fn assert_val(&mut self, val: PlaceRef, constrinat: ConstraintAtom, backtrack: BBRef) {
        let new_block = self.new_bb();
        self.fun
            .bb_mut(self.current_bb)
            .append_ins(MirInstr::AssertVal(
                val,
                constrinat,
                ControlFlow {
                    next: new_block,
                    backtrack: Some(backtrack),
                    error: None,
                    eof: None,
                },
            ));
        self.set_bb(new_block);
    }

    pub fn get_addr(&mut self, addr: PlaceRef, ret: PlaceRef, exc: ExceptionRetreat) {
        let new_block = self.new_bb();
        self.fun
            .bb_mut(self.current_bb)
            .append_ins(MirInstr::GetAddr(
                ret,
                addr,
                ControlFlow::new_with_exc(new_block, exc),
            ));
        self.set_bb(new_block);
    }

    pub fn next_ins(&self) -> InsRef {
        let current_bb = self.current_bb;
        let offset = self.fun.bb(current_bb).ins.len();
        InsRef(current_bb, offset as u32)
    }

    pub fn make_place_ref(&mut self, place: Place, ty: TypeId) -> PlaceRef {
        let place_info = PlaceInfo {
            place,
            ty,
            remove_bt: false,
        };
        self.add_place(place_info)
    }

    pub fn new_stack_place(&mut self, ty: TypeId, origin: PlaceOrigin) -> PlaceRef {
        let new_place = Place::Stack(self.new_stack_ref(origin));
        self.make_place_ref(new_place, ty)
    }

    pub fn new_remove_bt_stack_place(&mut self, ty: TypeId, origin: PlaceOrigin) -> PlaceRef {
        let new_place = Place::Stack(self.new_stack_ref(origin));
        let place_info = PlaceInfo {
            place: new_place,
            ty,
            remove_bt: true,
        };
        self.add_place(place_info)
    }

    fn span(&mut self, target: PlaceRef, start_place: PlaceRef, end_place: PlaceRef, err: BBRef) {
        let new_block = self.new_bb();
        self.append_ins(MirInstr::Span(
            target,
            start_place,
            end_place,
            ControlFlow::new_with_error(new_block, err),
        ));
        self.set_bb(new_block);
    }

    fn range(&mut self, target: PlaceRef, start_place: PlaceRef, end_place: PlaceRef, err: BBRef) {
        let new_block = self.new_bb();
        self.append_ins(MirInstr::Range(
            target,
            start_place,
            end_place,
            ControlFlow::new_with_error(new_block, err),
        ));
        self.set_bb(new_block);
    }
}

fn mir_block(db: &dyn Mirs, block: BlockId, requirements: RequirementSet) -> SResult<Function> {
    let order = db.block_serialization(block).silence()?;
    let mut ctx = ConvertCtx::new_block_builder(db, block, requirements, &order)?;
    for value in order.eval_order.iter() {
        if (value.requirements & requirements).is_empty() {
            continue;
        }
        ctx.add_sub_value(value.val)?;
    }
    ctx.change_context(block.lookup(db)?.root_context);
    Ok(ctx.finish_fun())
}

fn mir_pd(db: &dyn Mirs, pd: ParserDefId, requirements: RequirementSet) -> SResult<Function> {
    let parserdef = pd.lookup(db)?;
    let mut ctx = ConvertCtx::new_parserdef_builder(db, pd, requirements)?;
    ctx.add_sub_value(SubValue::new_val(parserdef.to.0))?;
    if parserdef.from.is_some() {
        ctx.parserdef_parse(&parserdef)?;
    } else {
        ctx.parserdef_eval_fun(&parserdef)?;
    }
    Ok(ctx.finish_fun())
}

fn mir_if(
    db: &dyn Mirs,
    constr: HirConstraintId,
    ty: TypeId,
    kind: WiggleKind,
    requirements: RequirementSet,
) -> SResult<Function> {
    let mut ctx = ConvertCtx::new_if_builder(db, ty, requirements, kind == WiggleKind::Try)?;
    ctx.if_parser(constr)?;
    Ok(ctx.finish_fun())
}

#[cfg(test)]
mod tests {
    use super::*;
    use yaboc_ast::{import::Import, AstDatabase};
    use yaboc_base::{
        config::ConfigDatabase, interner::InternerDatabase, source::FileDatabase, Context,
    };
    use yaboc_constraint::ConstraintDatabase;
    use yaboc_dependents::DependentsDatabase;
    use yaboc_hir::{HirDatabase, Hirs, Parser};
    use yaboc_hir_types::HirTypesDatabase;
    use yaboc_resolve::ResolveDatabase;
    use yaboc_types::TypeInternerDatabase;

    #[salsa::database(
        InternerDatabase,
        ConfigDatabase,
        AstDatabase,
        FileDatabase,
        HirDatabase,
        ResolveDatabase,
        TypeInternerDatabase,
        HirTypesDatabase,
        DependentsDatabase,
        ConstraintDatabase,
        MirDatabase
    )]
    #[derive(Default)]
    pub struct MirTestDatabase {
        storage: salsa::Storage<MirTestDatabase>,
    }

    impl salsa::Database for MirTestDatabase {}

    #[test]
    fn basic_mir() {
        let ctx = Context::<MirTestDatabase>::mock(
            r"
def *main = {
    | a: ~
      b: ~
      e: {
          let y: int = x + b
          x: ~
      }

    | b: ~
      a: ~
      c: ~
      let d: int = b + c * a * a
}
        ",
        );
        let main = ctx.parser("main");
        let blocks = ctx.db.all_parserdef_blocks(main);
        for need in [NeededBy::Val, NeededBy::Len] {
            for block in blocks.iter() {
                ctx.db
                    .mir(FunKind::Block(*block), MirKind::Call(need.into()))
                    .unwrap();
            }
        }
        ctx.db
            .mir(
                FunKind::ParserDef(main),
                MirKind::Call(NeededBy::Val.into()),
            )
            .unwrap();
        ctx.db
            .mir(
                FunKind::ParserDef(main),
                MirKind::Call(NeededBy::Len.into()),
            )
            .unwrap();
    }
}
