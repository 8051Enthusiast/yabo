mod convert;
mod represent;
mod strictness;

use std::num::NonZeroU32;

use fxhash::FxHashMap;

pub use strictness::Strictness;
use yaboc_ast::expr::{ValBinOp, ValUnOp};
use yaboc_ast::ConstraintAtom;
use yaboc_base::{
    error::{SResult, Silencable},
    interner::{DefId, FieldName},
    source::SpanIndex,
};
use yaboc_dependents::{Dependents, RequirementSet, SubValue};
use yaboc_hir::{BlockId, ExprId, HirIdWrapper, ParserDefId};
use yaboc_types::TypeId;

use self::convert::ConvertCtx;

pub use represent::print_all_mir;

#[salsa::query_group(MirDatabase)]
pub trait Mirs: Dependents {
    fn mir(&self, kind: FunKind, requirements: RequirementSet) -> SResult<Function>;
    fn strictness(&self, kind: FunKind, requirements: RequirementSet) -> SResult<Vec<Strictness>>;
}

fn mir(db: &dyn Mirs, kind: FunKind, requirements: RequirementSet) -> SResult<Function> {
    match kind {
        FunKind::Block(block) => mir_block(db, block, requirements),
        FunKind::ParserDef(pd) => mir_pd(db, pd, requirements),
    }
}

fn strictness(
    db: &dyn Mirs,
    kind: FunKind,
    requirements: RequirementSet,
) -> SResult<Vec<Strictness>> {
    let fun = db.mir(kind, requirements)?;
    strictness::StrictnessCtx::new(&fun, db)?.run()
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum FunKind {
    Block(BlockId),
    ParserDef(ParserDefId),
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
            | ValBinOp::Compose
            | ValBinOp::ParserApply
            | ValBinOp::Else => return Err(()),
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
            | ValBinOp::Compose
            | ValBinOp::ParserApply
            | ValBinOp::Else => return Err(()),
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Val {
    Char(u32),
    Int(i64),
    Bool(bool),
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
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum MirInstr {
    IntBin(PlaceRef, IntBinOp, PlaceRef, PlaceRef),
    IntUn(PlaceRef, IntUnOp, PlaceRef),
    Comp(PlaceRef, Comp, PlaceRef, PlaceRef),
    StoreVal(PlaceRef, Val),
    SetDiscriminant(PlaceRef, FieldName, bool),
    ApplyArgs(PlaceRef, PlaceRef, Vec<PlaceRef>, u64, ControlFlow),
    Copy(PlaceRef, PlaceRef, ControlFlow),
    ParseCall(
        Option<PlaceRef>,
        Option<PlaceRef>,
        RequirementSet,
        PlaceRef,
        PlaceRef,
        ControlFlow,
    ),
    Field(PlaceRef, PlaceRef, FieldName, ControlFlow),
    AssertVal(PlaceRef, ConstraintAtom, ControlFlow),
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
                | MirInstr::ApplyArgs(..)
                | MirInstr::Copy(..)
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
            | MirInstr::ParseCall(_, _, _, _, _, control_flow)
            | MirInstr::ApplyArgs(_, _, _, _, control_flow)
            | MirInstr::Copy(_, _, control_flow) => Some(*control_flow),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct BBRef(NonZeroU32);

impl BBRef {
    pub fn as_index(self) -> usize {
        u32::from(self.0) as usize - 1
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

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum DupleField {
    First,
    Second,
}

#[derive(Default, Clone, PartialEq, Eq, Hash, Debug)]
pub struct BasicBlock {
    ins: Vec<MirInstr>,
}

impl BasicBlock {
    pub fn append_ins(&mut self, ins: MirInstr) {
        if let Some(last_ins) = self.ins.last() {
            if last_ins.is_terminator() {
                panic!("Cannot append instruction to a block with a terminator");
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
    DupleField(PlaceRef, DupleField),
    From(PlaceRef),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum PlaceOrigin {
    Node(DefId),
    Ambient(BlockId, DefId),
    Expr(ExprId, SpanIndex),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct PlaceInfo {
    pub place: Place,
    pub ty: TypeId,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Function {
    bb: Vec<BasicBlock>,
    place: Vec<PlaceInfo>,
    stack: Vec<PlaceOrigin>,
    success_returns: Vec<BBRef>,
}

const fn const_ref(r: u32) -> PlaceRef {
    PlaceRef(match NonZeroU32::new(r) {
        Some(x) => x,
        None => panic!("ref is zero"),
    })
}

const CAP_REF: PlaceRef = const_ref(1);
const ARG_REF: PlaceRef = const_ref(2);
const RET_REF: PlaceRef = const_ref(3);
const RETLEN_REF: PlaceRef = const_ref(4);

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

    pub fn arg(&self) -> PlaceRef {
        ARG_REF
    }

    pub fn ret(&self) -> PlaceRef {
        RET_REF
    }

    pub fn retlen(&self) -> PlaceRef {
        RETLEN_REF
    }

    pub fn entry(&self) -> BBRef {
        BBRef(NonZeroU32::new(1).unwrap())
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
            let Some(controlflow) = bb_data.terminator().control_flow() else {continue};
            for succ in controlflow.successors() {
                preds[succ.as_index()].push(bb);
            }
        }
        preds
    }

    pub fn success_returns(&self) -> &[BBRef] {
        &self.success_returns
    }
}

pub struct FunctionWriter {
    fun: Function,
    place_map: FxHashMap<Place, PlaceRef>,
    current_bb: BBRef,
}

impl FunctionWriter {
    pub fn new(fun_ty: TypeId, arg_ty: TypeId, ret_ty: TypeId) -> Self {
        let fun = Function {
            bb: vec![Default::default()],
            place: Default::default(),
            stack: Default::default(),
            success_returns: Default::default(),
        };
        let mut builder = FunctionWriter {
            fun,
            place_map: Default::default(),
            current_bb: BBRef(NonZeroU32::new(1).unwrap()),
        };
        builder.add_place(PlaceInfo {
            place: Place::Captures,
            ty: fun_ty,
        });
        builder.add_place(PlaceInfo {
            place: Place::Arg,
            ty: arg_ty,
        });
        builder.add_place(PlaceInfo {
            place: Place::Return,
            ty: ret_ty,
        });
        builder.add_place(PlaceInfo {
            place: Place::ReturnLen,
            ty: arg_ty,
        });
        builder
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
        call_kind: RequirementSet,
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
                call_kind,
                arg,
                fun,
                ControlFlow::new_with_exc(new_block, exc),
            ));
        self.set_bb(new_block);
    }

    pub fn apply_args(
        &mut self,
        fun: PlaceRef,
        args: Vec<PlaceRef>,
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
}

fn mir_block(db: &dyn Mirs, block: BlockId, requirements: RequirementSet) -> SResult<Function> {
    let order = db.block_serialization(block).silence()?;
    let mut ctx = ConvertCtx::new_block_builder(db, block, requirements, &order)?;
    for value in order.eval_order.iter() {
        ctx.add_sub_value(value.val)?;
    }
    ctx.change_context(block.lookup(db)?.root_context);
    Ok(ctx.finish_fun())
}

fn mir_pd(db: &dyn Mirs, pd: ParserDefId, requirements: RequirementSet) -> SResult<Function> {
    let parserdef = pd.lookup(db)?;
    let mut ctx = ConvertCtx::new_parserdef_builder(db, pd, requirements)?;
    ctx.add_sub_value(SubValue::new_val(parserdef.to.0))?;
    ctx.parserdef(&parserdef);
    Ok(ctx.finish_fun())
}

#[cfg(test)]
mod tests {
    use super::*;
    use yaboc_ast::{import::Import, AstDatabase};
    use yaboc_base::{
        config::ConfigDatabase, interner::InternerDatabase, source::FileDatabase, Context,
    };
    use yaboc_dependents::{DependentsDatabase, NeededBy};
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
def for[int] *> main = {
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
                ctx.db.mir(FunKind::Block(*block), need.into()).unwrap();
            }
        }
        ctx.db
            .mir(FunKind::ParserDef(main), NeededBy::Val.into())
            .unwrap();
        ctx.db
            .mir(FunKind::ParserDef(main), NeededBy::Len.into())
            .unwrap();
    }
}
