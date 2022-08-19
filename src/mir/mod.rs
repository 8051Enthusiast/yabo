mod convert;
mod represent;

use std::num::NonZeroU32;

use fxhash::FxHashMap;

use crate::{
    error::{SResult, Silencable},
    expr::{Atom, ValBinOp, ValUnOp},
    hir::{BlockId, ExprId, HirIdWrapper, ParserDefId},
    interner::{DefId, FieldName},
    order::{Orders, SubValue},
    source::SpanIndex,
    types::{Type, TypeId}, dbeprintln,
};

use self::convert::ConvertCtx;

pub use represent::print_all_mir;

#[salsa::query_group(MirDatabase)]
pub trait Mirs: Orders {
    fn mir_block(&self, block: BlockId, call_kind: CallKind) -> SResult<Function>;
    fn mir_pd(
        &self,
        pd: ParserDefId,
        call_kind: CallKind,
        arg_kind: PdArgKind,
    ) -> SResult<Function>;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
#[repr(i64)]
pub enum ReturnStatus {
    Ok,
    Error,
    Eof,
    Backtrack,
}

#[derive(Default, Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum BlockExit {
    #[default]
    BlockInProgress,
    Jump(BBRef),
    Return(ReturnStatus),
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
pub enum CallKind {
    Len,
    Val,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum PdArgKind {
    Thunk,
    Parse,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Val {
    Char(u32),
    Int(i64),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum MirInstr {
    IntBin(PlaceRef, IntBinOp, PlaceRef, PlaceRef),
    IntUn(PlaceRef, IntUnOp, PlaceRef),
    Comp(PlaceRef, Comp, PlaceRef, PlaceRef),
    StoreVal(PlaceRef, Val),
    Call(PlaceRef, CallKind, PlaceRef, PlaceRef, ExceptionRetreat),
    Field(PlaceRef, PlaceRef, FieldName, BBRef),
    AssertVal(PlaceRef, Atom, BBRef),
    SetDiscriminant(PlaceRef, FieldName, bool),
    Copy(PlaceRef, PlaceRef, BBRef),
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
    exit: BlockExit,
}

impl BasicBlock {
    pub fn append_ins(&mut self, ins: MirInstr) {
        self.ins.push(ins)
    }
    pub fn ins(&self) -> impl Iterator<Item = MirInstr> + '_ {
        self.ins.iter().cloned()
    }
    pub fn exit(&self) -> BlockExit {
        self.exit
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Place {
    Captures,
    Arg,
    Return,
    Stack(StackRef),
    Field(PlaceRef, DefId),
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

    pub fn entry(&self) -> BBRef {
        BBRef(NonZeroU32::new(1).unwrap())
    }

    pub fn is_valid(&self) -> bool {
        self.bb
            .iter()
            .all(|x| matches!(x.exit, BlockExit::BlockInProgress))
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
                self.place[x].clone(),
            )
        })
    }

    pub fn iter_bb(&self) -> impl Iterator<Item = (BBRef, &BasicBlock)> + '_ {
        (0..self.bb.len()).map(|x| (BBRef(NonZeroU32::new((x + 1) as u32).unwrap()), &self.bb[x]))
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
        builder
    }

    pub fn make_top_level_retreat(&mut self) -> ExceptionRetreat {
        let backtrack = self.new_bb();
        self.set_bb(backtrack);
        self.set_return(ReturnStatus::Backtrack);
        let error = self.new_bb();
        self.set_bb(error);
        self.set_return(ReturnStatus::Error);
        let eof = self.new_bb();
        self.set_bb(eof);
        self.set_return(ReturnStatus::Eof);
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

    pub fn set_jump(&mut self, bb: BBRef) {
        self.fun.bb_mut(self.current_bb).exit = BlockExit::Jump(bb)
    }

    pub fn set_return(&mut self, status: ReturnStatus) {
        self.fun.bb_mut(self.current_bb).exit = BlockExit::Return(status)
    }
}

fn mir_block(db: &dyn Mirs, block: BlockId, call_kind: CallKind) -> SResult<Function> {
    let order = db.block_serialization(block).silence()?;
    let mut ctx = ConvertCtx::new_block_builder(db, block, call_kind, &order)?;
    for value in order.eval_order.iter() {
        let x = value.val;
        dbeprintln!(db, "{}", &x);
        ctx.add_sub_value(value.val)?;
    }
    ctx.change_context(block.lookup(db)?.root_context);
    Ok(ctx.finish_fun())
}

fn mir_pd_val_parser(db: &dyn Mirs, pd: ParserDefId) -> SResult<Function> {
    let sig = db.parser_args(pd)?;
    let arg_ty = sig.from.unwrap_or(db.intern_type(Type::Any));
    let fun_ty = db.intern_type(Type::ParserArg {
        result: sig.thunk,
        arg: arg_ty,
    });
    let ret_ty = sig.thunk;
    let mut writer = FunctionWriter::new(fun_ty, arg_ty, ret_ty);
    let error = writer.new_bb();
    writer.set_bb(error);
    writer.set_return(ReturnStatus::Error);
    writer.set_bb(writer.fun.entry());
    let ret_place = writer.fun.ret();
    let ret_place_from = writer.add_place(PlaceInfo {
        place: Place::From(ret_place),
        ty: arg_ty,
    });
    let arg_place = writer.fun.arg();
    writer.append_ins(MirInstr::Copy(ret_place_from, arg_place, error));
    writer.set_return(ReturnStatus::Ok);
    Ok(writer.fun)
}

fn mir_pd(
    db: &dyn Mirs,
    pd: ParserDefId,
    call_kind: CallKind,
    arg_kind: PdArgKind,
) -> SResult<Function> {
    if call_kind == CallKind::Val && arg_kind == PdArgKind::Parse {
        return mir_pd_val_parser(db, pd);
    }
    let parserdef = pd.lookup(db)?;
    let mut ctx = ConvertCtx::new_parserdef_builder(db, pd, call_kind, arg_kind)?;
    ctx.add_sub_value(SubValue::new_front(pd.0))?;
    ctx.add_sub_value(SubValue::new_val(parserdef.to.0))?;
    match call_kind {
        CallKind::Val => ctx.add_sub_value(SubValue::new_val(pd.0))?,
        CallKind::Len => ctx.add_sub_value(SubValue::new_back(pd.0))?,
    }
    Ok(ctx.finish_fun())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::Context;
    use crate::hir::Hirs;
    use crate::mir::Mirs;

    #[test]
    fn basic_mir() {
        let ctx = Context::mock(
            r"
def for[int] *> main = {
    a: ~,
    b: ~,
    e: {
        let y: int = x + b,
        x: ~,
    },
    ;
    b: ~,
    a: ~,
    c: ~,
    let d: int = b + c * a * a,
}
        ",
        );
        let main = ctx.parser("main");
        let blocks = ctx.db.all_parserdef_blocks(main);
        for call_kind in [CallKind::Val, CallKind::Len] {
            for block in blocks.iter() {
                ctx.db.mir_block(*block, call_kind).unwrap();
            }
        }
        ctx.db
            .mir_pd(main, CallKind::Val, PdArgKind::Thunk)
            .unwrap();
        ctx.db
            .mir_pd(main, CallKind::Len, PdArgKind::Parse)
            .unwrap();
    }
}
