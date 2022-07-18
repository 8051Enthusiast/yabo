#![allow(dead_code)]
mod convert;
mod represent;

use std::num::NonZeroU32;

use fxhash::FxHashMap;

use crate::{
    error::{SResult, Silencable},
    expr::{Atom, ValBinOp, ValUnOp},
    hir::{BlockId, HirIdWrapper, ParserDefId},
    interner::{DefId, FieldName},
    order::{Orders, SubValue, SubValueInfo},
    types::TypeId,
};

use self::convert::ConvertCtx;

pub use represent::print_all_mir;

#[salsa::query_group(MirDatabase)]
pub trait Mirs: Orders {
    fn mir_block(&self, block: BlockId, call_kind: CallKind) -> SResult<Function>;
    fn mir_pd_len(&self, pd: ParserDefId) -> SResult<Function>;
    fn mir_pd_val(&self, pd: ParserDefId) -> SResult<Function>;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
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
pub enum Val {
    Char(u32),
    Int(i64),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum MirInstr {
    IntBin(PlaceRef, IntBinOp, PlaceRef, PlaceRef),
    IntUn(PlaceRef, IntUnOp, PlaceRef),
    Comp(PlaceRef, Comp, PlaceRef, PlaceRef),
    LoadVal(PlaceRef, Val),
    Call(PlaceRef, CallKind, PlaceRef, PlaceRef, ExceptionRetreat),
    Field(PlaceRef, PlaceRef, FieldName, BBRef),
    AssertVal(PlaceRef, Atom, BBRef),
    SetDiscriminant(PlaceRef, FieldName, bool),
    Copy(PlaceRef, PlaceRef, BBRef),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct BBRef(NonZeroU32);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ExceptionRetreat {
    backtrack: BBRef,
    eof: BBRef,
    error: BBRef,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct PlaceRef(NonZeroU32);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct StackRef(NonZeroU32);

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

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum PlaceOrigin {
    None,
    Node(DefId),
    Expr(DefId, usize),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct PlaceInfo {
    place: Place,
    ty: TypeId,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Function {
    bb: Vec<BasicBlock>,
    place: Vec<PlaceInfo>,
    stack: Vec<()>,
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
    fn bb_mut(&mut self, bb: BBRef) -> &mut BasicBlock {
        &mut self.bb[u32::from(bb.0) as usize - 1]
    }

    pub fn place(&self, place_ref: PlaceRef) -> &PlaceInfo {
        &self.place[u32::from(place_ref.0) as usize - 1]
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

    pub fn new_stack_ref(&mut self) -> StackRef {
        self.fun.stack.push(());
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
    for value in order.eval_order.iter().filter(match call_kind {
        CallKind::Len => |x: &&SubValueInfo| x.rdepends_back,
        CallKind::Val => |x: &&SubValueInfo| x.rdepends_val,
    }) {
        ctx.add_sub_value(value.val)?;
    }
    ctx.change_context(block.lookup(db)?.root_context);
    Ok(ctx.finish_fun())
}

fn mir_pd_len(db: &dyn Mirs, pd: ParserDefId) -> SResult<Function> {
    let parserdef = pd.lookup(db)?;
    let mut ctx = ConvertCtx::new_parserdef_builder(db, pd, CallKind::Len)?;
    ctx.add_sub_value(SubValue::new_front(pd.0))?;
    ctx.add_sub_value(SubValue::new_val(parserdef.to.0))?;
    ctx.add_sub_value(SubValue::new_back(pd.0))?;
    Ok(ctx.finish_fun())
}

fn mir_pd_val(db: &dyn Mirs, pd: ParserDefId) -> SResult<Function> {
    let parserdef = pd.lookup(db)?;
    let mut ctx = ConvertCtx::new_parserdef_builder(db, pd, CallKind::Val)?;
    ctx.add_sub_value(SubValue::new_front(pd.0))?;
    ctx.add_sub_value(SubValue::new_val(parserdef.to.0))?;
    ctx.add_sub_value(SubValue::new_val(pd.0))?;
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
        ctx.db.mir_pd_val(main).unwrap();
        ctx.db.mir_pd_len(main).unwrap();
    }
}
