use fxhash::FxHashMap;
use inkwell::{
    basic_block::BasicBlock,
    values::{FunctionValue, IntValue, PointerValue},
};
use regex_automata::{dfa::dense::DFA, util::primitives::StateID};
use regex_automata::{dfa::Automaton, util::start::Config};
use yaboc_hir_types::DerefLevel;
use yaboc_layout::{ILayout, IMonoLayout, MonoLayout};
use yaboc_mir::{CallMeta, ReturnStatus};
use yaboc_req::RequirementSet;
use yaboc_types::{PrimitiveType, Type, TypeInterner};

use crate::{
    parser_args,
    val::{CgReturnValue, CgValue},
    CodeGenCtx, IResult,
};

pub struct RegexTranslator<'llvm, 'comp, 'r> {
    cg: &'r mut CodeGenCtx<'llvm, 'comp>,
    llvm_fun: FunctionValue<'llvm>,
    dfa: &'r DFA<Vec<u32>>,
    eof_fail: BasicBlock<'llvm>,
    succ: BasicBlock<'llvm>,
    ret: CgReturnValue<'llvm>,
    input_start: CgValue<'comp, 'llvm>,
    next_byte: CgValue<'comp, 'llvm>,
    next_byte_pos: CgValue<'comp, 'llvm>,
    has_no_match: PointerValue<'llvm>,
    last_match: CgValue<'comp, 'llvm>,
    stateblock: FxHashMap<StateID, BasicBlock<'llvm>>,
    new_states: Vec<StateID>,
    parser_fun: FunctionValue<'llvm>,
    active_pos: CgValue<'comp, 'llvm>,
    retlen: CgValue<'comp, 'llvm>,
}

impl<'llvm, 'comp, 'r> RegexTranslator<'llvm, 'comp, 'r> {
    pub fn new(
        cg: &'r mut CodeGenCtx<'llvm, 'comp>,
        llvm_fun: FunctionValue<'llvm>,
        dfa: &'r DFA<Vec<u32>>,
        retlen: ILayout<'comp>,
    ) -> IResult<Self> {
        let int = cg
            .compiler_database
            .db
            .intern_type(Type::Primitive(PrimitiveType::Int));
        let single = IMonoLayout::u8_single(cg.layouts);
        let info = CallMeta::new(RequirementSet::all(), false);
        let parser_fun = cg.parser_fun_val_tail(single, retlen, info.req);

        let int_layout = cg.layouts.dcx.intern(yaboc_layout::Layout::Mono(
            MonoLayout::Primitive(PrimitiveType::Int),
            int,
        ));

        cg.add_entry_block(llvm_fun);
        let next_byte = cg.build_alloca_value(int_layout, "next_byte")?;
        let input_start = cg.build_alloca_value(retlen, "input_start")?;
        let next_byte_pos = cg.build_alloca_value(retlen, "next_byte_pos")?;
        let last_match = cg.build_alloca_value(retlen, "last_match")?;
        let from = cg.build_alloca_value(retlen, "from")?;
        let has_no_match = cg
            .builder
            .build_alloca(cg.llvm.bool_type(), "has_no_match")?;
        cg.builder
            .build_store(has_no_match, cg.llvm.bool_type().const_int(1, false))?;
        let eof_fail = cg.llvm.append_basic_block(llvm_fun, "eof_fail");
        let succ = cg.llvm.append_basic_block(llvm_fun, "succ");
        let stateblock = FxHashMap::default();
        let new_states = Vec::new();
        let (ret, _, head, arg) = parser_args(llvm_fun);
        let retlen = CgValue::new(retlen, arg);
        let ret = CgReturnValue::new(head, ret);
        Ok(Self {
            cg,
            llvm_fun,
            dfa,
            eof_fail,
            succ,
            ret,
            input_start,
            next_byte_pos,
            next_byte,
            last_match,
            has_no_match,
            stateblock,
            new_states,
            parser_fun,
            active_pos: from,
            retlen,
        })
    }

    fn copy_position(
        &mut self,
        dest: CgValue<'comp, 'llvm>,
        src: CgValue<'comp, 'llvm>,
    ) -> IResult<()> {
        self.cg.build_copy_invariant(dest, src)?;
        Ok(())
    }

    fn state_bb(&mut self, state: StateID) -> BasicBlock<'llvm> {
        if let Some(bb) = self.stateblock.get(&state) {
            *bb
        } else {
            let prev_bb = self.cg.builder.get_insert_block();
            let bb = self
                .cg
                .llvm
                .append_basic_block(self.llvm_fun, &format!("state{}", state.as_u64()));
            self.stateblock.insert(state, bb);
            self.new_states.push(state);
            if let Some(prev_bb) = prev_bb {
                self.cg.builder.position_at_end(prev_bb);
            }
            bb
        }
    }

    fn next_byte(&mut self, eoi_state: StateID) -> IResult<IntValue<'llvm>> {
        self.copy_position(self.active_pos, self.next_byte_pos)?;
        let undef = self.cg.invalid_ptr();
        let head = DerefLevel::zero().into_shifted_runtime_value();
        let ret = self.cg.builder.build_call(
            self.parser_fun,
            &[
                self.next_byte.ptr.into(),
                // the single parser is a zero-sized type
                undef.into(),
                self.cg.const_i64(head as i64).into(),
                self.next_byte_pos.ptr.into(),
            ],
            "next_ret",
        )?;
        ret.set_call_convention(self.cg.tailcc());
        let ret = ret.try_as_basic_value().left().unwrap().into_int_value();
        let ret_bb = self.cg.llvm.append_basic_block(self.llvm_fun, "ret");
        let cont_bb = self.cg.llvm.append_basic_block(self.llvm_fun, "cont");
        let bt_bb = if self.dfa.is_match_state(eoi_state) {
            self.succ
        } else {
            self.eof_fail
        };
        self.cg.builder.build_switch(
            ret,
            ret_bb,
            &[
                (self.cg.const_i64(ReturnStatus::Ok as i64), cont_bb),
                (self.cg.const_i64(ReturnStatus::Eof as i64), bt_bb),
            ],
        )?;
        self.cg.builder.position_at_end(ret_bb);
        self.cg.builder.build_return(Some(&ret))?;
        self.cg.builder.position_at_end(cont_bb);
        self.cg.build_i64_load(self.next_byte.ptr, "next_byte")
    }

    fn most_followed_state(&self, state: StateID) -> StateID {
        let mut counts = FxHashMap::default();
        for possible_byte in 0..=255 {
            let next_state = self.dfa.next_state(state, possible_byte);
            let count = counts.entry(next_state).or_insert(0);
            *count += 1;
        }
        counts
            .into_iter()
            .max_by_key(|(_, count)| *count)
            .map(|(state, _)| state)
            .unwrap()
    }

    fn add_state(&mut self, state: StateID) -> IResult<()> {
        let bb = self.state_bb(state);
        self.cg.builder.position_at_end(bb);
        if self.dfa.is_dead_state(state) {
            self.cg.builder.build_unconditional_branch(self.eof_fail)?;
            return Ok(());
        }
        if self.dfa.is_match_state(state) {
            self.cg
                .builder
                .build_store(self.has_no_match, self.cg.llvm.bool_type().const_zero())?;
            self.copy_position(self.last_match, self.active_pos)?;
        }
        let next_byte = self.next_byte(self.dfa.next_eoi_state(state))?;
        let mut next_bbs = Vec::new();
        let most_followed = self.most_followed_state(state);
        for possible_byte in 0..=255 {
            let next_state = self.dfa.next_state(state, possible_byte);
            if next_state == most_followed {
                continue;
            }
            let next_bb = self.state_bb(next_state);
            let llvm_int = self.cg.const_i64(possible_byte as i64);
            next_bbs.push((llvm_int, next_bb));
        }
        let most_follow_bb = self.state_bb(most_followed);
        self.cg
            .builder
            .build_switch(next_byte, most_follow_bb, &next_bbs)?;
        Ok(())
    }

    fn add_succ_block(&mut self) -> IResult<()> {
        self.cg.builder.position_at_end(self.succ);
        self.copy_position(self.retlen, self.active_pos)?;
        let ret = self
            .cg
            .call_span_fun(self.ret, self.input_start, self.active_pos)?;
        self.cg.builder.build_return(Some(&ret))?;
        Ok(())
    }

    fn add_eof_fail_block(&mut self) -> IResult<()> {
        self.cg.builder.position_at_end(self.eof_fail);
        let no_match = self
            .cg
            .builder
            .build_load(self.cg.llvm.bool_type(), self.has_no_match, "no_match_bool")?
            .into_int_value();
        self.cg.branch(
            no_match,
            |cg| {
                cg.builder
                    .build_return(Some(&cg.const_i64(ReturnStatus::Backtrack as i64)))
            },
            |_| Ok(()),
        )?;
        self.copy_position(self.active_pos, self.last_match)?;
        self.cg.builder.build_unconditional_branch(self.succ)?;
        Ok(())
    }

    pub fn build(&mut self) -> IResult<()> {
        self.copy_position(self.input_start, self.retlen)?;
        self.copy_position(self.next_byte_pos, self.retlen)?;
        let config = Config::new().anchored(regex_automata::Anchored::Yes);
        let start_state_block = self.state_bb(self.dfa.start_state(&config).unwrap());
        self.cg
            .builder
            .build_unconditional_branch(start_state_block)?;
        self.add_succ_block()?;
        self.add_eof_fail_block()?;
        while let Some(state) = self.new_states.pop() {
            self.add_state(state)?;
        }
        Ok(())
    }
}
