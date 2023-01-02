use fxhash::FxHashMap;
use inkwell::{
    basic_block::BasicBlock,
    values::{FunctionValue, IntValue, PointerValue},
};
use regex_automata::DFA;
use yaboc_dependents::RequirementSet;
use yaboc_hir_types::DerefLevel;
use yaboc_layout::{ILayout, IMonoLayout, MonoLayout};
use yaboc_mir::ReturnStatus;
use yaboc_types::{PrimitiveType, Type, TypeInterner};

use crate::{parser_args, CodeGenCtx};

pub struct RegexTranslator<'llvm, 'comp, 'r, D: DFA<ID = usize>> {
    cg: &'r mut CodeGenCtx<'llvm, 'comp>,
    llvm_fun: FunctionValue<'llvm>,
    dfa: &'r D,
    bt: BasicBlock<'llvm>,
    succ: BasicBlock<'llvm>,
    arg: PointerValue<'llvm>,
    ret: PointerValue<'llvm>,
    head: IntValue<'llvm>,
    tmp_ret: PointerValue<'llvm>,
    next_byte: PointerValue<'llvm>,
    greedy_info: Option<(PointerValue<'llvm>, PointerValue<'llvm>)>,
    stateblock: FxHashMap<usize, BasicBlock<'llvm>>,
    new_states: Vec<usize>,
    parser_fun: FunctionValue<'llvm>,
    from: ILayout<'comp>,
}

impl<'llvm, 'comp, 'r, D: DFA<ID = usize>> RegexTranslator<'llvm, 'comp, 'r, D> {
    pub fn new(
        cg: &'r mut CodeGenCtx<'llvm, 'comp>,
        llvm_fun: FunctionValue<'llvm>,
        dfa: &'r D,
        from: ILayout<'comp>,
        greedy: bool,
    ) -> Self {
        let int = cg
            .compiler_database
            .db
            .intern_type(Type::Primitive(PrimitiveType::Int));
        let single = IMonoLayout::int_single(cg.layouts);
        let req = RequirementSet::all();
        let slot =
            cg.collected_layouts.parser_slots.layout_vtable_offsets[&((from, req), single.inner())];
        let parser_fun = cg.parser_fun_val(single, slot, req);

        let int_layout = cg.layouts.dcx.intern(yaboc_layout::Layout::Mono(
            MonoLayout::Primitive(PrimitiveType::Int),
            int,
        ));

        let entry = cg.llvm.append_basic_block(llvm_fun, "entry");
        cg.builder.position_at_end(entry);
        let next_byte = cg.build_layout_alloca(int_layout, "next_byte");
        let tmp_ret = cg.build_layout_alloca(from, "tmp_ret");
        let greedy_info = if greedy {
            let no_match = cg.builder.build_alloca(cg.llvm.bool_type(), "has_matched");
            cg.builder
                .build_store(no_match, cg.llvm.bool_type().const_int(1, false));
            let last_match = cg.build_layout_alloca(from, "last_match");
            Some((no_match, last_match))
        } else {
            None
        };
        let bt = cg.llvm.append_basic_block(llvm_fun, "bt");
        let succ = cg.llvm.append_basic_block(llvm_fun, "succ");
        let stateblock = FxHashMap::default();
        let new_states = Vec::new();
        let (ret, _, head, arg) = parser_args(llvm_fun);
        Self {
            cg,
            llvm_fun,
            dfa,
            bt,
            succ,
            arg,
            ret,
            head,
            tmp_ret,
            next_byte,
            greedy_info,
            stateblock,
            new_states,
            parser_fun,
            from,
        }
    }

    fn copy_position(&mut self, dest: PointerValue<'llvm>, src: PointerValue<'llvm>) {
        self.cg.build_copy_invariant(dest, src, self.from)
    }

    fn state_bb(&mut self, state: usize) -> BasicBlock<'llvm> {
        if let Some(bb) = self.stateblock.get(&state) {
            *bb
        } else {
            let prev_bb = self.cg.builder.get_insert_block();
            let bb = self
                .cg
                .llvm
                .append_basic_block(self.llvm_fun, &format!("state{state}"));
            self.stateblock.insert(state, bb);
            self.new_states.push(state);
            if let Some(prev_bb) = prev_bb {
                self.cg.builder.position_at_end(prev_bb);
            }
            bb
        }
    }

    fn next_byte(&mut self) -> IntValue<'llvm> {
        let undef = self.cg.any_ptr().get_undef();
        let head = DerefLevel::zero().into_shifted_runtime_value();
        let ret = self.cg.builder.build_call(
            self.parser_fun,
            &[
                self.next_byte.into(),
                undef.into(),
                self.cg.const_i64(head as i64).into(),
                self.arg.into(),
            ],
            "next_ret",
        );
        let ret = ret.try_as_basic_value().left().unwrap().into_int_value();
        let ret_bb = self.cg.llvm.append_basic_block(self.llvm_fun, "ret");
        let cont_bb = self.cg.llvm.append_basic_block(self.llvm_fun, "cont");
        self.cg.builder.build_switch(
            ret,
            ret_bb,
            &[
                (self.cg.const_i64(ReturnStatus::Ok as i64), cont_bb),
                (self.cg.const_i64(ReturnStatus::Backtrack as i64), self.bt),
                (self.cg.const_i64(ReturnStatus::Eof as i64), self.bt),
            ],
        );
        self.cg.builder.position_at_end(ret_bb);
        self.cg.builder.build_return(Some(&ret));
        self.cg.builder.position_at_end(cont_bb);
        let next_byte = self.cg.build_cast::<*mut i64, _>(self.next_byte);
        self.cg
            .builder
            .build_load(next_byte, "next_byte")
            .into_int_value()
    }

    fn add_state(&mut self, state: usize) {
        let bb = self.state_bb(state);
        self.cg.builder.position_at_end(bb);
        if self.dfa.is_dead_state(state) {
            self.cg.builder.build_unconditional_branch(self.bt);
            return;
        }
        if self.dfa.is_match_state(state) {
            if let Some((has_no_match, last_match)) = self.greedy_info {
                self.cg
                    .builder
                    .build_store(has_no_match, self.cg.llvm.bool_type().const_zero());
                self.copy_position(last_match, self.arg);
            } else {
                self.cg.builder.build_unconditional_branch(self.succ);
                return;
            }
        }
        let next_byte = self.next_byte();
        let mut next_bbs = Vec::new();
        for possible_byte in 0..=255 {
            let next_state = self.dfa.next_state(state, possible_byte);
            let next_bb = self.state_bb(next_state);
            let llvm_int = self.cg.const_i64(possible_byte as i64);
            next_bbs.push((llvm_int, next_bb));
        }
        self.cg.builder.build_switch(next_byte, self.bt, &next_bbs);
    }

    fn add_succ_block(&mut self) {
        self.cg.builder.position_at_end(self.succ);
        if let Some((_, last_match)) = self.greedy_info {
            self.copy_position(self.arg, last_match);
        }
        let spanner = self.cg.build_span_fun_get(self.from.maybe_mono(), self.arg);
        let ret = self.cg.builder.build_call(
            spanner,
            &[
                self.ret.into(),
                self.tmp_ret.into(),
                self.head.into(),
                self.arg.into(),
            ],
            "span",
        );
        let ret = ret.try_as_basic_value().left().unwrap().into_int_value();
        self.cg.builder.build_return(Some(&ret));
    }

    fn add_bt_block(&mut self) {
        self.cg.builder.position_at_end(self.bt);
        if let Some((no_match, _)) = self.greedy_info {
            let no_match_block = self.cg.llvm.append_basic_block(self.llvm_fun, "no_match");
            let no_match_bool = self
                .cg
                .builder
                .build_load(no_match, "no_match_bool")
                .into_int_value();
            self.cg
                .builder
                .build_conditional_branch(no_match_bool, no_match_block, self.succ);
            self.cg.builder.position_at_end(no_match_block);
        }
        self.cg
            .builder
            .build_return(Some(&self.cg.const_i64(ReturnStatus::Backtrack as i64)));
    }

    pub fn build(&mut self) {
        self.copy_position(self.tmp_ret, self.arg);
        let start_state_block = self.state_bb(self.dfa.start_state());
        self.cg
            .builder
            .build_unconditional_branch(start_state_block);
        self.add_succ_block();
        self.add_bt_block();
        while let Some(state) = self.new_states.pop() {
            self.add_state(state);
        }
    }
}