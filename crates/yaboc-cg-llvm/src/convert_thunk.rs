use inkwell::{
    basic_block::BasicBlock,
    values::{FunctionValue, IntValue, PhiValue, PointerValue},
    IntPredicate,
};

use yaboc_hir_types::DISCRIMINANT_MASK;
use yaboc_layout::{
    prop::{PSize, SizeAlign, TargetSized},
    IMonoLayout,
};

use super::CodeGenCtx;

pub enum ThunkKind<'comp> {
    Typecast(IMonoLayout<'comp>),
    CreateArgs {
        from: IMonoLayout<'comp>,
        to: IMonoLayout<'comp>,
        slot: PSize,
    },
}

impl<'comp> ThunkKind<'comp> {
    pub fn copy_size<'llvm>(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> SizeAlign {
        let size_layout = match self {
            ThunkKind::Typecast(layout) => layout,
            ThunkKind::CreateArgs { from, .. } => from,
        };
        size_layout.inner().size_align(cg.layouts).unwrap()
    }
    pub fn alloc_size<'llvm>(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> SizeAlign {
        let size_layout = match self {
            ThunkKind::Typecast(layout) => layout,
            ThunkKind::CreateArgs { to, .. } => to,
        };
        size_layout.inner().size_align(cg.layouts).unwrap()
    }
    pub fn function<'llvm>(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> FunctionValue<'llvm> {
        match self {
            ThunkKind::Typecast(layout) => cg.typecast_fun_val(*layout),
            ThunkKind::CreateArgs { from, slot, .. } => {
                cg.function_create_args_fun_val(*from, *slot)
            }
        }
    }
    pub fn target_layout(&self) -> IMonoLayout<'comp> {
        match self {
            ThunkKind::Typecast(layout) => *layout,
            ThunkKind::CreateArgs { to, .. } => *to,
        }
    }
}

pub struct ThunkContext<'llvm, 'comp, 'r> {
    cg: &'r mut CodeGenCtx<'llvm, 'comp>,
    thunk: FunctionValue<'llvm>,
    target_head: IntValue<'llvm>,
    from_ptr: PointerValue<'llvm>,
    target_layout: IMonoLayout<'comp>,
    return_ptr: PointerValue<'llvm>,
    copy_sa: SizeAlign,
    alloc_sa: SizeAlign,
}

impl<'llvm, 'comp, 'r> ThunkContext<'llvm, 'comp, 'r> {
    pub fn new(cg: &'r mut CodeGenCtx<'llvm, 'comp>, kind: ThunkKind<'comp>) -> Self {
        let thunk = kind.function(cg);
        let copy_sa = kind.copy_size(cg);
        let alloc_sa = kind.alloc_size(cg);
        let from_ptr = thunk.get_nth_param(0).unwrap().into_pointer_value();
        let target_head = thunk.get_nth_param(1).unwrap().into_int_value();
        let return_ptr = thunk.get_nth_param(2).unwrap().into_pointer_value();
        let entry_block = cg.llvm.append_basic_block(thunk, "entry");
        let target_layout = kind.target_layout();
        cg.builder.position_at_end(entry_block);
        ThunkContext {
            cg,
            thunk,
            target_head,
            from_ptr,
            target_layout,
            return_ptr,
            copy_sa,
            alloc_sa,
        }
    }

    fn maybe_deref(&mut self) {
        if self.target_layout.deref(self.cg.layouts).unwrap().is_some() {
            let mask = self.cg.const_i64(DISCRIMINANT_MASK);
            let discriminant = self
                .cg
                .build_head_disc_get(Some(self.target_layout), self.from_ptr);
            let masked_target = self
                .cg
                .builder
                .build_and(self.target_head, mask, "masked_target");
            let heads_equal = self.cg.builder.build_int_compare(
                IntPredicate::EQ,
                discriminant,
                masked_target,
                "heads_equal",
            );
            let heads_zero = self.cg.builder.build_int_compare(
                IntPredicate::EQ,
                self.cg.const_i64(0),
                masked_target,
                "heads_zero",
            );
            let no_deref = self
                .cg
                .builder
                .build_or(heads_equal, heads_zero, "no_deref");
            let tail = self.typecast_tail();
            let next_bb = self.cg.llvm.append_basic_block(self.thunk, "head_match");
            self.cg
                .builder
                .build_conditional_branch(no_deref, next_bb, tail);
            self.cg.builder.position_at_end(next_bb);
        }
    }
    fn typecast_tail(&mut self) -> BasicBlock<'llvm> {
        let previous_bb = self.cg.builder.get_insert_block();
        let current_bb = self.cg.llvm.append_basic_block(self.thunk, "typecast_tail");

        self.cg.builder.position_at_end(current_bb);
        let deref_fun = self
            .cg
            .build_deref_fun_get(Some(self.target_layout), self.from_ptr);
        let ret = self.cg.build_call_with_int_ret(
            deref_fun,
            &[
                self.from_ptr.into(),
                self.target_head.into(),
                self.return_ptr.into(),
            ],
        );
        self.cg.builder.build_return(Some(&ret));
        if let Some(bb) = previous_bb {
            self.cg.builder.position_at_end(bb);
        }
        current_bb
    }

    fn copy_to_target(&mut self) {
        let copy_bb = self.cg.llvm.append_basic_block(self.thunk, "copy");
        let old_bb = self.cg.builder.get_insert_block().unwrap();

        self.cg.builder.position_at_end(copy_bb);
        let copy_phi = self.cg.builder.build_phi(self.cg.any_ptr(), "copy_phi");
        let check_vtable_bb = self.cg.llvm.append_basic_block(self.thunk, "check_vtable");

        self.cg.builder.position_at_end(check_vtable_bb);
        self.check_vtable(copy_bb, copy_phi, copy_bb);
        copy_phi.add_incoming(&[(&self.return_ptr, check_vtable_bb)]);

        self.cg.builder.position_at_end(old_bb);
        if self.alloc_sa.size > <*const u8>::tsize().size {
            self.check_malloc(copy_bb, copy_phi, check_vtable_bb);
        } else {
            self.cg.builder.build_unconditional_branch(check_vtable_bb);
        }

        self.cg.builder.position_at_end(copy_bb);
        let target_ptr = copy_phi.as_basic_value().into_pointer_value();
        if self.copy_sa.size > 0 {
            // it is nice to be able to pass an invalid pointer
            // for ZSTs, but calling memcpy with a null pointer
            // is UB, therefore we simply don't generate it for ZSTs
            self.cg
                .builder
                .build_memcpy(
                    target_ptr,
                    self.copy_sa.align() as u32,
                    self.from_ptr,
                    self.alloc_sa.align() as u32,
                    self.cg.const_size_t(self.copy_sa.size),
                )
                .unwrap();
        }
        self.cg.builder.build_return(Some(&self.cg.const_i64(0)));
    }

    fn check_vtable(
        &mut self,
        copy_bb: BasicBlock<'llvm>,
        copy_phi: PhiValue<'llvm>,
        otherwise: BasicBlock<'llvm>,
    ) {
        let has_vtable = self.cg.build_check_i64_bit_set(self.target_head, 0);
        let write_vtable_ptr = self
            .cg
            .llvm
            .append_basic_block(self.thunk, "write_vtable_ptr");
        self.cg
            .builder
            .build_conditional_branch(has_vtable, write_vtable_ptr, otherwise);
        self.cg.builder.position_at_end(write_vtable_ptr);
        let vtable_pointer = self.build_vtable_any_ptr();
        let ret_vtable_ptr = self.cg.build_cast::<*mut *const u8, _>(self.return_ptr);
        self.cg.builder.build_store(ret_vtable_ptr, vtable_pointer);
        let ptr_width = self.cg.any_ptr().size_of();
        let after_ptr = self
            .cg
            .build_byte_gep(self.return_ptr, ptr_width, "vtable_ptr_skip");
        copy_phi.add_incoming(&[(&after_ptr, write_vtable_ptr)]);
        self.cg.builder.build_unconditional_branch(copy_bb);
    }

    fn check_malloc(
        &mut self,
        copy_bb: BasicBlock<'llvm>,
        copy_phi: PhiValue<'llvm>,
        otherwise: BasicBlock<'llvm>,
    ) {
        let is_malloc = self.cg.build_check_i64_bit_set(self.target_head, 1);
        let allocator_bb = self.cg.llvm.append_basic_block(self.thunk, "allocator");
        self.cg
            .builder
            .build_conditional_branch(is_malloc, allocator_bb, otherwise);

        self.cg.builder.position_at_end(allocator_bb);
        let vtable_pointer = self.build_vtable_any_ptr();
        let tagged_vtable_ptr = self.build_malloc_tag_ptr(vtable_pointer);
        let malloc_pointer = self.build_malloc();
        let target_as_ptr_ptr = self.cg.build_cast::<*mut *mut u8, _>(self.return_ptr);
        self.cg
            .builder
            .build_store(target_as_ptr_ptr, tagged_vtable_ptr);
        let second = unsafe {
            self.cg.builder.build_in_bounds_gep(
                target_as_ptr_ptr,
                &[self.cg.const_size_t(1)],
                "second",
            )
        };
        self.cg.builder.build_store(second, malloc_pointer);
        copy_phi.add_incoming(&[(&malloc_pointer, allocator_bb)]);
        self.cg.builder.build_unconditional_branch(copy_bb);
    }

    fn build_malloc_tag_ptr(&mut self, ptr: PointerValue<'llvm>) -> PointerValue<'llvm> {
        let int_type = self.cg.llvm.ptr_sized_int_type(&self.cg.target_data, None);
        let vtable_ptr_int = self
            .cg
            .builder
            .build_ptr_to_int(ptr, int_type, "vtable_ptr_int");
        let tagged_vtable_ptr_int = self.cg.builder.build_or(
            vtable_ptr_int,
            self.cg.const_size_t(1),
            "tagged_vtable_ptr_int",
        );
        self.cg
            .builder
            .build_int_to_ptr(tagged_vtable_ptr_int, self.cg.any_ptr(), "tagged_ptr")
    }

    fn build_malloc(&mut self) -> PointerValue<'llvm> {
        let sa = self.alloc_sa;
        let ty = self.cg.sa_type(sa);
        let malloc_ptr = self.cg.builder.build_malloc(ty, "benloc").unwrap();
        self.cg.set_last_instr_align(sa);
        self.cg.build_cast::<*mut u8, _>(malloc_ptr)
    }

    fn build_vtable_any_ptr(&mut self) -> PointerValue<'llvm> {
        self.cg.build_get_vtable_tag(self.target_layout)
    }

    pub fn build(mut self) -> FunctionValue<'llvm> {
        self.maybe_deref();
        self.copy_to_target();
        self.thunk
    }
}
