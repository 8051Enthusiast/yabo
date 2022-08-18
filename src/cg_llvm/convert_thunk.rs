use inkwell::{
    basic_block::BasicBlock,
    values::{FunctionValue, InstructionValue, IntValue, PhiValue, PointerValue},
    IntPredicate,
};

use crate::{
    hir_types::DISCRIMINANT_MASK,
    layout::{prop::TargetSized, represent::LayoutPart, ILayout, IMonoLayout},
};

use super::CodeGenCtx;

pub struct ThunkContext<'llvm, 'comp, 'r> {
    cg: &'r mut CodeGenCtx<'llvm, 'comp>,
    thunk: FunctionValue<'llvm>,
    alloca_ins: InstructionValue<'llvm>,
    deref_fun: Option<FunctionValue<'llvm>>,
    target_head: IntValue<'llvm>,
    from_ptr: PointerValue<'llvm>,
    from_layout: IMonoLayout<'comp>,
    return_ptr: PointerValue<'llvm>,
}

impl<'llvm, 'comp, 'r> ThunkContext<'llvm, 'comp, 'r> {
    pub fn new(
        cg: &'r mut CodeGenCtx<'llvm, 'comp>,
        layout: IMonoLayout<'comp>,
        deref_fun: Option<FunctionValue<'llvm>>,
    ) -> Self {
        let thunk = cg.typecast_fun_val(layout);
        let from_ptr = thunk.get_nth_param(0).unwrap().into_pointer_value();
        let target_head = thunk.get_nth_param(1).unwrap().into_int_value();
        let return_ptr = thunk.get_nth_param(2).unwrap().into_pointer_value();
        let alloca_block = cg.llvm.append_basic_block(thunk, "entry");
        let start_block = cg.llvm.append_basic_block(thunk, "start");
        cg.builder.position_at_end(alloca_block);
        let alloca_ins = cg.builder.build_unconditional_branch(start_block);
        cg.builder.position_at_end(start_block);
        ThunkContext {
            cg,
            thunk,
            alloca_ins,
            deref_fun,
            target_head,
            from_ptr,
            from_layout: layout,
            return_ptr,
        }
    }

    fn maybe_deref(&mut self) {
        if let Some(deref_layout) = self.from_layout.deref(self.cg.layouts).unwrap() {
            let mask = self.cg.const_i64(DISCRIMINANT_MASK);
            let discriminant = self
                .cg
                .build_head_disc_get(Some(self.from_layout), self.from_ptr);
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
            let tail = self.typecast_tail(self.deref_fun.unwrap(), deref_layout);
            let next_bb = self.cg.llvm.append_basic_block(self.thunk, "head_match");
            self.cg
                .builder
                .build_conditional_branch(no_deref, next_bb, tail);
            self.cg.builder.position_at_end(next_bb);
        }
    }
    fn typecast_tail(
        &mut self,
        deref_fun: FunctionValue<'llvm>,
        deref_layout: ILayout<'comp>,
    ) -> BasicBlock<'llvm> {
        let previous_bb = self.cg.builder.get_insert_block();
        let current_bb = self.cg.llvm.append_basic_block(self.thunk, "typecast_tail");

        self.cg.builder.position_before(&self.alloca_ins);
        let deref_ret_ptr = self.cg.build_layout_alloca(deref_layout);

        self.cg.builder.position_at_end(current_bb);
        let deref_status = self.cg.build_ptr_call_with_int_ret(
            deref_fun.as_global_value().as_pointer_value(),
            &[self.from_ptr.into(), deref_ret_ptr.into()],
        );
        self.cg.non_zero_early_return(self.thunk, deref_status);

        let deref_typecast_fun = self
            .cg
            .build_typecast_fun_get(deref_layout.maybe_mono(), deref_ret_ptr);
        let ret = self.cg.build_call_with_int_ret(
            deref_typecast_fun,
            &[
                deref_ret_ptr.into(),
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
        copy_phi.add_incoming(&[(&self.from_ptr, check_vtable_bb)]);

        self.cg.builder.position_at_end(old_bb);
        let layout_size = self
            .from_layout
            .inner()
            .size_align(self.cg.layouts)
            .unwrap()
            .size;
        if layout_size > <*const u8>::tsize().size {
            self.check_malloc(copy_bb, copy_phi, check_vtable_bb);
        } else {
            self.cg.builder.build_unconditional_branch(check_vtable_bb);
        }

        self.cg.builder.position_at_end(copy_bb);
        let from_ptr = copy_phi.as_basic_value().into_pointer_value();
        let align = self
            .from_layout
            .inner()
            .size_align(self.cg.layouts)
            .unwrap()
            .align() as u32;
        self.cg
            .builder
            .build_memcpy(
                self.return_ptr,
                align,
                from_ptr,
                align,
                self.cg.const_size_t(layout_size),
            )
            .unwrap();
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
        let ret_vtable_ptr = self.cg.build_cast::<*mut *const u8, _>(self.from_ptr);
        self.cg.builder.build_store(ret_vtable_ptr, vtable_pointer);
        let ptr_width = self.cg.any_ptr().size_of();
        let after_ptr = unsafe {
            self.cg
                .builder
                .build_in_bounds_gep(self.from_ptr, &[ptr_width], "vtable_ptr_skip")
        };
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
        self.cg.builder.build_store(target_as_ptr_ptr, tagged_vtable_ptr);
        let second = unsafe {
            self.cg.builder.build_in_bounds_gep(target_as_ptr_ptr, &[self.cg.const_size_t(1)],"second")
        };
        self.cg.builder.build_store(second, malloc_pointer);
        copy_phi.add_incoming(&[(&malloc_pointer, allocator_bb)]);
        self.cg.builder.build_unconditional_branch(copy_bb);
    }

    fn build_malloc_tag_ptr(&mut self, ptr: PointerValue<'llvm>) -> PointerValue<'llvm> {
        let int_type = self
            .cg
            .llvm
            .ptr_sized_int_type(&self.cg.target_data, None);
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
        let sa = self
            .from_layout
            .inner()
            .size_align(self.cg.layouts)
            .unwrap();
        let ty = self.cg.sa_type(sa);
        let malloc_ptr = self.cg.builder.build_malloc(ty, "benloc").unwrap();
        self.cg.build_cast::<*mut u8, _>(malloc_ptr)
    }

    fn build_vtable_any_ptr(&mut self) -> PointerValue<'llvm> {
        let ptr = self.cg.sym_ptr(self.from_layout, LayoutPart::VTable);
        self.cg.build_cast::<*const u8, _>(ptr)
    }

    pub fn build(mut self) -> FunctionValue<'llvm> {
        self.maybe_deref();
        self.copy_to_target();
        self.thunk
    }
}
