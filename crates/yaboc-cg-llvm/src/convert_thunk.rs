use inkwell::{
    basic_block::BasicBlock,
    values::{FunctionValue, PhiValue, PointerValue},
    IntPredicate,
};

use yaboc_dependents::NeededBy;
use yaboc_hir_types::{TyHirs, MALLOC_BIT, NOBACKTRACK_BIT, VTABLE_BIT};
use yaboc_layout::{
    collect::pd_val_req,
    prop::{PSize, SizeAlign, TargetSized},
    ILayout, IMonoLayout, MonoLayout,
};
use yaboc_mir::CallMeta;

use crate::{
    get_fun_args, parser_values,
    val::{CgMonoValue, CgReturnValue, CgValue},
};

use super::CodeGenCtx;

pub trait ThunkInfo<'comp> {
    fn alloc_size<'llvm>(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> SizeAlign;
    fn function<'llvm>(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> FunctionValue<'llvm>;
    fn build_copy_region_ptr<'llvm>(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        idx: u8,
    ) -> Option<(PointerValue<'llvm>, SizeAlign)>;
    fn build_tail<'llvm>(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        after_copy: bool,
        return_ptr: PointerValue<'llvm>,
    ) -> Option<BasicBlock<'llvm>>;
    fn target_layout(&self) -> IMonoLayout<'comp>;
}

pub struct TypecastThunk<'comp> {
    pub layout: IMonoLayout<'comp>,
}

impl<'comp> ThunkInfo<'comp> for TypecastThunk<'comp> {
    fn alloc_size<'llvm>(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> SizeAlign {
        self.layout.inner().size_align(cg.layouts).unwrap()
    }

    fn function<'llvm>(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> FunctionValue<'llvm> {
        let f = cg.typecast_fun_val(self.layout);
        cg.add_entry_block(f);
        f
    }

    fn build_copy_region_ptr<'llvm>(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        idx: u8,
    ) -> Option<(PointerValue<'llvm>, SizeAlign)> {
        if idx != 0 {
            return None;
        }
        let ptr = cg
            .current_function()
            .get_nth_param(1)
            .unwrap()
            .into_pointer_value();
        let sa = self.layout.inner().size_align(cg.layouts).unwrap();
        Some((ptr, sa))
    }

    fn target_layout(&self) -> IMonoLayout<'comp> {
        self.layout
    }

    fn build_tail<'llvm>(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        after_copy: bool,
        _return_ptr: PointerValue<'llvm>,
    ) -> Option<BasicBlock<'llvm>> {
        if after_copy {
            return None;
        }
        let previous_bb = cg.builder.get_insert_block();
        let fun = cg.current_function();
        let current_bb = cg.llvm.append_basic_block(fun, "tail");
        cg.builder.position_at_end(current_bb);

        let [return_ptr, thunk_ptr, target_level] = get_fun_args(fun);
        let [ret_ptr, thunk_ptr] = [return_ptr, thunk_ptr].map(|x| x.into_pointer_value());
        let target_level = target_level.into_int_value();
        let thunk = CgMonoValue::new(self.layout, thunk_ptr);
        let ret = CgReturnValue::new(target_level, ret_ptr);

        let (from, fun, slot) = cg.build_nominal_components(thunk, pd_val_req());

        let ret = cg.call_parser_fun(
            ret,
            fun.into(),
            from,
            slot,
            CallMeta::new(NeededBy::Val.into(), false),
            true,
        );
        cg.builder.build_return(Some(&ret));
        if let Some(bb) = previous_bb {
            cg.builder.position_at_end(bb);
        }
        Some(current_bb)
    }
}

pub struct CreateArgsThunk<'comp> {
    pub from: IMonoLayout<'comp>,
    pub to: IMonoLayout<'comp>,
    pub slot: PSize,
}

impl<'comp> ThunkInfo<'comp> for CreateArgsThunk<'comp> {
    fn alloc_size<'llvm>(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> SizeAlign {
        self.to.inner().size_align(cg.layouts).unwrap()
    }
    fn function<'llvm>(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> FunctionValue<'llvm> {
        let f = cg.function_create_args_fun_val(self.from, self.slot);
        cg.add_entry_block(f);
        f
    }
    fn build_copy_region_ptr<'llvm>(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        idx: u8,
    ) -> Option<(PointerValue<'llvm>, SizeAlign)> {
        if idx != 0 {
            return None;
        }
        let ptr = cg
            .current_function()
            .get_nth_param(1)
            .unwrap()
            .into_pointer_value();
        let sa = self.from.inner().size_align(cg.layouts).unwrap();
        Some((ptr, sa))
    }
    fn target_layout(&self) -> IMonoLayout<'comp> {
        self.to
    }

    fn build_tail<'llvm>(
        &self,
        _cg: &mut CodeGenCtx<'llvm, 'comp>,
        _after_copy: bool,
        _return_ptr: PointerValue<'llvm>,
    ) -> Option<BasicBlock<'llvm>> {
        None
    }
}

pub struct ValThunk<'comp> {
    pub from: ILayout<'comp>,
    pub fun: IMonoLayout<'comp>,
    pub thunk: IMonoLayout<'comp>,
    pub slot: PSize,
    pub req: CallMeta,
}

impl<'comp> ThunkInfo<'comp> for ValThunk<'comp> {
    fn alloc_size<'llvm>(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> SizeAlign {
        self.from
            .size_align(cg.layouts)
            .unwrap()
            .cat(self.fun.inner().size_align(cg.layouts).unwrap())
    }

    fn function<'llvm>(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> FunctionValue<'llvm> {
        let f = cg.parser_fun_val(self.fun, self.slot, self.req);
        cg.add_entry_block(f);
        f
    }

    fn build_copy_region_ptr<'llvm>(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        idx: u8,
    ) -> Option<(PointerValue<'llvm>, SizeAlign)> {
        if idx == 0 {
            let arg_ptr = cg
                .current_function()
                .get_nth_param(3)
                .unwrap()
                .into_pointer_value();
            let sa = self.from.size_align(cg.layouts).unwrap();
            let arg_obj_ptr = cg.get_object_start(CgValue::new(self.from, arg_ptr));
            return Some((arg_obj_ptr, sa));
        } else if idx == 1 {
            let fun_ptr = cg
                .current_function()
                .get_nth_param(1)
                .unwrap()
                .into_pointer_value();
            let sa = self.fun.inner().size_align(cg.layouts).unwrap();
            return Some((fun_ptr, sa));
        }
        None
    }

    fn build_tail<'llvm>(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        after_copy: bool,
        _return_ptr: PointerValue<'llvm>,
    ) -> Option<BasicBlock<'llvm>> {
        let info = if after_copy {
            self.req.with_req(|req| req & !NeededBy::Val)
        } else {
            self.req
        };
        if info.req.is_empty() {
            return None;
        }
        let previous_bb = cg.builder.get_insert_block();
        let fun = cg.current_function();
        let current_bb = cg.llvm.append_basic_block(fun, "tail");
        cg.builder.position_at_end(current_bb);

        let (ret, fun, arg) = parser_values(fun, self.fun, self.from);

        let ret = cg.call_parser_fun(ret, fun.into(), arg, self.slot, info, true);
        cg.builder.build_return(Some(&ret));
        if let Some(bb) = previous_bb {
            cg.builder.position_at_end(bb);
        }
        Some(current_bb)
    }

    fn target_layout(&self) -> IMonoLayout<'comp> {
        self.thunk
    }
}

pub struct BlockThunk<'comp> {
    pub from: ILayout<'comp>,
    pub fun: IMonoLayout<'comp>,
    pub result: IMonoLayout<'comp>,
    pub req: CallMeta,
    pub slot: PSize,
}

impl<'comp> ThunkInfo<'comp> for BlockThunk<'comp> {
    fn alloc_size<'llvm>(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> SizeAlign {
        self.result.inner().size_align(cg.layouts).unwrap()
    }

    fn function<'llvm>(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> FunctionValue<'llvm> {
        let f = cg.parser_fun_val(self.fun, self.slot, self.req);
        cg.add_entry_block(f);
        f
    }

    fn build_copy_region_ptr<'llvm>(
        &self,
        _cg: &mut CodeGenCtx<'llvm, 'comp>,
        _idx: u8,
    ) -> Option<(PointerValue<'llvm>, SizeAlign)> {
        None
    }

    fn build_tail<'llvm>(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        after_copy: bool,
        return_ptr: PointerValue<'llvm>,
    ) -> Option<BasicBlock<'llvm>> {
        if !after_copy {
            return None;
        }
        let previous_bb = cg.builder.get_insert_block();
        let fun = cg.current_function();
        let current_bb = cg.llvm.append_basic_block(fun, "tail");
        cg.builder.position_at_end(current_bb);
        let (ret_val, fun_val, arg_val) = parser_values(fun, self.fun, self.from);
        let ret_val = ret_val.with_ptr(return_ptr);

        let ret = cg.call_parser_fun(ret_val, fun_val.into(), arg_val, self.slot, self.req, true);
        cg.builder.build_return(Some(&ret));
        if let Some(bb) = previous_bb {
            cg.builder.position_at_end(bb);
        }
        Some(current_bb)
    }

    fn target_layout(&self) -> IMonoLayout<'comp> {
        self.result
    }
}

pub struct ThunkContext<'llvm, 'comp, 'r, Info: ThunkInfo<'comp>> {
    cg: &'r mut CodeGenCtx<'llvm, 'comp>,
    kind: Info,
    fun: FunctionValue<'llvm>,
    target_layout: IMonoLayout<'comp>,
    ret: CgReturnValue<'llvm>,
}

impl<'llvm, 'comp, 'r, Info: ThunkInfo<'comp>> ThunkContext<'llvm, 'comp, 'r, Info> {
    pub fn new(cg: &'r mut CodeGenCtx<'llvm, 'comp>, kind: Info) -> Self {
        let fun = kind.function(cg);
        let return_ptr = fun.get_nth_param(0).unwrap().into_pointer_value();
        let target_level = fun.get_nth_param(2).unwrap().into_int_value();
        let target_layout = kind.target_layout();
        let ret = CgReturnValue::new(target_level, return_ptr);
        ThunkContext {
            cg,
            kind,
            fun,
            target_layout,
            ret,
        }
    }

    fn maybe_deref(&mut self) {
        let ty = self.target_layout.mono_layout().1;
        let deref_level = self.cg.compiler_database.db.deref_level(ty).unwrap();
        if deref_level.is_deref() {
            let self_level = self
                .cg
                .build_deref_level_get(Some(self.target_layout), self.cg.any_ptr().get_undef());
            let no_deref = self.cg.builder.build_int_compare(
                IntPredicate::ULE,
                self_level,
                self.ret.head,
                "no_deref",
            );
            let tail = self.typecast_tail(false, self.ret.ptr);
            let next_bb = self.cg.llvm.append_basic_block(self.fun, "head_match");
            self.cg
                .builder
                .build_conditional_branch(no_deref, next_bb, tail);
            self.cg.builder.position_at_end(next_bb);
        }
    }

    fn typecast_tail(
        &mut self,
        after_copy: bool,
        return_ptr: PointerValue<'llvm>,
    ) -> BasicBlock<'llvm> {
        if let Some(block) = self.kind.build_tail(self.cg, after_copy, return_ptr) {
            return block;
        }
        let previous_bb = self.cg.builder.get_insert_block();
        let current_bb = self.cg.llvm.append_basic_block(self.fun, "typecast_tail");
        self.cg.builder.position_at_end(current_bb);
        self.cg.builder.build_return(Some(&self.cg.const_i64(0)));
        if let Some(bb) = previous_bb {
            self.cg.builder.position_at_end(bb);
        }
        current_bb
    }

    fn copy_to_target(&mut self) {
        let copy_bb = self.cg.llvm.append_basic_block(self.fun, "copy");
        let old_bb = self.cg.builder.get_insert_block().unwrap();

        self.cg.builder.position_at_end(copy_bb);
        let copy_phi = self.cg.builder.build_phi(self.cg.any_ptr(), "copy_phi");
        let check_vtable_bb = self.cg.llvm.append_basic_block(self.fun, "check_vtable");

        self.cg.builder.position_at_end(check_vtable_bb);
        self.check_vtable(copy_bb, copy_phi, copy_bb);
        copy_phi.add_incoming(&[(&self.ret.ptr, check_vtable_bb)]);

        self.cg.builder.position_at_end(old_bb);
        let alloc_sa = self.kind.alloc_size(self.cg);
        if alloc_sa.size > <*const u8>::tsize().array(2).size {
            self.check_malloc(copy_bb, copy_phi, check_vtable_bb, alloc_sa);
        } else {
            self.cg.builder.build_unconditional_branch(check_vtable_bb);
        }

        self.cg.builder.position_at_end(copy_bb);
        let target_ptr = copy_phi.as_basic_value().into_pointer_value();
        let mut i = 0u8;
        let mut offset = 0u64;
        while let Some((ptr, sa)) = self.kind.build_copy_region_ptr(self.cg, i) {
            if sa.size > 0 {
                offset = (offset + sa.align_mask) & !sa.align_mask;
                let llvm_offset = self.cg.const_i64(offset as i64);
                let real_target = self
                    .cg
                    .build_byte_gep(target_ptr, llvm_offset, "real_target");
                // it is nice to be able to pass an invalid pointer
                // for ZSTs, but calling memcpy with a null pointer
                // is UB, therefore we simply don't generate it for ZSTs
                self.cg
                    .builder
                    .build_memcpy(
                        real_target,
                        sa.align() as u32,
                        ptr,
                        sa.align() as u32,
                        self.cg.const_size_t(sa.size as i64),
                    )
                    .unwrap();
            }
            i += 1;
            offset += sa.size;
        }
        let after = self.typecast_tail(true, target_ptr);
        self.cg.builder.build_unconditional_branch(after);
    }

    fn check_vtable(
        &mut self,
        copy_bb: BasicBlock<'llvm>,
        copy_phi: PhiValue<'llvm>,
        otherwise: BasicBlock<'llvm>,
    ) {
        let has_vtable = self.cg.build_check_i64_bit_set(self.ret.head, VTABLE_BIT);
        let write_vtable_ptr = self
            .cg
            .llvm
            .append_basic_block(self.fun, "write_vtable_ptr");
        self.cg
            .builder
            .build_conditional_branch(has_vtable, write_vtable_ptr, otherwise);
        self.cg.builder.position_at_end(write_vtable_ptr);
        let vtable_pointer = self.build_vtable_any_ptr();
        let vtable_offset = self.cg.any_ptr().size_of().const_neg();
        let before_ptr = self
            .cg
            .build_byte_gep(self.ret.ptr, vtable_offset, "vtable_ptr_skip");
        let ret_vtable_ptr = self.cg.build_cast::<*mut *const u8, _>(before_ptr);
        self.cg.builder.build_store(ret_vtable_ptr, vtable_pointer);
        copy_phi.add_incoming(&[(&self.ret.ptr, write_vtable_ptr)]);
        self.cg.builder.build_unconditional_branch(copy_bb);
    }

    fn check_malloc(
        &mut self,
        copy_bb: BasicBlock<'llvm>,
        copy_phi: PhiValue<'llvm>,
        otherwise: BasicBlock<'llvm>,
        alloc_sa: SizeAlign,
    ) {
        let is_malloc = self.cg.build_check_i64_bit_set(self.ret.head, MALLOC_BIT);
        let allocator_bb = self.cg.llvm.append_basic_block(self.fun, "allocator");
        self.cg
            .builder
            .build_conditional_branch(is_malloc, allocator_bb, otherwise);

        self.cg.builder.position_at_end(allocator_bb);
        let vtable_pointer = self.build_vtable_any_ptr();
        let tagged_vtable_ptr = self.build_malloc_tag_ptr(vtable_pointer);
        let malloc_pointer = self.build_malloc(alloc_sa);
        let target_as_ptr_ptr = self.cg.build_cast::<*mut *mut u8, _>(self.ret.ptr);
        let before_ptr = unsafe {
            self.cg.builder.build_in_bounds_gep(
                target_as_ptr_ptr,
                &[self.cg.const_size_t(-1)],
                "before",
            )
        };
        self.cg.builder.build_store(before_ptr, tagged_vtable_ptr);
        self.cg
            .builder
            .build_store(target_as_ptr_ptr, malloc_pointer);
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

    fn build_malloc(&mut self, sa: SizeAlign) -> PointerValue<'llvm> {
        let ty = self.cg.sa_type(sa);
        let malloc_ptr = self.cg.builder.build_malloc(ty, "benloc").unwrap();
        self.cg.set_last_instr_align(sa);
        self.cg.build_cast::<*mut u8, _>(malloc_ptr)
    }

    fn build_vtable_any_ptr(&mut self) -> PointerValue<'llvm> {
        let bt_ptr = self.cg.build_get_vtable_tag(self.target_layout);
        if let MonoLayout::NominalParser(..) = self.target_layout.mono_layout().0 {
        } else {
            return bt_ptr;
        }
        let nbt_target_layout = self.target_layout.remove_backtracking(self.cg.layouts);
        let nbt_ptr = self.cg.build_get_vtable_tag(nbt_target_layout);
        let needs_nbt = self
            .cg
            .build_check_i64_bit_set(self.ret.head, NOBACKTRACK_BIT);
        self.cg
            .builder
            .build_select(needs_nbt, nbt_ptr, bt_ptr, "vtable_ptr")
            .into_pointer_value()
    }

    pub fn build(mut self) -> FunctionValue<'llvm> {
        self.maybe_deref();
        self.copy_to_target();
        self.fun
    }
}
