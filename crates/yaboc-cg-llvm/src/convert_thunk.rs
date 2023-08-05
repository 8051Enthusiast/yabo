use inkwell::{
    basic_block::BasicBlock,
    values::{FunctionValue, PhiValue, PointerValue},
    IntPredicate,
};

use yaboc_dependents::{NeededBy, RequirementSet};
use yaboc_hir_types::{TyHirs, NOBACKTRACK_BIT, VTABLE_BIT};
use yaboc_layout::{
    collect::pd_val_req,
    prop::{PSize, SizeAlign},
    ILayout, IMonoLayout, MonoLayout, TailInfo,
};

use crate::{
    get_fun_args, parser_values,
    val::{CgMonoValue, CgReturnValue, CgValue},
};

use super::CodeGenCtx;

pub trait ThunkInfo<'comp, 'llvm> {
    fn alloc_size(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> SizeAlign;
    fn function(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> FunctionValue<'llvm>;
    fn build_copy_region_ptr(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        idx: u8,
    ) -> Option<(PointerValue<'llvm>, SizeAlign)>;
    fn build_tail(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        after_copy: bool,
        return_ptr: PointerValue<'llvm>,
    ) -> Option<BasicBlock<'llvm>>;
    fn target_layout(&self) -> IMonoLayout<'comp>;
}

pub struct TypecastThunk<'comp, 'llvm> {
    layout: IMonoLayout<'comp>,
    arg_copy: Option<CgValue<'comp, 'llvm>>,
    fun_copy: Option<CgMonoValue<'comp, 'llvm>>,
    f: FunctionValue<'llvm>,
}

impl<'comp, 'llvm> TypecastThunk<'comp, 'llvm> {
    pub fn new(cg: &mut CodeGenCtx<'llvm, 'comp>, layout: IMonoLayout<'comp>) -> Self {
        let f = cg.typecast_fun_val(layout);
        cg.add_entry_block(f);
        let (arg_copy, fun_copy) = if let MonoLayout::Nominal(..) = layout.mono_layout().0 {
            let (from, layout) = layout.unapply_nominal(cg.layouts);
            let arg_copy = cg.build_alloca_value(from, "arg_copy");
            let fun_copy = if let TailInfo {
                has_tailsites: true,
                sa,
            } = cg.collected_layouts.tail_sa[&(from, layout)]
            {
                let sa_alloc = cg.build_sa_alloca(sa, Some(false), "fun_copy");
                Some(CgMonoValue::new(layout, sa_alloc))
            } else {
                None
            };
            (Some(arg_copy), fun_copy)
        } else {
            (None, None)
        };
        Self {
            layout,
            arg_copy,
            fun_copy,
            f,
        }
    }
}

impl<'comp, 'llvm> ThunkInfo<'comp, 'llvm> for TypecastThunk<'comp, 'llvm> {
    fn alloc_size(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> SizeAlign {
        self.layout.inner().size_align(cg.layouts).unwrap()
    }

    fn function(&self, _cg: &mut CodeGenCtx<'llvm, 'comp>) -> FunctionValue<'llvm> {
        self.f
    }

    fn build_copy_region_ptr(
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

    fn build_tail(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        after_copy: bool,
        _return_ptr: PointerValue<'llvm>,
    ) -> Option<BasicBlock<'llvm>> {
        if after_copy {
            return None;
        }
        let arg_copy = self.arg_copy.unwrap();
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
        let fun = if let Some(fun_cpy) = self.fun_copy {
            cg.build_copy_invariant(fun_cpy.into(), fun.into());
            fun_cpy
        } else {
            fun
        };
        cg.build_copy_invariant(arg_copy, from);

        let ret = cg.call_parser_fun_impl(ret, fun, arg_copy, slot, NeededBy::Val.into(), false);
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

impl<'comp, 'llvm> ThunkInfo<'comp, 'llvm> for CreateArgsThunk<'comp> {
    fn alloc_size(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> SizeAlign {
        self.to.inner().size_align(cg.layouts).unwrap()
    }
    fn function(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> FunctionValue<'llvm> {
        let f = cg.function_create_args_fun_val(self.from, self.slot);
        cg.add_entry_block(f);
        f
    }
    fn build_copy_region_ptr(
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

    fn build_tail(
        &self,
        _cg: &mut CodeGenCtx<'llvm, 'comp>,
        _after_copy: bool,
        _return_ptr: PointerValue<'llvm>,
    ) -> Option<BasicBlock<'llvm>> {
        None
    }
}

pub struct ValThunk<'comp> {
    from: ILayout<'comp>,
    fun: IMonoLayout<'comp>,
    thunk: IMonoLayout<'comp>,
    slot: PSize,
    req: RequirementSet,
}

impl<'comp> ValThunk<'comp> {
    pub fn new(
        from: ILayout<'comp>,
        fun: IMonoLayout<'comp>,
        thunk: IMonoLayout<'comp>,
        slot: PSize,
        req: RequirementSet,
    ) -> Self {
        Self {
            from,
            fun,
            thunk,
            slot,
            req,
        }
    }
}

impl<'comp, 'llvm> ThunkInfo<'comp, 'llvm> for ValThunk<'comp> {
    fn alloc_size(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> SizeAlign {
        self.from
            .size_align(cg.layouts)
            .unwrap()
            .cat(self.fun.inner().size_align(cg.layouts).unwrap())
    }

    fn function(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> FunctionValue<'llvm> {
        let f = cg.parser_fun_val_tail(self.fun, self.slot, self.req);
        cg.add_entry_block(f);
        f
    }

    fn build_copy_region_ptr(
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

    fn build_tail(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        after_copy: bool,
        _return_ptr: PointerValue<'llvm>,
    ) -> Option<BasicBlock<'llvm>> {
        let req = if after_copy {
            self.req & !NeededBy::Val
        } else {
            self.req
        };
        if req.is_empty() {
            return None;
        }
        let previous_bb = cg.builder.get_insert_block();
        let fun = cg.current_function();
        let current_bb = cg.llvm.append_basic_block(fun, "tail");
        cg.builder.position_at_end(current_bb);

        let (ret, fun, arg) = parser_values(fun, self.fun, self.from);

        let ret = cg.call_parser_fun_impl(ret, fun, arg, self.slot, req, true);
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
    pub req: RequirementSet,
    pub slot: PSize,
}

impl<'comp, 'llvm> ThunkInfo<'comp, 'llvm> for BlockThunk<'comp> {
    fn alloc_size(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> SizeAlign {
        self.result.inner().size_align(cg.layouts).unwrap()
    }

    fn function(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> FunctionValue<'llvm> {
        let f = cg.parser_fun_val_tail(self.fun, self.slot, self.req);
        cg.add_entry_block(f);
        f
    }

    fn build_copy_region_ptr(
        &self,
        _cg: &mut CodeGenCtx<'llvm, 'comp>,
        _idx: u8,
    ) -> Option<(PointerValue<'llvm>, SizeAlign)> {
        None
    }

    fn build_tail(
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

        let ret = cg.call_parser_fun_impl(ret_val, fun_val, arg_val, self.slot, self.req, true);
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

pub struct ThunkContext<'llvm, 'comp, 'r, Info: ThunkInfo<'comp, 'llvm>> {
    cg: &'r mut CodeGenCtx<'llvm, 'comp>,
    kind: Info,
    fun: FunctionValue<'llvm>,
    target_layout: IMonoLayout<'comp>,
    ret: CgReturnValue<'llvm>,
}

impl<'llvm, 'comp, 'r, Info: ThunkInfo<'comp, 'llvm>> ThunkContext<'llvm, 'comp, 'r, Info> {
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
        self.cg.builder.build_unconditional_branch(check_vtable_bb);

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
