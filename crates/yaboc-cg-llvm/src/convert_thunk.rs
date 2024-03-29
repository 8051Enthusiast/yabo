use inkwell::{
    basic_block::BasicBlock,
    values::{FunctionValue, PhiValue, PointerValue},
    IntPredicate,
};

use yaboc_hir_types::{TyHirs, NOBACKTRACK_BIT, VTABLE_BIT};
use yaboc_layout::{ILayout, IMonoLayout, MonoLayout, TailInfo};
use yaboc_req::{NeededBy, RequirementSet};
use yaboc_target::layout::SizeAlign;
use yaboc_types::PrimitiveType;

use crate::{
    eval_fun_values, get_fun_args, parser_values,
    val::{CgMonoValue, CgReturnValue, CgValue},
    IResult,
};

use super::CodeGenCtx;

pub trait ThunkInfo<'comp, 'llvm> {
    fn function(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> FunctionValue<'llvm>;
    fn build_copy_region_ptr(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        idx: u8,
    ) -> IResult<Option<(PointerValue<'llvm>, SizeAlign)>>;
    fn build_tail(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        after_copy: bool,
        return_ptr: PointerValue<'llvm>,
    ) -> IResult<Option<BasicBlock<'llvm>>>;
    fn target_layout(&self) -> IMonoLayout<'comp>;
}

pub struct TypecastThunk<'comp, 'llvm> {
    layout: IMonoLayout<'comp>,
    arg_copy: Option<CgValue<'comp, 'llvm>>,
    fun_copy: Option<CgMonoValue<'comp, 'llvm>>,
    f: FunctionValue<'llvm>,
}

impl<'comp, 'llvm> TypecastThunk<'comp, 'llvm> {
    pub fn new(cg: &mut CodeGenCtx<'llvm, 'comp>, layout: IMonoLayout<'comp>) -> IResult<Self> {
        let f = cg.typecast_fun_val(layout);
        cg.add_entry_block(f);
        let (arg_copy, fun_copy) = if let MonoLayout::Nominal(..) = layout.mono_layout().0 {
            let (from, layout) = layout.unapply_nominal(cg.layouts);
            let arg_copy = cg.build_alloca_value(from, "arg_copy")?;
            let fun_copy = if let TailInfo {
                has_tailsites: true,
                sa,
            } = cg.collected_layouts.tail_sa[&(from, layout)]
            {
                let sa_alloc = cg.build_sa_alloca(sa, "tail_storage")?;
                Some(CgMonoValue::new(layout, sa_alloc))
            } else {
                None
            };
            (Some(arg_copy), fun_copy)
        } else {
            (None, None)
        };
        Ok(Self {
            layout,
            arg_copy,
            fun_copy,
            f,
        })
    }
}

impl<'comp, 'llvm> ThunkInfo<'comp, 'llvm> for TypecastThunk<'comp, 'llvm> {
    fn function(&self, _cg: &mut CodeGenCtx<'llvm, 'comp>) -> FunctionValue<'llvm> {
        self.f
    }

    fn build_copy_region_ptr(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        idx: u8,
    ) -> IResult<Option<(PointerValue<'llvm>, SizeAlign)>> {
        if idx != 0 {
            return Ok(None);
        }
        let ptr = cg
            .current_function()
            .get_nth_param(1)
            .unwrap()
            .into_pointer_value();
        let sa = self.layout.inner().size_align(cg.layouts).unwrap();
        Ok(Some((ptr, sa)))
    }

    fn target_layout(&self) -> IMonoLayout<'comp> {
        self.layout
    }

    fn build_tail(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        after_copy: bool,
        _return_ptr: PointerValue<'llvm>,
    ) -> IResult<Option<BasicBlock<'llvm>>> {
        if after_copy {
            return Ok(None);
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

        let ret = if let MonoLayout::Primitive(PrimitiveType::U8) = thunk.layout.mono_layout().0 {
            cg.call_current_element_fun(ret, thunk.into())?
        } else {
            let arg_copy = self.arg_copy.unwrap();
            let (from, fun) = cg.build_nominal_components(thunk)?;
            let fun = if let Some(fun_cpy) = self.fun_copy {
                cg.build_copy_invariant(fun_cpy.into(), fun.into())?;
                fun_cpy
            } else {
                fun
            };
            cg.build_copy_invariant(arg_copy, from)?;

            cg.call_parser_fun_impl(ret, fun, arg_copy, NeededBy::Val.into(), false)?
        };

        cg.builder.build_return(Some(&ret))?;
        if let Some(bb) = previous_bb {
            cg.builder.position_at_end(bb);
        }
        Ok(Some(current_bb))
    }
}

pub struct TransmuteCopyThunk<'comp, 'llvm> {
    pub from: IMonoLayout<'comp>,
    pub to: IMonoLayout<'comp>,
    pub f: FunctionValue<'llvm>,
}

impl<'comp, 'llvm> ThunkInfo<'comp, 'llvm> for TransmuteCopyThunk<'comp, 'llvm> {
    fn function(&self, _: &mut CodeGenCtx<'llvm, 'comp>) -> FunctionValue<'llvm> {
        self.f
    }
    fn build_copy_region_ptr(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        idx: u8,
    ) -> IResult<Option<(PointerValue<'llvm>, SizeAlign)>> {
        if idx != 0 {
            return Ok(None);
        }
        let ptr = cg
            .current_function()
            .get_nth_param(1)
            .unwrap()
            .into_pointer_value();
        let sa = self.from.inner().size_align(cg.layouts).unwrap();
        Ok(Some((ptr, sa)))
    }
    fn target_layout(&self) -> IMonoLayout<'comp> {
        self.to
    }

    fn build_tail(
        &self,
        _cg: &mut CodeGenCtx<'llvm, 'comp>,
        _after_copy: bool,
        _return_ptr: PointerValue<'llvm>,
    ) -> IResult<Option<BasicBlock<'llvm>>> {
        Ok(None)
    }
}

pub struct ValThunk<'comp> {
    from: Option<ILayout<'comp>>,
    fun: IMonoLayout<'comp>,
    thunk: IMonoLayout<'comp>,
    req: RequirementSet,
}

impl<'comp> ValThunk<'comp> {
    pub fn new(
        from: Option<ILayout<'comp>>,
        fun: IMonoLayout<'comp>,
        thunk: IMonoLayout<'comp>,
        req: RequirementSet,
    ) -> Self {
        Self {
            from,
            fun,
            thunk,
            req,
        }
    }
}

impl<'comp, 'llvm> ThunkInfo<'comp, 'llvm> for ValThunk<'comp> {
    fn function(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> FunctionValue<'llvm> {
        let f = if let Some(from) = self.from {
            cg.parser_fun_val_tail(self.fun, from, self.req)
        } else {
            cg.eval_fun_fun_val_wrapper(self.fun)
        };
        cg.add_entry_block(f);
        f
    }

    fn build_copy_region_ptr(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        idx: u8,
    ) -> IResult<Option<(PointerValue<'llvm>, SizeAlign)>> {
        Ok(match (self.from, idx) {
            (Some(from), 0) => {
                let arg_ptr = cg
                    .current_function()
                    .get_nth_param(3)
                    .unwrap()
                    .into_pointer_value();
                let sa = from.size_align(cg.layouts).unwrap();
                Some((arg_ptr, sa))
            }
            (Some(_), 1) | (None, 0) => {
                let fun_ptr = cg
                    .current_function()
                    .get_nth_param(1)
                    .unwrap()
                    .into_pointer_value();
                let sa = self.fun.inner().size_align(cg.layouts).unwrap();
                Some((fun_ptr, sa))
            }
            (Some(_), 2..) | (None, 1..) => None,
        })
    }

    fn build_tail(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        after_copy: bool,
        _return_ptr: PointerValue<'llvm>,
    ) -> IResult<Option<BasicBlock<'llvm>>> {
        let req = if after_copy {
            self.req & !NeededBy::Val
        } else {
            self.req
        };
        if req.is_empty() {
            return Ok(None);
        }
        let previous_bb = cg.builder.get_insert_block();
        let fun = cg.current_function();
        let current_bb = cg.llvm.append_basic_block(fun, "tail");
        cg.builder.position_at_end(current_bb);
        let ret = if let Some(from) = self.from {
            let (ret, fun, arg) = parser_values(fun, self.fun, from);
            cg.call_parser_fun_impl(ret, fun, arg, req, true)?
        } else {
            let (ret, fun) = eval_fun_values(fun, self.fun);
            cg.call_eval_fun_fun_impl(ret, fun.into())?
        };
        cg.builder.build_return(Some(&ret))?;
        if let Some(bb) = previous_bb {
            cg.builder.position_at_end(bb);
        }
        Ok(Some(current_bb))
    }

    fn target_layout(&self) -> IMonoLayout<'comp> {
        self.thunk
    }
}

pub struct BlockThunk<'comp> {
    pub from: Option<ILayout<'comp>>,
    pub fun: IMonoLayout<'comp>,
    pub result: IMonoLayout<'comp>,
    pub req: RequirementSet,
}

impl<'comp, 'llvm> ThunkInfo<'comp, 'llvm> for BlockThunk<'comp> {
    fn function(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> FunctionValue<'llvm> {
        let f = if let Some(from) = self.from {
            cg.parser_fun_val_tail(self.fun, from, self.req)
        } else {
            cg.eval_fun_fun_val_wrapper(self.fun)
        };
        cg.add_entry_block(f);
        f
    }

    fn build_copy_region_ptr(
        &self,
        _cg: &mut CodeGenCtx<'llvm, 'comp>,
        _idx: u8,
    ) -> IResult<Option<(PointerValue<'llvm>, SizeAlign)>> {
        Ok(None)
    }

    fn build_tail(
        &self,
        cg: &mut CodeGenCtx<'llvm, 'comp>,
        after_copy: bool,
        return_ptr: PointerValue<'llvm>,
    ) -> IResult<Option<BasicBlock<'llvm>>> {
        if !after_copy {
            return Ok(None);
        }
        let previous_bb = cg.builder.get_insert_block();
        let fun = cg.current_function();
        let current_bb = cg.llvm.append_basic_block(fun, "tail");
        cg.builder.position_at_end(current_bb);
        let ret = if let Some(from) = self.from {
            let (ret_val, fun_val, arg_val) = parser_values(fun, self.fun, from);
            let ret_val = ret_val.with_ptr(return_ptr);
            cg.call_parser_fun_impl(ret_val, fun_val, arg_val, self.req, true)?
        } else {
            let (ret_val, fun_val) = eval_fun_values(fun, self.fun);
            let ret_val = ret_val.with_ptr(return_ptr);
            cg.call_eval_fun_fun_impl(ret_val, fun_val.into())?
        };
        cg.builder.build_return(Some(&ret))?;
        if let Some(bb) = previous_bb {
            cg.builder.position_at_end(bb);
        }
        Ok(Some(current_bb))
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

    fn maybe_deref(&mut self) -> IResult<()> {
        let ty = self.target_layout.mono_layout().1;
        let deref_level = self.cg.compiler_database.db.deref_level(ty).unwrap();
        if deref_level.is_deref() {
            let self_level = self
                .cg
                .build_deref_level_get(Some(self.target_layout), self.cg.invalid_ptr())?;
            let no_deref = self.cg.builder.build_int_compare(
                IntPredicate::ULE,
                self_level,
                self.ret.head,
                "no_deref",
            )?;
            let tail = self.typecast_tail(false, self.ret.ptr)?;
            let next_bb = self.cg.llvm.append_basic_block(self.fun, "head_match");
            self.cg
                .builder
                .build_conditional_branch(no_deref, next_bb, tail)?;
            self.cg.builder.position_at_end(next_bb);
        }
        Ok(())
    }

    fn typecast_tail(
        &mut self,
        after_copy: bool,
        return_ptr: PointerValue<'llvm>,
    ) -> IResult<BasicBlock<'llvm>> {
        if let Some(block) = self.kind.build_tail(self.cg, after_copy, return_ptr)? {
            return Ok(block);
        }
        let previous_bb = self.cg.builder.get_insert_block();
        let current_bb = self.cg.llvm.append_basic_block(self.fun, "typecast_tail");
        self.cg.builder.position_at_end(current_bb);
        self.cg.builder.build_return(Some(&self.cg.const_i64(0)))?;
        if let Some(bb) = previous_bb {
            self.cg.builder.position_at_end(bb);
        }
        Ok(current_bb)
    }

    fn copy_to_target(&mut self) -> IResult<()> {
        let copy_bb = self.cg.llvm.append_basic_block(self.fun, "copy");
        let old_bb = self.cg.builder.get_insert_block().unwrap();

        self.cg.builder.position_at_end(copy_bb);
        let copy_phi = self.cg.builder.build_phi(self.cg.any_ptr(), "copy_phi")?;
        let check_vtable_bb = self.cg.llvm.append_basic_block(self.fun, "check_vtable");

        self.cg.builder.position_at_end(check_vtable_bb);
        self.check_vtable(copy_bb, copy_phi, copy_bb)?;
        copy_phi.add_incoming(&[(&self.ret.ptr, check_vtable_bb)]);

        self.cg.builder.position_at_end(old_bb);
        self.cg
            .builder
            .build_unconditional_branch(check_vtable_bb)?;

        self.cg.builder.position_at_end(copy_bb);
        let target_ptr = copy_phi.as_basic_value().into_pointer_value();
        let mut i = 0u8;
        let mut offset = 0u64;
        while let Some((ptr, sa)) = self.kind.build_copy_region_ptr(self.cg, i)? {
            offset = sa.next_offset(offset);
            if sa.total_size() > 0 {
                let llvm_start_offset = self.cg.const_i64((offset - sa.before) as i64);
                let real_target =
                    self.cg
                        .build_byte_gep(target_ptr, llvm_start_offset, "real_target")?;
                let real_source = self.cg.build_byte_gep(
                    ptr,
                    self.cg.const_i64(-(sa.before as i64)),
                    "real_source",
                )?;
                let align = sa.start_alignment();
                // it is nice to be able to pass an invalid pointer
                // for ZSTs, but calling memcpy with a null pointer
                // is UB, therefore we simply don't generate it for ZSTs
                self.cg.builder.build_memcpy(
                    real_target,
                    align as u32,
                    real_source,
                    align as u32,
                    self.cg.const_size_t(sa.total_size() as i64),
                )?;
            }
            offset += sa.after;
            i += 1;
        }
        let after = self.typecast_tail(true, target_ptr)?;
        self.cg.builder.build_unconditional_branch(after)?;
        Ok(())
    }

    fn check_vtable(
        &mut self,
        copy_bb: BasicBlock<'llvm>,
        copy_phi: PhiValue<'llvm>,
        otherwise: BasicBlock<'llvm>,
    ) -> IResult<()> {
        let has_vtable = self.cg.build_check_i64_bit_set(self.ret.head, VTABLE_BIT)?;
        let write_vtable_ptr = self
            .cg
            .llvm
            .append_basic_block(self.fun, "write_vtable_ptr");
        self.cg
            .builder
            .build_conditional_branch(has_vtable, write_vtable_ptr, otherwise)?;
        self.cg.builder.position_at_end(write_vtable_ptr);
        let vtable_pointer = self.build_vtable_any_ptr()?;
        self.cg.build_vtable_store(self.ret.ptr, vtable_pointer)?;
        copy_phi.add_incoming(&[(&self.ret.ptr, write_vtable_ptr)]);
        self.cg.builder.build_unconditional_branch(copy_bb)?;
        Ok(())
    }

    fn build_vtable_any_ptr(&mut self) -> IResult<PointerValue<'llvm>> {
        let bt_ptr = self.cg.build_get_vtable_tag(self.target_layout);
        if !matches!(
            self.target_layout.mono_layout().0,
            MonoLayout::NominalParser(..)
        ) {
            return Ok(bt_ptr);
        }
        let nbt_target_layout = self.target_layout.remove_backtracking(self.cg.layouts);
        let nbt_ptr = self.cg.build_get_vtable_tag(nbt_target_layout);
        let needs_nbt = self
            .cg
            .build_check_i64_bit_set(self.ret.head, NOBACKTRACK_BIT)?;
        Ok(self
            .cg
            .builder
            .build_select(needs_nbt, nbt_ptr, bt_ptr, "vtable_ptr")?
            .into_pointer_value())
    }

    pub fn build(mut self) -> IResult<FunctionValue<'llvm>> {
        self.maybe_deref()?;
        self.copy_to_target()?;
        Ok(self.fun)
    }
}
