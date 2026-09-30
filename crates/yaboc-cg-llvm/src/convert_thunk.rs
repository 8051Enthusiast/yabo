use inkwell::{
    basic_block::BasicBlock,
    values::{FunctionValue, PointerValue},
};

use yaboc_layout::{ILayout, IMonoLayout, collect::LCallReq};
use yaboc_target::layout::SizeAlign;

use crate::{
    IResult, parser_values, tail_eval_fun_values,
    val::{CgReturnValue, CgValue},
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

pub struct BlockThunk<'comp> {
    pub from: Option<ILayout<'comp>>,
    pub fun: IMonoLayout<'comp>,
    pub result: IMonoLayout<'comp>,
    pub req: LCallReq,
}

impl<'comp, 'llvm> ThunkInfo<'comp, 'llvm> for BlockThunk<'comp> {
    fn function(&self, cg: &mut CodeGenCtx<'llvm, 'comp>) -> FunctionValue<'llvm> {
        let f = if let Some(from) = self.from {
            cg.parser_fun_val_tail(self.fun, from, self.req)
        } else {
            cg.eval_fun_fun_val_tail(self.fun, self.req)
        };
        cg.add_entry_block(f, self.fun);
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
        if let Some(from) = self.from {
            let (ret_val, fun_val, arg_val) = parser_values(fun, self.fun, from);
            let ret_val = ret_val.with_ptr(return_ptr);
            cg.call_parser_fun_impl(ret_val, fun_val, arg_val, self.req)?
        } else {
            let (ret_val, fun_val, arg_ptr) = tail_eval_fun_values(fun, self.fun);
            let zst = cg.layouts.dcx.primitive(yaboc_types::PrimitiveType::Unit);
            let arg_val = CgValue::new(zst, arg_ptr);
            let ret_val = ret_val.with_ptr(return_ptr);
            cg.call_eval_fun_fun_impl(ret_val, fun_val.into(), arg_val, self.req)?
        };
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
        let target_level = fun.get_nth_param(2).unwrap().into_pointer_value();
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

    fn deref_tail(
        &mut self,
        after_copy: bool,
        return_ptr: PointerValue<'llvm>,
    ) -> IResult<BasicBlock<'llvm>> {
        if let Some(block) = self.kind.build_tail(self.cg, after_copy, return_ptr)? {
            return Ok(block);
        }
        let previous_bb = self.cg.builder.get_insert_block();
        let current_bb = self.cg.llvm.append_basic_block(self.fun, "deref_tail");
        self.cg.builder.position_at_end(current_bb);
        self.cg.builder.build_return(Some(&self.cg.const_i64(0)))?;
        if let Some(bb) = previous_bb {
            self.cg.builder.position_at_end(bb);
        }
        Ok(current_bb)
    }

    fn copy_to_target(&mut self) -> IResult<()> {
        self.check_vtable()?;

        let mut i = 0u8;
        let mut offset = 0u64;
        while let Some((ptr, sa)) = self.kind.build_copy_region_ptr(self.cg, i)? {
            offset = sa.next_offset(offset);
            if sa.total_size() > 0 {
                let llvm_start_offset = self.cg.const_i64((offset - sa.before) as i64);
                let real_target =
                    self.cg
                        .build_byte_gep(self.ret.ptr, llvm_start_offset, "real_target")?;
                let real_source = self.cg.build_byte_gep(
                    ptr,
                    self.cg.const_i64(-(sa.before as i64)),
                    "real_source",
                )?;
                let align = sa.start_alignment();
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
        let after = self.deref_tail(true, self.ret.ptr)?;
        self.cg.builder.build_unconditional_branch(after)?;
        Ok(())
    }

    fn check_vtable(&mut self) -> IResult<()> {
        self.cg.write_vtable_if_tagged(
            self.ret,
            CgValue {
                layout: self.target_layout.inner(),
                ptr: self.cg.invalid_ptr(),
            },
        )
    }

    pub fn build(mut self) -> IResult<FunctionValue<'llvm>> {
        self.copy_to_target()?;
        Ok(self.fun)
    }
}
