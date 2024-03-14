use inkwell::values::{IntValue, PointerValue};
use yaboc_layout::{ILayout, IMonoLayout};

#[derive(Clone, Copy)]
pub struct CgValue<'comp, 'llvm> {
    pub(crate) layout: ILayout<'comp>,
    pub(crate) ptr: PointerValue<'llvm>,
}

impl<'comp, 'llvm> CgValue<'comp, 'llvm> {
    pub fn new(layout: ILayout<'comp>, ptr: PointerValue<'llvm>) -> Self {
        Self { layout, ptr }
    }
}
#[derive(Clone, Copy)]
pub struct CgMonoValue<'comp, 'llvm> {
    pub(crate) layout: IMonoLayout<'comp>,
    pub(crate) ptr: PointerValue<'llvm>,
}

impl<'comp, 'llvm> CgMonoValue<'comp, 'llvm> {
    pub fn new(layout: IMonoLayout<'comp>, ptr: PointerValue<'llvm>) -> Self {
        Self { layout, ptr }
    }
}

impl<'comp, 'llvm> From<CgMonoValue<'comp, 'llvm>> for CgValue<'comp, 'llvm> {
    fn from(x: CgMonoValue<'comp, 'llvm>) -> Self {
        Self {
            layout: x.layout.inner(),
            ptr: x.ptr,
        }
    }
}

#[derive(Clone, Copy)]
pub struct CgReturnValue<'llvm> {
    pub(crate) head: IntValue<'llvm>,
    pub(crate) ptr: PointerValue<'llvm>,
}

impl<'llvm> CgReturnValue<'llvm> {
    pub fn new(head: IntValue<'llvm>, ptr: PointerValue<'llvm>) -> Self {
        Self { head, ptr }
    }

    pub fn with_ptr(self, ptr: PointerValue<'llvm>) -> Self {
        Self { ptr, ..self }
    }
}
