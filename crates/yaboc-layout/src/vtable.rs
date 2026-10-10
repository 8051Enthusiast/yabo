use std::marker::PhantomData;

use yaboc_target::{layout::VtablePointer, target_struct};

pub type DerefFun = fn(ret: *mut u8, from: *const u8, context: *const u8) -> i64;
pub type MaskFun = fn(ret: *mut u8) -> usize;

target_struct! {
    pub struct FunctionInfo<T: VtablePointer, F> {
        pub fun_impl: T::FPtr<F>,
        pub vtable: T::Ptr<VTableHeader<T>>,
    }
}

target_struct! {
    pub struct VTableHeader<T: VtablePointer> {
        pub head: u32,
        pub size_align: u32,
        pub deref_impl: FunctionInfo<T, DerefFun>,
        pub mask_impl: T::FPtr<MaskFun>,
        pub phantom: PhantomData<T>,
    }
}

target_struct! {
    pub struct BlockFields<T: VtablePointer> {
        pub number_fields: usize,
        pub phantom: PhantomData<T>,
        pub fields: [T::Ptr<u8>; 0],
    }
}

pub type BlockFieldFun = fn(ret: *mut u8, from: *const u8, context: *const u8) -> i64;

target_struct! {
    pub struct BlockVTable<T: VtablePointer> {
        pub head: VTableHeader<T>,
        pub fields: T::Ptr<BlockFields<T>>,
        pub access_impl: [FunctionInfo<T, BlockFieldFun>; 0],
    }
}

pub type StartFun = fn(ret: *mut u8, nom: *const u8, context: *const u8) -> i64;
pub type EndFun = fn(ret: *mut u8, nom: *const u8, context: *const u8) -> i64;

target_struct! {
    pub struct NominalVTable<T: VtablePointer> {
        pub head: VTableHeader<T>,
        pub name: T::Ptr<u8>,
        pub start_impl: FunctionInfo<T, StartFun>,
        pub end_impl: FunctionInfo<T, EndFun>,
    }
}

target_struct! {
    pub struct ArgDescriptor {
        pub head: u32,
        pub offset: u32,
    }
}

pub type ParserFun = fn(ret: *mut u8, fun: *const u8, context: *const u8, from: *const u8) -> i64;
pub type LenFun = fn(ret: *mut u8, from: *const u8, globals: *const u8) -> i64;

target_struct! {
    pub struct ParserVTable<T: VtablePointer> {
        pub set_arg_info: [ArgDescriptor; 0],
        pub head: VTableHeader<T>,
        pub len_impl: T::FPtr<Option<LenFun>>,
        pub apply_table: [FunctionInfo<T, Option<ParserFun>>; 0],
    }
}

pub type CreateArgFun = fn(ret: *mut u8, from: *const u8, context: *const u8) -> i64;
pub type EvalFunFun = fn(ret: *mut u8, fun: *const u8, context: *const u8) -> i64;
pub type EvalFunFunInternal =
    fn(ret: *mut u8, fun: *const u8, context: *const u8, *const u8) -> i64;

target_struct! {
    pub struct FunctionVTable<T: VtablePointer> {
        pub set_arg_info: [ArgDescriptor; 0],
        pub head: VTableHeader<T>,
        pub apply_table: [FunctionInfo<T, Option<CreateArgFun>>; 0],
    }
}

pub type SingleForwardFun = fn(from: *mut u8, globals: *const u8) -> i64;
pub type CurrentElementFun = fn(ret: *mut u8, from: *const u8, globals: *const u8) -> i64;
pub type ArrayLenFun = fn(from: *const u8, globals: *const u8) -> i64;
pub type SkipFun = fn(ret: *mut u8, offset: u64, context: *const u8) -> i64;
pub type SpanFun = fn(ret: *mut u8, from: *const u8, context: *const u8, to: *const u8) -> i64;
pub type InnerArrayFun = fn(ret: *mut u8, from: *const u8, context: *const u8) -> i64;

target_struct! {
    pub struct ArrayVTable<T: VtablePointer> {
        pub head: VTableHeader<T>,
        pub single_forward_impl: T::FPtr<SingleForwardFun>,
        pub current_element_impl: FunctionInfo<T, CurrentElementFun>,
        pub len_impl: T::FPtr<Option<ArrayLenFun>>,
        pub skip_impl: T::FPtr<Option<SkipFun>>,
        pub span_impl: T::FPtr<Option<SpanFun>>,
        pub inner_array_impl: FunctionInfo<T, Option<InnerArrayFun>>,
    }
}

target_struct! {
    pub struct ParserExport<T: VtablePointer> {
        pub parser: FunctionInfo<T, ParserFun>,
        pub phantom: PhantomData<T>,
        pub args: [T::Ptr<VTableHeader<T>>; 0],
    }
}

pub type InitFun = fn(start: *const u8, end: *const u8, context: *const u8) -> i64;

#[cfg(test)]
mod tests {
    use yaboc_target::layout::{AbsPtr, SizeAlign, TargetSized};

    use super::*;

    #[test]
    fn vtable_sizes() {
        let data = yaboc_target::layout::POINTER64;
        assert_eq!(
            VTableHeader::<AbsPtr>::tsize(&data),
            SizeAlign {
                before: 0,
                after: 32,
                align_mask: 0b111
            }
        );
        assert_eq!(
            BlockVTable::<AbsPtr>::tsize(&data),
            SizeAlign {
                before: 0,
                after: 40,
                align_mask: 0b111,
            }
        );
        assert_eq!(
            ParserVTable::<AbsPtr>::tsize(&data),
            SizeAlign {
                before: 0,
                after: 40,
                align_mask: 0b111,
            }
        );
        assert_eq!(
            ArrayVTable::<AbsPtr>::tsize(&data),
            SizeAlign {
                before: 0,
                after: 96,
                align_mask: 0b111,
            }
        );
    }
}
