use std::marker::PhantomData;

use yaboc_target::{layout::VtablePointer, target_struct};

pub type TypecastFun = fn(ret: *mut u8, from: *const u8, target_head: i64) -> i64;
pub type MaskFun = fn(ret: *mut u8) -> usize;

target_struct! {
    pub struct VTableHeader<T: VtablePointer> {
        pub head: i64,
        pub deref_level: i64,
        pub typecast_impl: T::FPtr<TypecastFun>,
        pub mask_impl: T::FPtr<MaskFun>,
        pub size: usize,
        pub align: usize,
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

pub type BlockFieldFun = fn(ret: *mut u8, from: *const u8, target_head: i64) -> i64;

target_struct! {
    pub struct BlockVTable<T: VtablePointer> {
        pub head: VTableHeader<T>,
        pub fields: T::Ptr<BlockFields<T>>,
        pub access_impl: [T::FPtr<BlockFieldFun>; 0],
    }
}

pub type StartFun = fn(ret: *mut u8, nom: *const u8, target_head: i64) -> i64;
pub type EndFun = fn(ret: *mut u8, nom: *const u8, target_head: i64) -> i64;

target_struct! {
    pub struct NominalVTable<T: VtablePointer> {
        pub head: VTableHeader<T>,
        pub name: T::Ptr<u8>,
        pub start_impl: T::FPtr<StartFun>,
        pub end_impl: T::FPtr<EndFun>,
    }
}

target_struct! {
    pub struct ArgDescriptor {
        pub head: i64,
        pub offset: usize,
    }
}

pub type ParserFun = fn(ret: *mut u8, fun: *const u8, target_head: i64, from: *const u8) -> i64;
pub type LenFun = fn(ret: *mut u8, from: *const u8) -> i64;

target_struct! {
    pub struct ParserVTable<T: VtablePointer> {
        pub set_arg_info: [ArgDescriptor; 0],
        pub head: VTableHeader<T>,
        pub len_impl: Option<T::FPtr<LenFun>>,
        pub apply_table: [Option<T::FPtr<ParserFun>>; 0],
    }
}

pub type CreateArgFun = fn(ret: *mut u8, from: *const u8, target_head: i64) -> i64;
pub type EvalFunFun = fn(ret: *mut u8, fun: *const u8, target_head: i64) -> i64;

target_struct! {
    pub struct FunctionVTable<T: VtablePointer> {
        pub set_arg_info: [ArgDescriptor; 0],
        pub head: VTableHeader<T>,
        pub eval_fun_impl: Option<T::FPtr<EvalFunFun>>,
        pub apply_table: [Option<T::FPtr<CreateArgFun>>; 0],
    }
}

pub type SingleForwardFun = fn(from: *mut u8) -> i64;
pub type CurrentElementFun = fn(ret: *mut u8, from: *const u8, target_head: i64) -> i64;
pub type ArrayLenFun = fn(from: *const u8) -> i64;
pub type SkipFun = fn(ret: *mut u8, offset: u64) -> i64;
pub type SpanFun = fn(ret: *mut u8, from: *const u8, target_head: i64, to: *const u8) -> i64;
pub type InnerArrayFun = fn(ret: *mut u8, from: *const u8, target_head: i64) -> i64;

target_struct! {
    pub struct ArrayVTable<T: VtablePointer> {
        pub head: VTableHeader<T>,
        pub single_forward_impl: T::FPtr<SingleForwardFun>,
        pub current_element_impl: T::FPtr<CurrentElementFun>,
        pub len_impl: Option<T::FPtr<ArrayLenFun>>,
        pub skip_impl: Option<T::FPtr<SkipFun>>,
        pub span_impl: Option<T::FPtr<SpanFun>>,
        pub inner_array_impl: Option<T::FPtr<InnerArrayFun>>,
    }
}

target_struct! {
    pub struct ParserExport<T: VtablePointer> {
        pub parser: T::FPtr<ParserFun>,
        pub phantom: PhantomData<T>,
        pub args: [T::Ptr<VTableHeader<T>>; 0],
    }
}

pub type InitFun = fn(start: *const u8, end: *const u8) -> i64;

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
                after: 48,
                align_mask: 0b111
            }
        );
        assert_eq!(
            BlockVTable::<AbsPtr>::tsize(&data),
            SizeAlign {
                before: 0,
                after: 56,
                align_mask: 0b111,
            }
        );
        assert_eq!(
            ParserVTable::<AbsPtr>::tsize(&data),
            SizeAlign {
                before: 0,
                after: 56,
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
