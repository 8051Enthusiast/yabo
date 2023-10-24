use crate::target_struct;

pub type TypecastFun = fn(ret: *mut u8, from: *const u8, target_head: i64) -> i64;
pub type MaskFun = fn(ret: *mut u8) -> usize;

target_struct! {
    pub struct VTableHeader {
        pub head: i64,
        pub deref_level: usize,
        pub typecast_impl: TypecastFun,
        pub mask_impl: MaskFun,
        pub size: usize,
        pub align: usize,
    }
}

target_struct! {
    pub struct BlockFields {
        pub number_fields: usize,
        pub fields: [*const u8; 0],
    }
}

pub type BlockFieldFun = fn(ret: *mut u8, from: *const u8, target_head: i64) -> i64;

target_struct! {
    pub struct BlockVTable {
        pub head: VTableHeader,
        pub fields: &'static BlockFields,
        pub access_impl: [BlockFieldFun; 0],
    }
}

pub type StartFun = fn(ret: *mut u8, nom: *const u8, target_head: i64) -> i64;
pub type EndFun = fn(ret: *mut u8, nom: *const u8, target_head: i64) -> i64;

target_struct! {
    pub struct NominalVTable {
        pub head: VTableHeader,
        pub name: *const u8,
        pub start_impl: StartFun,
        pub end_impl: EndFun,
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
    pub struct ParserVTable {
        pub set_arg_info: [ArgDescriptor; 0],
        pub head: VTableHeader,
        pub len_impl: Option<LenFun>,
        pub apply_table: [Option<ParserFun>; 0],
    }
}

pub type CreateArgFun = fn(ret: *mut u8, from: *const u8, target_head: i64) -> i64;
pub type EvalFunFun = fn(ret: *mut u8, fun: *const u8, target_head: i64) -> i64;

target_struct! {
    pub struct FunctionVTable {
        pub set_arg_info: [ArgDescriptor; 0],
        pub head: VTableHeader,
        pub eval_fun_impl: Option<EvalFunFun>,
        pub apply_table: [CreateArgFun; 0],
    }
}

pub type SingleForwardFun = fn(from: *mut u8) -> i64;
pub type CurrentElementFun = fn(ret: *mut u8, from: *const u8, target_head: i64) -> i64;
pub type ArrayLenFun = fn(from: *const u8) -> i64;
pub type SkipFun = fn(ret: *mut u8, offset: u64) -> i64;
pub type SpanFun = fn(ret: *mut u8, from: *const u8, target_head: i64, to: *const u8) -> i64;
pub type InnerArrayFun = fn(ret: *mut u8, from: *const u8, target_head: i64) -> i64;

target_struct! {
    pub struct ArrayVTable {
        pub head: VTableHeader,
        pub single_forward_impl: SingleForwardFun,
        pub current_element_impl: CurrentElementFun,
        pub len_impl: Option<ArrayLenFun>,
        pub skip_impl: Option<SkipFun>,
        pub span_impl: Option<SpanFun>,
        pub inner_array_impl: Option<InnerArrayFun>,
    }
}

#[cfg(test)]
mod tests {
    use crate::prop::{SizeAlign, TargetSized};

    use super::*;

    #[test]
    fn vtable_sizes() {
        assert_eq!(
            VTableHeader::tsize(),
            SizeAlign {
                size: 48,
                align_mask: 0b111
            }
        );
        assert_eq!(
            BlockVTable::tsize(),
            SizeAlign {
                size: 56,
                align_mask: 0b111,
            }
        );
        assert_eq!(
            ParserVTable::tsize(),
            SizeAlign {
                size: 56,
                align_mask: 0b111,
            }
        );
        assert_eq!(
            ArrayVTable::tsize(),
            SizeAlign {
                size: 96,
                align_mask: 0b111,
            }
        );
    }
}
