use crate::target_struct;

pub type TypecastFun = fn(ret: *mut u8, from: *const u8, target_head: i64) -> i64;

target_struct! {
    pub struct VTableHeader {
        pub head: i64,
        pub deref_level: usize,
        pub typecast_impl: TypecastFun,
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

pub type DerefFun = fn(ret: *mut u8, from: *const u8, target_head: i64) -> i64;

target_struct! {
    pub struct NominalVTable {
        pub head: VTableHeader,
        pub deref_impl: DerefFun,
        pub start_impl: fn(nom: *const u8, ret: *mut u8) -> i64,
        pub end_impl: fn(nom: *const u8, ret: *mut u8) -> i64,
    }
}

target_struct! {
    pub struct ParserArgImpl {
        pub val_impl: Option<
            fn(fun: *const u8, from: *const u8, target_head: i64, ret: *mut u8) -> i64
        >,
        pub len_impl: Option<fn(fun: *const u8, from: *const u8, ret: *mut u8) -> i64>,
    }
}

target_struct! {
    pub struct ArgDescriptor {
        pub head: i64,
        pub offset: usize,
    }
}

target_struct! {
    pub struct ParserVTable {
        pub set_arg_info: [ArgDescriptor; 0],
        pub head: VTableHeader,
        //pub default_arg_impl: ParserArgImpl,
        pub apply_table: [ParserArgImpl; 0],
    }
}

pub type CreateArgFun = fn(ret: *mut u8, from: *const u8, target_head: i64) -> i64;

target_struct! {
    pub struct FunctionVTable {
        pub set_arg_info: [ArgDescriptor; 0],
        pub head: VTableHeader,
        pub apply_table: [CreateArgFun; 0],
    }
}

pub type SingleForwardFun = fn(ret: *mut u8, from: *const u8) -> i64;
pub type CurrentElementFun = fn(ret: *mut u8, from: *const u8, target_head: i64, ) -> i64;
pub type SkipFun = fn(ret: *mut u8, from: *const u8, offset: u64) -> i64;

target_struct! {
    pub struct ArrayVTable {
        pub head: VTableHeader,
        pub single_forward_impl: SingleForwardFun,
        pub current_element_impl: CurrentElementFun,
        pub skip_impl: Option<SkipFun>,
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
                size: 40,
                align_mask: 0b111
            }
        );
        assert_eq!(
            BlockVTable::tsize(),
            SizeAlign {
                size: 48,
                align_mask: 0b111,
            }
        );
        assert_eq!(
            ParserVTable::tsize(),
            SizeAlign {
                size: 40,
                align_mask: 0b111,
            }
        );
        assert_eq!(
            ParserArgImpl::tsize(),
            SizeAlign {
                size: 16,
                align_mask: 0b111,
            }
        );
        assert_eq!(
            ArrayVTable::tsize(),
            SizeAlign {
                size: 64,
                align_mask: 0b111,
            }
        );
    }
}
