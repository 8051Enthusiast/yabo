use crate::target_struct;

use super::prop::Zst;

target_struct! {
    pub struct LayoutKind {
        pub head: i64,
        pub number_fields: usize,
        pub fields: [FieldInfo; 0],
    }
}

target_struct! {
    pub struct FieldInfo {
        pub name: &'static Zst,
        pub layout: &'static LayoutKind,
    }
}

target_struct! {
    pub struct VTableHeader {
        pub kind: &'static LayoutKind,
        pub typecast_impl: &'static Zst,
        pub size: usize,
        pub align: usize,
    }
}

target_struct! {
    pub struct BlockVTable {
        pub head: VTableHeader,
        pub access_impl: [&'static Zst; 0],
    }
}

target_struct! {
    pub struct NominalVTable {
        pub head: VTableHeader,
        pub deref_table: &'static VTableHeader,
        pub start_impl: &'static Zst,
        pub end_impl: &'static Zst,
    }
}

target_struct! {
    pub struct ParserArgImpl {
        pub vtable: Option<&'static VTableHeader>,
        pub val_impl: Option<&'static Zst>,
        pub len_impl: Option<&'static Zst>,
    }
}

target_struct! {
    pub struct ParserVTable {
        pub head: VTableHeader,
        pub default_arg_impl: ParserArgImpl,
        pub apply_table: [ParserArgImpl; 0],
    }
}

target_struct! {
    pub struct ArrayVTable {
        pub head: VTableHeader,
        pub single_forward_impl: &'static Zst,
        pub current_element_impl: &'static Zst,
        pub skip_impl: Option<&'static Zst>,
    }
}

#[cfg(test)]
mod tests {
    use crate::layout::prop::{SizeAlign, TargetSized};

    use super::*;

    #[test]
    fn vtable_sizes() {
        assert_eq!(
            VTableHeader::tsize(),
            SizeAlign {
                size: 32,
                align_mask: 0b111
            }
        );
        assert_eq!(
            BlockVTable::tsize(),
            SizeAlign {
                size: 32,
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
            LayoutKind::tsize(),
            SizeAlign {
                size: 16,
                align_mask: 0b111,
            }
        );
        assert_eq!(
            ParserArgImpl::tsize(),
            SizeAlign {
                size: 24,
                align_mask: 0b111,
            }
        );
        assert_eq!(
            ArrayVTable::tsize(),
            SizeAlign {
                size: 56,
                align_mask: 0b111,
            }
        );
    }
}