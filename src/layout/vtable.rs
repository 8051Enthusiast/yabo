use crate::target_struct;

use super::size_align::Zst;

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
        pub layout: &'static LayoutKind,
        pub typecast: &'static Zst,
        pub size: usize,
        pub align: usize,
    }
}

target_struct! {
    pub struct BlockVTable {
        pub head: VTableHeader,
        pub access_funs: [&'static Zst; 0],
    }
}

target_struct! {
    pub struct NominalVTable {
        pub head: VTableHeader,
        pub deref_table: &'static VTableHeader,
    }
}

target_struct! {
    pub struct ParserArgImpl {
        pub vtable: Option<&'static VTableHeader>,
        pub implementation: Option<&'static Zst>,
    }
}

target_struct! {
    pub struct ParserVTable {
        pub head: VTableHeader,
        pub default_arg_impl: ParserArgImpl,
        pub apply_table: [ParserArgImpl; 0],
    }
}