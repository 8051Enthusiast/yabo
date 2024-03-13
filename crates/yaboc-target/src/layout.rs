pub trait CodegenTypeContext {
    type Type;
    fn int(&mut self, bits: u8, signed: bool) -> Self::Type;
    fn size(&mut self, signed: bool) -> Self::Type;
    fn char(&mut self) -> Self::Type;
    fn ptr(&mut self, inner: Self::Type) -> Self::Type;
    fn zst(&mut self) -> Self::Type;
    fn array(&mut self, ty: Self::Type, size: PSize) -> Self::Type;
    fn fun_ptr(&mut self, args: &[Self::Type], ret: Self::Type) -> Self::Type;
    type StructType: Into<Self::Type>;
    fn tuple(&mut self, fields: &[Self::Type]) -> Self::StructType;
}

pub type PSize = u64;

pub struct Zst([u8; 0]);

pub(crate) const fn max(a: PSize, b: PSize) -> PSize {
    if a > b {
        a
    } else {
        b
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Default, Debug, Hash)]
pub struct SizeAlign {
    /// total size before center (not including potential alignment padding at the start)
    pub before: PSize,
    /// total size after center (not including potential alignment padding at the end)
    pub after: PSize,
    /// mask of alignment
    pub align_mask: PSize,
}

impl SizeAlign {
    pub const ZST: SizeAlign = SizeAlign {
        before: 0,
        after: 0,
        align_mask: 0,
    };
    pub const fn align(&self) -> PSize {
        self.align_mask + 1
    }
    /// log2 of alignment
    pub const fn alogn(&self) -> u32 {
        self.align_mask.count_ones()
    }
    pub const fn stride(&self) -> PSize {
        (self.after + self.before + self.align_mask) & !self.align_mask
    }
    pub const fn cat(self, other: Self) -> Self {
        let other_center = (other.align_mask + self.after + other.before) & !other.align_mask;
        let align_mask = max(self.align_mask, other.align_mask);
        SizeAlign {
            before: self.before,
            after: other_center + other.after,
            align_mask,
        }
    }
    pub const fn tac(self, other: Self) -> Self {
        let other_center = (other.align_mask + self.before + other.after) & !other.align_mask;
        let align_mask = max(self.align_mask, other.align_mask);
        SizeAlign {
            before: other_center + other.before,
            after: self.after,
            align_mask,
        }
    }
    pub const fn allocation_size(self) -> PSize {
        Self::ZST.cat(self).after
    }
    pub const fn total_size(self) -> PSize {
        self.before + self.after
    }
    pub const fn next_offset(self, offset: PSize) -> PSize {
        (offset + self.before + self.align_mask) & !self.align_mask
    }
    pub const fn allocation_center_offset(self) -> PSize {
        (self.before + self.align_mask) & !self.align_mask
    }
    pub const fn start_alignment(self) -> PSize {
        let before_align_mask = (self.before ^ (self.before.wrapping_sub(1))) / 2;
        (before_align_mask & self.align_mask) + 1
    }
    pub const fn union(self, other: Self) -> Self {
        let after = max(self.after, other.after);
        let before = max(self.before, other.before);
        let align_mask = max(self.align_mask, other.align_mask);
        SizeAlign {
            before,
            after,
            align_mask,
        }
    }
    pub const fn array(self, len: PSize) -> Self {
        if len == 0 {
            return SizeAlign {
                after: 0,
                before: 0,
                align_mask: self.align_mask,
            };
        }
        SizeAlign {
            before: self.before,
            after: self.stride() * (len - 1) + self.after,
            align_mask: self.align_mask,
        }
    }
    pub const fn array_offset(self, index: PSize) -> PSize {
        self.stride() * index
    }
    pub const fn int_sa(size_log: u8) -> Self {
        SizeAlign {
            before: 0,
            after: 1 << size_log,
            align_mask: (1 << size_log) - 1,
        }
    }
    pub fn offsets<const N: usize>(layouts: [SizeAlign; N]) -> [PSize; N] {
        let mut current = Self::ZST;
        layouts.map(|sa| {
            let offset = sa.next_offset(current.after);
            current = current.cat(sa);
            offset
        })
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TargetLayoutData {
    pub pointer_sa: SizeAlign,
    pub int_sa: SizeAlign,
    pub byte_sa: SizeAlign,
    pub bit_sa: SizeAlign,
    pub char_sa: SizeAlign,
}

pub trait TargetSized: Sized {
    fn tsize(data: &TargetLayoutData) -> SizeAlign;
    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type;
}

impl<T: TargetSized> TargetSized for &T {
    fn tsize(data: &TargetLayoutData) -> SizeAlign {
        data.pointer_sa
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        let inner = T::codegen_ty(ctx);
        ctx.ptr(inner)
    }
}

impl<T: TargetSized> TargetSized for *const T {
    fn tsize(data: &TargetLayoutData) -> SizeAlign {
        data.pointer_sa
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        let inner = T::codegen_ty(ctx);
        ctx.ptr(inner)
    }
}

impl<T: TargetSized> TargetSized for *mut T {
    fn tsize(data: &TargetLayoutData) -> SizeAlign {
        data.pointer_sa
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        let inner = T::codegen_ty(ctx);
        ctx.ptr(inner)
    }
}

impl<T: TargetSized> TargetSized for Option<&T> {
    fn tsize(data: &TargetLayoutData) -> SizeAlign {
        <&T>::tsize(data)
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        let inner = T::codegen_ty(ctx);
        ctx.ptr(inner)
    }
}

macro_rules! fn_impl {
    ($($name:ident),*) => {
        impl<Ret: TargetSized, $($name: TargetSized),*> TargetSized for fn($($name),*) -> Ret {
            fn tsize(data: &TargetLayoutData) -> SizeAlign {
                data.pointer_sa
            }

            fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
                let pointers = [$(
                    $name::codegen_ty(ctx)
                ),*
                ];
                let ret = Ret::codegen_ty(ctx);
                ctx.fun_ptr(&pointers, ret)
            }
        }
        impl<Ret: TargetSized, $($name: TargetSized),*> TargetSized for Option<fn($($name),*) -> Ret> {
            fn tsize(data: &TargetLayoutData) -> SizeAlign {
                data.pointer_sa
            }

            fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
                let pointers = [$(
                    $name::codegen_ty(ctx)
                ),*
                ];
                let ret = Ret::codegen_ty(ctx);
                ctx.fun_ptr(&pointers, ret)
            }
        }
    };
}

fn_impl!(A);

fn_impl!(A, B);

fn_impl!(A, B, C);

fn_impl!(A, B, C, D);

fn_impl!(A, B, C, D, E);

impl TargetSized for Zst {
    fn tsize(_: &TargetLayoutData) -> SizeAlign {
        SizeAlign {
            before: 0,
            after: 0,
            align_mask: 0,
        }
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        ctx.zst()
    }
}

impl TargetSized for char {
    fn tsize(data: &TargetLayoutData) -> SizeAlign {
        data.char_sa
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        ctx.char()
    }
}

impl TargetSized for i64 {
    fn tsize(data: &TargetLayoutData) -> SizeAlign {
        data.int_sa
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        ctx.int(64, true)
    }
}

impl TargetSized for u8 {
    fn tsize(data: &TargetLayoutData) -> SizeAlign {
        data.byte_sa
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        ctx.int(8, false)
    }
}

impl TargetSized for u64 {
    fn tsize(data: &TargetLayoutData) -> SizeAlign {
        data.int_sa
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        ctx.int(64, false)
    }
}

impl TargetSized for usize {
    fn tsize(data: &TargetLayoutData) -> SizeAlign {
        data.pointer_sa
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        ctx.size(false)
    }
}

impl TargetSized for bool {
    fn tsize(data: &TargetLayoutData) -> SizeAlign {
        data.bit_sa
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        ctx.int(1, false)
    }
}

impl<T: TargetSized, const N: usize> TargetSized for [T; N] {
    fn tsize(data: &TargetLayoutData) -> SizeAlign {
        T::tsize(data).array(N as PSize)
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        let inner = T::codegen_ty(ctx);
        ctx.array(inner, N as PSize)
    }
}

#[macro_export]
macro_rules! target_struct {
    {$pub:vis struct $name:ident {$($field_pub:vis $field:ident: $ty:ty),*$(,)?}} => {
        #[repr(C)]
        $pub struct $name {$($field_pub $field: $ty),*}

        impl $crate::layout::TargetSized for $name {
            fn tsize(data: &$crate::layout::TargetLayoutData) -> $crate::layout::SizeAlign {
                $crate::layout::Zst::tsize(data)
                $(
                    .cat(<$ty as $crate::layout::TargetSized>::tsize(data))
                )*
            }

            fn codegen_ty<Ctx: $crate::layout::CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
                $name::struct_type(ctx).into()
            }
        }

        impl $name {
            pub fn struct_type<Ctx: $crate::layout::CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::StructType {
                let arr = &[$(
                    <$ty as $crate::layout::TargetSized>::codegen_ty(ctx)
                ),*];
                ctx.tuple(arr)
            }
        }

        $crate::paste::paste! {
            #[allow(non_camel_case_types)]
            #[allow(unused)]
            $pub enum [<$name Fields>] {
                $($field),*
            }
        }
    };
}

pub const POINTER64: TargetLayoutData = TargetLayoutData {
    pointer_sa: SizeAlign::int_sa(3),
    int_sa: SizeAlign::int_sa(3),
    byte_sa: SizeAlign::int_sa(0),
    bit_sa: SizeAlign::int_sa(0),
    char_sa: SizeAlign::int_sa(2),
};

pub const POINTER32: TargetLayoutData = TargetLayoutData {
    pointer_sa: SizeAlign::int_sa(2),
    int_sa: SizeAlign {
        before: 0,
        after: 8,
        align_mask: 7,
    },
    byte_sa: SizeAlign::int_sa(0),
    bit_sa: SizeAlign::int_sa(0),
    char_sa: SizeAlign::int_sa(2),
};

#[cfg(test)]
mod tests {
    use crate::layout::SizeAlign;

    use super::TargetSized;

    #[test]
    fn struct_size() {
        target_struct!(
            pub struct Test {
                pub a: u8,
                pub b: u64,
                pub c: u8,
            }
        );
        let data = super::POINTER64;
        assert_eq!(
            Test::tsize(&data),
            SizeAlign {
                before: 0,
                after: 17,
                align_mask: 7
            }
        );
    }
    #[test]
    fn before() {
        let sa = SizeAlign::int_sa(3).tac(SizeAlign::int_sa(2));
        assert_eq!(sa.before, 4);
        assert_eq!(sa.after, 8);
        assert_eq!(sa.align_mask, 7);
        assert_eq!(sa.allocation_size(), 16);
        assert_eq!(sa.allocation_center_offset(), 8);
        assert_eq!(sa.start_alignment(), 4);
        assert_eq!(sa.stride(), 16);
    }
}
