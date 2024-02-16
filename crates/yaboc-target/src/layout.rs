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
    /// total size (not including potential alignment padding at the end)
    pub size: PSize,
    /// mask of alignment
    pub align_mask: PSize,
}

impl SizeAlign {
    pub const fn align(&self) -> PSize {
        self.align_mask + 1
    }
    /// log2 of alignment
    pub const fn alogn(&self) -> u32 {
        self.align_mask.count_ones()
    }
    pub const fn stride(&self) -> PSize {
        (self.size + self.align_mask) & !self.align_mask
    }
    pub const fn cat(self, other: Self) -> Self {
        let other_start = (other.align_mask + self.size) & !other.align_mask;
        let align_mask = max(self.align_mask, other.align_mask);
        SizeAlign {
            size: other_start + other.size,
            align_mask,
        }
    }
    pub const fn union(self, other: Self) -> Self {
        let size = max(self.size, other.size);
        let align_mask = max(self.align_mask, other.align_mask);
        SizeAlign { size, align_mask }
    }
    pub const fn array(self, len: PSize) -> Self {
        if len == 0 {
            return SizeAlign { size: 0, ..self };
        }
        SizeAlign {
            size: self.stride() * (len - 1) + self.size,
            align_mask: self.align_mask,
        }
    }
    pub const fn array_offset(self, index: PSize) -> PSize {
        self.stride() * index
    }
    pub const fn int_sa(size_log: u8) -> Self {
        SizeAlign {
            size: 1 << size_log,
            align_mask: (1 << size_log) - 1,
        }
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
            size: 0,
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
    pointer_sa: SizeAlign {
        size: 8,
        align_mask: 7,
    },
    int_sa: SizeAlign {
        size: 8,
        align_mask: 7,
    },
    byte_sa: SizeAlign {
        size: 1,
        align_mask: 0,
    },
    bit_sa: SizeAlign {
        size: 1,
        align_mask: 0,
    },
    char_sa: SizeAlign {
        size: 4,
        align_mask: 3,
    },
};

pub const POINTER32: TargetLayoutData = TargetLayoutData {
    pointer_sa: SizeAlign {
        size: 4,
        align_mask: 3,
    },
    int_sa: SizeAlign {
        size: 4,
        align_mask: 3,
    },
    byte_sa: SizeAlign {
        size: 1,
        align_mask: 0,
    },
    bit_sa: SizeAlign {
        size: 1,
        align_mask: 0,
    },
    char_sa: SizeAlign {
        size: 4,
        align_mask: 3,
    },
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
                size: 17,
                align_mask: 7
            }
        );
    }
}
