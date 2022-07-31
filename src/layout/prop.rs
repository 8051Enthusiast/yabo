pub trait CodegenTypeContext {
    type Type;
    fn int(&mut self, bits: u8, signed: bool) -> Self::Type;
    fn size(&mut self, signed: bool) -> Self::Type;
    fn char(&mut self) -> Self::Type;
    fn ptr(&mut self) -> Self::Type;
    fn zst(&mut self) -> Self::Type;
    fn array(&mut self, ty: Self::Type, size: PSize) -> Self::Type;
    fn tuple(&mut self, fields: &[Self::Type]) -> Self::Type;
}

pub type PSize = u64;
pub const TARGET_POINTER_SA: SizeAlign = SizeAlign {
    size: 8,
    align_mask: 0b111,
};
pub const TARGET_SIZE_SA: SizeAlign = SizeAlign {
    size: 8,
    align_mask: 0b111,
};
pub const TARGET_BIT_SA: SizeAlign = SizeAlign {
    size: 1,
    align_mask: 0,
};
pub const TARGET_CHAR_SA: SizeAlign = SizeAlign {
    size: 4,
    align_mask: 0b11,
};
pub const TARGET_INT_SA: SizeAlign = SizeAlign {
    size: 8,
    align_mask: 0b111,
};
pub const TARGET_ZST_SA: SizeAlign = SizeAlign {
    size: 0,
    align_mask: 0,
};

pub struct Zst([u8; 0]);

#[derive(Clone, Copy, PartialEq, Eq, Default, Debug)]
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
        let align_mask = if self.align_mask > other.align_mask {
            self.align_mask
        } else {
            other.align_mask
        };
        SizeAlign {
            size: other_start + other.size,
            align_mask,
        }
    }
    pub const fn union(self, other: Self) -> Self {
        let size = if self.size > other.size {
            self.size
        } else {
            other.size
        };
        let align_mask = if self.align_mask > other.align_mask {
            self.align_mask
        } else {
            other.align_mask
        };
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
}

pub trait TargetSized: Sized {
    fn tsize() -> SizeAlign;
    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type;
}

impl<T: TargetSized> TargetSized for &T {
    fn tsize() -> SizeAlign {
        TARGET_POINTER_SA
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        ctx.ptr()
    }
}

impl<T: TargetSized> TargetSized for Option<&T> {
    fn tsize() -> SizeAlign {
        <&T>::tsize()
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        ctx.ptr()
    }
}

impl TargetSized for Zst {
    fn tsize() -> SizeAlign {
        TARGET_ZST_SA
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        ctx.zst()
    }
}

impl TargetSized for char {
    fn tsize() -> SizeAlign {
        TARGET_CHAR_SA
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        ctx.char()
    }
}

impl TargetSized for i64 {
    fn tsize() -> SizeAlign {
        TARGET_INT_SA
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        ctx.int(64, true)
    }
}

impl TargetSized for u64 {
    fn tsize() -> SizeAlign {
        TARGET_INT_SA
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        ctx.int(64, false)
    }
}

impl TargetSized for usize {
    fn tsize() -> SizeAlign {
        TARGET_SIZE_SA
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        ctx.size(false)
    }
}

impl TargetSized for bool {
    fn tsize() -> SizeAlign {
        TARGET_BIT_SA
    }

    fn codegen_ty<Ctx: CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
        ctx.int(1, false)
    }
}

impl<T: TargetSized, const N: usize> TargetSized for [T; N] {
    fn tsize() -> SizeAlign {
        T::tsize().array(N as PSize)
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

        impl $crate::layout::prop::TargetSized for $name {
            fn tsize() -> $crate::layout::prop::SizeAlign {
                $crate::layout::prop::Zst::tsize()
                $(
                    .cat(<$ty as $crate::layout::prop::TargetSized>::tsize())
                )*
            }

            fn codegen_ty<Ctx: $crate::layout::prop::CodegenTypeContext>(ctx: &mut Ctx) -> Ctx::Type {
                let arr = &[$(
                    <$ty as $crate::layout::prop::TargetSized>::codegen_ty(ctx)
                ),*];
                ctx.tuple(arr)
            }
        }

        paste::paste! {
            #[repr(u16)]
            #[allow(non_camel_case_types)]
            $pub enum [<$name Fields>] {
                $($field),*
            }
        }
    };
}
