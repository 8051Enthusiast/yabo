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
}

impl<T: TargetSized> TargetSized for &T {
    fn tsize() -> SizeAlign {
        TARGET_POINTER_SA
    }
}

impl<T: TargetSized> TargetSized for Option<&T> {
    fn tsize() -> SizeAlign {
        <&T>::tsize()
    }
}

impl TargetSized for Zst {
    fn tsize() -> SizeAlign {
        TARGET_ZST_SA
    }
}

impl TargetSized for char {
    fn tsize() -> SizeAlign {
        TARGET_CHAR_SA
    }
}

impl TargetSized for i64 {
    fn tsize() -> SizeAlign {
        TARGET_INT_SA
    }
}

impl TargetSized for u64 {
    fn tsize() -> SizeAlign {
        TARGET_INT_SA
    }
}

impl TargetSized for usize {
    fn tsize() -> SizeAlign {
        TARGET_SIZE_SA
    }
}

impl TargetSized for bool {
    fn tsize() -> SizeAlign {
        TARGET_BIT_SA
    }
}

impl<T: TargetSized, const N: usize> TargetSized for [T; N] {
    fn tsize() -> SizeAlign {
        T::tsize().array(N as PSize)
    }
}

#[macro_export]
macro_rules! target_struct {
    {$pub:vis struct $name:ident {$($field_pub:vis $field:ident: $ty:ty),*$(,)?}} => {
        #[repr(C)]
        $pub struct $name {$($field_pub $field: $ty),*}

        impl $crate::layout::size_align::TargetSized for $name {
            fn tsize() -> $crate::layout::size_align::SizeAlign {
                $crate::layout::size_align::Zst::tsize()
                $(
                    .cat(<$ty as $crate::layout::size_align::TargetSized>::tsize())
                )*
            }
        }
    };
}