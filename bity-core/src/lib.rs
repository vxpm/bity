pub mod prelude;
pub mod raw;

use arbitrary_int::Number;
pub use bity_impl::bity;

/// Trait for types that can try to be created from and turned into raw bits.
pub trait TryBits: Sized {
    /// The integer type with the same amount of bits as this value needs.
    type Raw: Number;

    fn try_from_raw(value: Self::Raw) -> Option<Self>;
    fn into_raw(self) -> Self::Raw;
}

/// Trait for types that can be created from and turned into raw bits.
pub trait Bits: TryBits {
    fn from_raw(value: Self::Raw) -> Self;
}

macro_rules! impl_bits {
    ($($ty:ty),*) => {
        $(
            impl<const LEN: usize> TryBits for arbitrary_int::UInt<$ty, LEN> {
                type Raw = Self;

                #[inline(always)]
                fn try_from_raw(value: Self::Raw) -> Option<Self> {
                    Some(value)
                }

                #[inline(always)]
                fn into_raw(self) -> Self::Raw {
                    self
                }
            }

            impl<const LEN: usize> Bits for arbitrary_int::UInt<$ty, LEN> {
                #[inline(always)]
                fn from_raw(value: Self::Raw) -> Self {
                    value
                }
            }
        )*
    };
}

impl_bits! {
    u8,
    u16,
    u32,
    u64
}

impl TryBits for bool {
    type Raw = raw::u1;

    #[inline(always)]
    fn try_from_raw(value: Self::Raw) -> Option<Self> {
        Some(value.into())
    }

    #[inline(always)]
    fn into_raw(self) -> Self::Raw {
        raw::u1::new(self.into())
    }
}

impl Bits for bool {
    #[inline(always)]
    fn from_raw(value: Self::Raw) -> Self {
        value.into()
    }
}
