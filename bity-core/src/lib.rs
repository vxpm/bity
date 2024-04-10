pub mod prelude;
pub mod raw;

pub use bity_impl::bity;
pub use bity_impl::VariantSetEnum;

use arbitrary_int::Number;
use num_traits::{PrimInt, Zero};

/// Trait for types that can try to be created from and turned into raw bits.
pub trait TryBits: Sized {
    /// The integer type with the same amount of bits as this value needs.
    type Raw: Number + Copy + PrimInt;

    /// Tries to create a value of this type from it's raw bit representation.
    fn try_from_raw(value: Self::Raw) -> Option<Self>;

    /// Turns this value into it's raw bit representation.
    fn into_raw(self) -> Self::Raw;
}

/// Trait for types that can be created from and turned into raw bits.
pub trait Bits: TryBits {
    /// Creates a value of this type from it's raw bit representation.
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

            impl TryBits for $ty {
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

            impl Bits for $ty {
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

/// Trait for enums that can be used as the element type of a VariantSet.
pub trait VariantSetEnum: TryBits + Copy + Sized + 'static {
    /// Slice with all possible values for this element.
    const VARIANTS: &'static [Self];

    /// Mask for the valid bits of this set.
    const SET_MASK: Self::Raw;

    fn into_bit(self) -> Self::Raw;
}

/// A set of enum values for which their presence is encoded by the bits of an integer value.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct VariantSet<E>
where
    E: VariantSetEnum,
{
    raw: E::Raw,
}

impl<E> VariantSet<E>
where
    E: VariantSetEnum,
{
    /// Creates a new, empty set.
    #[inline]
    pub fn empty() -> Self {
        Self {
            raw: E::Raw::zero(),
        }
    }

    /// Inserts a value into this set, even if already present.
    #[inline]
    pub fn with(self, value: E) -> Self {
        Self {
            raw: self.raw | value.into_bit(),
        }
    }

    /// Removes a value from this set, even if not present.
    #[inline]
    pub fn without(self, value: E) -> Self {
        Self {
            raw: self.raw & !value.into_bit(),
        }
    }

    /// Checks if this set contains the given value.
    #[inline]
    pub fn contains(self, value: E) -> bool {
        self.raw & value.into_bit() != E::Raw::zero()
    }

    /// The length of this set.
    #[inline]
    pub fn len(self) -> usize {
        (self.raw & E::SET_MASK).count_ones() as usize
    }

    /// Whether this set is empty or not.
    #[inline]
    pub fn is_empty(self) -> bool {
        self.len() == 0
    }

    /// Returns an iterator over all the values this set contains.
    #[inline]
    pub fn iter<'set>(&'set self) -> impl Iterator<Item = E> + 'set {
        E::VARIANTS
            .into_iter()
            .copied()
            .filter(|v| self.contains(*v))
    }
}

impl<E> std::ops::Not for VariantSet<E>
where
    E: VariantSetEnum,
{
    type Output = Self;

    #[inline]
    fn not(self) -> Self::Output {
        Self { raw: !self.raw }
    }
}

impl<E> std::ops::BitOr for VariantSet<E>
where
    E: VariantSetEnum,
{
    type Output = Self;

    #[inline]
    fn bitor(self, rhs: Self) -> Self::Output {
        Self {
            raw: self.raw | rhs.raw,
        }
    }
}

impl<E> std::ops::BitAnd for VariantSet<E>
where
    E: VariantSetEnum,
{
    type Output = Self;

    #[inline]
    fn bitand(self, rhs: Self) -> Self::Output {
        Self {
            raw: self.raw & rhs.raw,
        }
    }
}

impl<E> TryBits for VariantSet<E>
where
    E: VariantSetEnum,
{
    type Raw = E::Raw;

    #[inline]
    fn try_from_raw(value: Self::Raw) -> Option<Self> {
        Some(Self { raw: value })
    }

    #[inline]
    fn into_raw(self) -> Self::Raw {
        self.raw
    }
}

impl<E> Bits for VariantSet<E>
where
    E: VariantSetEnum,
{
    #[inline]
    fn from_raw(value: Self::Raw) -> Self {
        Self { raw: value }
    }
}

impl<E> std::fmt::Debug for VariantSet<E>
where
    E: VariantSetEnum + std::fmt::Debug,
{
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}
