pub mod prelude;
pub mod raw;

use arbitrary_int::Number;
pub use bity_impl::bity;
pub use bity_impl::BitySet;

/// Trait for types that can try to be created from and turned into raw bits.
pub trait TryBits: Sized {
    /// The integer type with the same amount of bits as this value needs.
    type Raw: Number + Copy;

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

/// Trait for types that can be used as the element type of a BitSet.
pub trait BitSetElement: Copy + Sized + 'static {
    type Raw: Number + Copy;
    const SET_MASK: Self::Raw;

    /// Slice with all possible values for this element.
    fn variants() -> &'static [Self];

    fn set_insert(set: Self::Raw, value: Self) -> Self::Raw;
    fn set_remove(set: Self::Raw, value: Self) -> Self::Raw;
    fn set_contains(set: Self::Raw, value: Self) -> bool;
    fn set_len(set: Self::Raw) -> usize;

    fn set_not(set: Self::Raw) -> Self::Raw;
    fn set_or(lhs: Self::Raw, rhs: Self::Raw) -> Self::Raw;
    fn set_and(lhs: Self::Raw, rhs: Self::Raw) -> Self::Raw;
}

/// A BitSet is, as the name implies, a set of values for which their presence is encoded by the
/// bits of an integer value.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct BitSet<E>
where
    E: BitSetElement,
{
    raw: E::Raw,
}

impl<E> BitSet<E>
where
    E: BitSetElement,
{
    /// Creates a new, empty set.
    #[inline]
    pub fn empty() -> Self {
        Self {
            raw: E::Raw::new(<E::Raw as Number>::UnderlyingType::from(0u8)),
        }
    }

    /// Inserts a value into this set, even if already present.
    #[inline]
    pub fn with(self, value: E) -> Self {
        Self {
            raw: E::set_insert(self.raw, value),
        }
    }

    /// Removes a value from this set, even if not present.
    #[inline]
    pub fn without(self, value: E) -> Self {
        Self {
            raw: E::set_remove(self.raw, value),
        }
    }

    /// Checks if this set contains the given value.
    #[inline]
    pub fn contains(self, value: E) -> bool {
        E::set_contains(self.raw, value)
    }

    /// The length of this set.
    #[inline]
    pub fn len(self) -> usize {
        E::set_len(self.raw)
    }

    /// Whether this set is empty or not.
    #[inline]
    pub fn is_empty(self) -> bool {
        self.len() == 0
    }

    /// Returns an iterator over all the values this set contains.
    #[inline]
    pub fn iter<'set>(&'set self) -> impl Iterator<Item = E> + 'set {
        E::variants()
            .into_iter()
            .copied()
            .filter(|v| self.contains(*v))
    }
}

impl<E> std::ops::Not for BitSet<E>
where
    E: BitSetElement,
{
    type Output = Self;

    #[inline]
    fn not(self) -> Self::Output {
        Self {
            raw: E::set_not(self.raw),
        }
    }
}

impl<E> std::ops::BitOr for BitSet<E>
where
    E: BitSetElement,
{
    type Output = Self;

    #[inline]
    fn bitor(self, rhs: Self) -> Self::Output {
        Self {
            raw: E::set_or(self.raw, rhs.raw),
        }
    }
}

impl<E> std::ops::BitAnd for BitSet<E>
where
    E: BitSetElement,
{
    type Output = Self;

    #[inline]
    fn bitand(self, rhs: Self) -> Self::Output {
        Self {
            raw: E::set_and(self.raw, rhs.raw),
        }
    }
}

impl<E> TryBits for BitSet<E>
where
    E: BitSetElement,
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

impl<E> Bits for BitSet<E>
where
    E: BitSetElement,
{
    #[inline]
    fn from_raw(value: Self::Raw) -> Self {
        Self { raw: value }
    }
}

impl<E> std::fmt::Debug for BitSet<E>
where
    E: BitSetElement + std::fmt::Debug,
{
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}
