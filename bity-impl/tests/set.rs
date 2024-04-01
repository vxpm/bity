use bity::{prelude::*, BitSet};
use bity_impl::BitySet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, BitySet)]
#[bity_set(2)]
enum Elem {
    A = 0b01,
    B = 0b10,
}

#[test]
fn test_set() {
    let mut set = BitSet::empty().with(Elem::A);
    assert!(set.contains(Elem::A));
    assert!(set.len() == 1);

    set = set.without(Elem::B);
    assert!(set.contains(Elem::A));
    assert!(set.len() == 1);
}
