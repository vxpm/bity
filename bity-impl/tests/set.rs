use bity::{prelude::*, VariantSet};
use bity_impl::VariantSetEnum;

#[bity(4)]
#[derive(Debug, PartialEq, Eq, VariantSetEnum)]
#[bity_set(4)]
enum Elem {
    A = 1,
    B = 3,
}

#[bity(8)]
struct Foo {
    #[bits(0..4)]
    set: VariantSet<Elem>,
}

#[test]
fn test_set() {
    let mut set = VariantSet::empty().with(Elem::A);
    assert!(set.contains(Elem::A));
    assert!(set.len() == 1);

    set = set.without(Elem::B);
    assert!(set.contains(Elem::A));
    assert!(set.len() == 1);
}

#[test]
fn test_foo() {
    let foo = Foo::from_raw(0b1);
    assert!(foo.set().contains(Elem::A));
    assert!(!foo.set().contains(Elem::B));

    let foo = Foo::from_raw(0b100);
    assert!(!foo.set().contains(Elem::A));
    assert!(foo.set().contains(Elem::B));

    let foo = Foo::from_raw(0b101);
    assert!(foo.set().contains(Elem::A));
    assert!(foo.set().contains(Elem::B));
}
