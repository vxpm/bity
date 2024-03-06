use bity::prelude::*;
use bity_impl::bity;

#[bity(2)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Foo {
    A = 0x00,
    B = 0x01,
    C = 0x02,
}

#[bity(2)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Bar {
    A = 0x00,
    B = 0x01,
    C = 0x02,
    D = 0x03,
}

#[bity(8)]
#[derive(Clone, Copy)]
struct Test {
    #[bits(0..2)]
    foo: Option<Foo>,
    #[bits(2..4)]
    bar: Bar,
}

#[test]
fn test_enum() {
    let t0 = Test::from_raw(0b10_11);
    assert_eq!(t0.foo(), None);
    assert_eq!(t0.bar(), Bar::C);
}
