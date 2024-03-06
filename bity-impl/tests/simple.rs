use bity::prelude::*;
use bity_impl::bity;

#[bity(32)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Simple {
    /// This is a test flag.
    #[bits(0..1)]
    flag: bool,
    #[bits(1..4)]
    value: u3,
}

#[test]
fn test_simple() {
    let s0 = Simple::from_raw(0b101_1);

    assert_eq!(s0.flag(), true);
    assert_eq!(s0.value(), u3::new(5));
    let s1 = s0.with_flag(false);
    assert_eq!(s1.flag(), false);
    assert_eq!(s1.value(), u3::new(5));
    let s2 = s1.with_flag(true);
    assert_eq!(s0, s2);

    let s1 = s0.with_value(u3::new(0));
    assert_eq!(s1.value(), u3::new(0));
    let s2 = s1.with_value(u3::new(7));
    assert_eq!(s2.value(), u3::new(7));
    let s3 = s2.with_value(u3::new(5));
    assert_eq!(s0, s3);
}
