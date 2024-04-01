use bity::prelude::*;
use bity_impl::bity;

#[bity(32)]
#[derive(PartialEq, Eq)]
struct Arr {
    #[bits(0..4)]
    arr: [bool; 4],
}

#[test]
fn test_array() {
    let a0 = Arr::from_raw(0b1011);
    assert!(a0.arr_at(0));
    assert!(a0.arr_at(1));
    assert!(!a0.arr_at(2));
    assert!(a0.arr_at(3));
    assert_eq!(a0.arr(), [true, true, false, true]);

    let a1 = a0
        .with_arr_at(0, false)
        .with_arr_at(1, false)
        .with_arr_at(2, true)
        .with_arr_at(3, false);

    assert!(!a1.arr_at(0));
    assert!(!a1.arr_at(1));
    assert!(a1.arr_at(2));
    assert!(!a1.arr_at(3));
    assert_eq!(a1.arr(), [false, false, true, false]);

    let a2 = a0.with_arr([false, false, true, false]);
    assert!(!a2.arr_at(0));
    assert!(!a2.arr_at(1));
    assert!(a2.arr_at(2));
    assert!(!a2.arr_at(3));
    assert_eq!(a2.arr(), [false, false, true, false]);
}
