#![no_std]

// This test makes sure that all of the impls generated do not rely on `std`,
// simply by using the macro in a `no_std` crate.

use newtype_ops::newtype_ops;

// derive everything under the sun
#[derive(Debug, PartialEq)]
pub struct Foo(pub i32);
newtype_ops! { [Foo] integer {:=} {^&}Self {^&}{Self i32} }

// derive everything under the sun for floats
#[derive(Debug, PartialEq)]
pub struct Bar(pub f32);
newtype_ops! { [Bar] arithmetic {:=} {^&}Self {^&}{Self f32} }

#[test]
fn test() {
    assert_eq!(Foo(1) + 2, Foo(3));
    assert_eq!(Foo(1) + &2, Foo(3));
    assert_eq!(Foo(1) + Foo(2), Foo(3));
    assert_eq!(Bar(1.0) + 2.0, Bar(3.0));
    assert_eq!(Bar(1.0) + &2.0, Bar(3.0));
    assert_eq!(Bar(1.0) + Bar(2.0), Bar(3.0));
}
