use newtype_ops::newtype_ops;

#[derive(Debug, PartialEq)]
pub struct Foo(pub i32);
newtype_ops! { [Foo] integer {:=} {^&}Self {^&}{Self i32} }

#[derive(Debug, PartialEq)]
pub struct Bar(pub f32);
newtype_ops! { [Bar] arithmetic {:=} {^&}Self {^&}{Self f32} }

fn main() {
    assert_eq!(Foo(2) + Foo(3), Foo(5));
    assert_eq!(Foo(2) | Foo(3), Foo(3));
    assert_eq!(Bar(2.0) - Bar(3.0), Bar(-1.0));
}
