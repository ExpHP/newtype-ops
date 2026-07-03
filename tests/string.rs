use newtype_ops::newtype_ops;

#[derive(PartialEq, Clone, Debug)]
pub struct MyString(String);
newtype_ops! { [MyString] {add} {:=} ^Self &{Self str} }

#[test]
fn test() {
    assert_eq!(
        MyString("Hello world".to_string()) + "!",
        MyString("Hello world!".to_string())
    )
}

// This DISABLED test documents a known victim of the rule that forbids OpAssign<&U> impls;
// were it not for that rule, this test would succeed.
#[cfg(nope)] // FIXME
#[test]
fn victim() {
    // use `String: for<'a> AddAssign<&'a str>`
    let mut s = MyString("Hello world".to_string());
    s += "!";
    assert_eq!(s, MyString("Hello world!".to_string()));
}
