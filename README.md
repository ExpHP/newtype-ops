newtype-ops
===========

A operator-deriving macro for newtypes that is wartier than [`newtype_derive`](https://github.com/DanielKeep/rust-custom-derive),
and therefore better.<sup>[[_Citation needed_](https://en.wikipedia.org/wiki/Wikipedia:Citation_needed)]</sup>

```rust
pub struct Foo(i32);

newtype_ops! { [Foo] integer {:=} {^&}Self {^&}{Self i32} }

// alternatively
newtype_ops! { [Foo] {add sub mul div rem neg not bitand bitor bitxor} {:=} {^&}Self {^&}{Self i32} }
```

Installation
------------

So for once I have finally decided to actually publish a crate this time.

**Cargo.toml**

```toml
[dependencies]
newtype-ops = "0.1"
```

Documentation
-------------

Funny thing, actually, pretty much the sole reason I published the crate
was so that I could link to docs.rs.

[See the documentation for `newtype_ops!`](https://docs.rs/newtype-ops/0.1.3/newtype_ops/macro.newtype_ops.html)

License
-------

WTFPL 2.0
