newtype-ops
===========

[![License](https://img.shields.io/crates/l/newtype-ops.svg)](LICENSE)
[![Documentation](https://docs.rs/newtype-ops/badge.svg)](https://docs.rs/newtype-ops)
[![Crates.io Version](https://img.shields.io/crates/v/newtype-ops.svg)](https://crates.io/crates/newtype-ops)
[![Build Status](https://travis-ci.org/ExpHP/newtype-ops.svg?branch=master)](https://travis-ci.org/ExpHP/newtype-ops)

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

[See the documentation for `newtype_ops!`](https://docs.rs/newtype-ops/*/newtype_ops/macro.newtype_ops.html)

License
-------

WTFPL 2.0
