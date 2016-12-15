// Copyright 2016 Michael Lamparski
//
// Licensed under WTFPL-2.0 (see file COPYING).

//! # newtype-ops
//!
//! An attempt to provide a macro for mass-forwarding impls for newtypes.
//!
//! Is currently "usable" for wrappers around primitives and some awkward special cases.

#![cfg_attr(test, feature(trace_macros))]
#[cfg(test)] trace_macros!(true);

/// A macro for mass-forwarding operator impls on newtypes.
///
/// ```rust
/// #[macro_use]
/// extern crate newtype_ops;
///
/// // derive everything under the sun
/// pub struct Foo(pub i32);
/// newtype_ops! { [Foo] integer {:=} {^&}Self {^&}{Self i32} }
///
/// // derive everything under the sun for floats
/// pub struct Bar(pub f32);
/// newtype_ops! { [Bar] arithmetic {:=} {^&}Self {^&}{Self f32} }
/// ```

/// These two impls are equivalent to this:
///
/// ```rust
/// # #[macro_use] extern crate newtype_ops;
/// # pub struct Foo(pub i32);
/// # pub struct Bar(pub f32);
/// newtype_ops! { [Foo] {add sub mul div rem neg
///                       not bitand bitor bitxor} {:=} {^&}Self {^&}{Self i32} }
/// newtype_ops! { [Bar] {add sub mul div rem neg} {:=} {^&}Self {^&}{Self f32} }
/// ```

/// Which are in turn equivalent to this;
/// every braced portion is expanded in a cartesian product of the tokens inside.
/// To give you a feel for what the various parts of the syntax stand for, the
/// impls are labeled in comments.
///
/// ```rust
/// # #[macro_use] extern crate newtype_ops;
/// # pub struct Foo(pub i32);
/// # pub struct Bar(pub f32);
/// newtype_ops! { [Bar] add : ^Self ^Self } // impl Add<Bar>  for Bar { ... }
/// newtype_ops! { [Bar] add : ^Self ^i32  } // impl Add<i32>  for Bar { ... }
/// newtype_ops! { [Bar] add : ^Self &Self } // impl Add<&Bar> for Bar { ... }
/// newtype_ops! { [Bar] add : ^Self &i32  } // impl Add<&i32> for Bar { ... }
/// newtype_ops! { [Bar] add : &Self ^Self } // impl Add<Bar>  for &Bar { ... }
/// newtype_ops! { [Bar] add : &Self ^i32  } // impl Add<i32>  for &Bar { ... }
/// newtype_ops! { [Bar] add : &Self &Self } // impl Add<&Bar> for &Bar { ... }
/// newtype_ops! { [Bar] add : &Self &i32  } // impl Add<&i32> for &Bar { ... }
/// newtype_ops! { [Bar] add = ^Self ^Self } // impl AddAssign<Bar> for Bar { ... }
/// newtype_ops! { [Bar] add = ^Self ^i32  } // impl AddAssign<i32> for Bar { ... }
/// newtype_ops! { [Bar] add = ^Self &Self } // Silently ignored [b]
/// newtype_ops! { [Bar] add = ^Self &i32  } // Silently ignored [b]
/// newtype_ops! { [Bar] add = &Self ^Self } // Silently ignored [a]
/// newtype_ops! { [Bar] add = &Self ^i32  } // Silently ignored [a]
/// newtype_ops! { [Bar] add = &Self &Self } // Silently ignored [a]
/// newtype_ops! { [Bar] add = &Self &i32  } // Silently ignored [a]
/// // ... Sub impls ...
/// // ... Mul impls ...
/// // ... Div impls ...
/// // ... Rem impls ...
/// newtype_ops! { [Bar] neg : ^Self ^Self } // impl Neg for Bar { ... }
/// newtype_ops! { [Bar] neg : ^Self ^i32  } // Silently ignored [c]
/// newtype_ops! { [Bar] neg : ^Self &Self } // Silently ignored [c]
/// newtype_ops! { [Bar] neg : ^Self &i32  } // Silently ignored [c]
/// newtype_ops! { [Bar] neg : &Self ^Self } // impl Neg for &Bar { ... }
/// newtype_ops! { [Bar] neg : &Self ^i32  } // Silently ignored [c]
/// newtype_ops! { [Bar] neg : &Self &Self } // Silently ignored [c]
/// newtype_ops! { [Bar] neg : &Self &i32  } // Silently ignored [c]
/// newtype_ops! { [Bar] neg = ^Self ^Self } // Silently ignored [a]
/// newtype_ops! { [Bar] neg = ^Self ^i32  } // Silently ignored [a]
/// newtype_ops! { [Bar] neg = ^Self &Self } // Silently ignored [a]
/// newtype_ops! { [Bar] neg = ^Self &i32  } // Silently ignored [a]
/// newtype_ops! { [Bar] neg = &Self ^Self } // Silently ignored [a]
/// newtype_ops! { [Bar] neg = &Self ^i32  } // Silently ignored [a]
/// newtype_ops! { [Bar] neg = &Self &Self } // Silently ignored [a]
/// newtype_ops! { [Bar] neg = &Self &i32  } // Silently ignored [a]
/// ```

/// **_Holy blazing swordfish,_** you should reply,
/// _why are so many impls silently ignored?_
///
/// Well, a couple of reasons.  They're labeled:
///
/// * `[a]`: **They're nonsense/impossible,** yet cartesian products generate them.
/// * `[b]`: **For not very good reasons.** <br />
///          `AddAssign<&Bar>` and `AddAssign<&i32>` are certainly possible.
///          However, the majority of newtype wrappers are likely around primitives,
///          and primitive types are mysteriously missing `XyzAssign` impls for borrowed args.
///          Simply put, I forbade these so that the invocation at the top of the page *works.*
/// * `[c]`: **Shortcomings in implementation.** <br />
///          Obviously `Neg` doesn't care about an argument type,
///          but gets paired with a whole bunch of them anyways
///          thanks to the cartesian product design.
///          The labeled invocations are thus ignored to avoid generating
///          multiple conflicting impls.

/// ## Other notes:
///
/// There are word equivalents for `:` (`normal`) and `=` (`assign`).
///
/// There are deliberately NOT operator shortcuts such as `+` for `add`,
/// because it is too easy to accidentally embed a comment delimiter (`+-*/%`).
///
/// The caret `^` before value types can be omitted.
/// Its raison d'être is to give you something to put in a cartesian product, as `{^&}`.
///
/// ```rust
/// # #[macro_use] extern crate newtype_ops;
/// # pub struct Foo(pub i32);
/// # pub struct Bar(pub f32);
/// newtype_ops! { [Bar] add : Self Self }
/// ```

/// You can omit the final type, in which case it is inferred to be `Self`.
/// This is for the sake of unary ops, but it currently also affects others...
///
/// ```rust
/// # #[macro_use] extern crate newtype_ops;
/// # pub struct Bar(pub f32);
/// newtype_ops! { [Bar] neg : Self } // Allowed
/// newtype_ops! { [Bar] add : Self } // Also allowed (but discouraged; may change)
/// ```

/// The implemented type is bracketed (a) to help parse it,
/// and (b) so that it can use a cartesian product,
/// which currently operates specifically on token trees.
/// This is valid:
///
/// ```rust
/// # #[macro_use] extern crate newtype_ops;
/// pub mod foo {
///     struct Foo(pub i32);
/// }
/// pub struct Bar(pub i32);
/// pub struct Generic<T>(T);
///
/// newtype_ops! { {[foo::Foo] [Bar] [Generic<i32>]} arithmetic {:=} Self Self }
/// ```

/// The output type can also be anything arbitrary like `foo::Foo` or `Generic<i32>`,
/// so long as `self.0` (`&self.0` for `&Self` receivers) implements `Add` for that type.
/// However, be careful with cartesian products;
/// `{Self i32}` only works because `Self` and `i32` are individual token trees.
/// `{foo::Foo Generic<i32>}` will fail miserably.
///
/// The `Index`, `Deref`, and `Fn` trait hierarchies are considered in-scope for future additions,
/// but hell if I know how to fit them into the existing interface.

#[macro_export]
macro_rules! newtype_ops {
	($($rest:tt)*) => { newtype_ops__!{ @product::next($($rest)*) -> () }};
}

/// implementation detail, go away
#[macro_export]
macro_rules! newtype_ops__ {

	//----------------------
	// @product:  Handles the behavior of {} products

	(@product::next({$($token:tt)+} $($rest:tt)*) -> $args:tt) => {
		newtype_ops__!{ @product::unpack({$($token)+} $($rest)*) -> $args }};
	(@product::next( $token:tt $($rest:tt)*) -> $args:tt) => {
		newtype_ops__!{ @product::single($token $($rest)*) -> $args }};
	(@product::next() -> $args:tt) => {
		newtype_ops__!{ @interpret$args }};

	// Add one token to the argument list.
	(@product::single($token:tt $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @product::next($($rest)*) -> ($($args)* $token) }};

	// Each direct product in the invocation incurs a fixed number of recursions
	//   as we replicate things to the correct depth.
	// Compress all unparsed text into a single tt so we can match it without a repetition.
	(@product::unpack({$($token:tt)*} $($rest:tt)*) -> $args:tt) => {
		newtype_ops__!{ @product::unpack_2({$($token)*} [$($rest)*]) -> $args }};
	// Replicate macro for each token:
	(@product::unpack_2({$($token:tt)*} $rest:tt) -> $args:tt) => {
		$( newtype_ops__!{ @product::unpack_3($token $rest) -> $args } )* };
	// Expand the unparsed arguments back to normal.
	// (using single instead of next avoids reparsing a nested {} as another direct product,
	//  as I am uncertain that such behavior would be useful)
	(@product::unpack_3($token:tt [$($rest:tt)*]) -> $args:tt) => {
		newtype_ops__!{ @product::single($token $($rest)*) -> $args }};

	//----------------------
	// @interpret:  Parses individual arguments, expands internally-defined groups.

	(@interpret($($rest:tt)*)) => { newtype_ops__!{ @interpret::type($($rest)*) -> () }};

	(@interpret::type([$($T:tt)*] $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::oper($($rest)*) -> ($($args)* {value_ty:[$($T)*]}) }};

	(@interpret::oper(arithmetic $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::oper(add $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(sub $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(mul $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(div $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(rem $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(neg $($rest)*) -> ($($args)*) }
	};

	// API NOTE:
	//  Purposefully not named 'all' because such a name would also imply support for Index,
	//  Deref, and the Fn traits if I ever get to adding them. (and an option which provides
	//  both e.g. Mul and Index will probably have no applicable types)
	(@interpret::oper(integer $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::oper(bitand $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(bitor  $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(bitxor $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(not $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(add $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(sub $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(mul $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(div $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(rem $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(neg $($rest)*) -> ($($args)*) }
	};

	(@interpret::oper(arith_ring $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::oper(add $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(sub $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(mul $($rest)*) -> ($($args)*) }
	};

	(@interpret::oper(bitwise $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::oper(bitand $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(bitor  $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(bitxor $($rest)*) -> ($($args)*) }
		newtype_ops__!{ @interpret::oper(not    $($rest)*) -> ($($args)*) }
	};

	// NOTE: The original plan was to allow +*-/%^&| here but it is too easy to
	//       accidentally put * and / together and end up making your life hell. :V
	//
	// Commas would be nice, e.g. {+,-,*,/,%,^,|,&} but it's tough to fit that into
	// @product() without exposing some cleverly-hidden warts and/or enabling unbounded
	// recursion.
	(@interpret::oper(add $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::mode($($rest)*) -> (
			$($args)* {kind:binary}
			{traits:[[::std::ops::Add][::std::ops::AddAssign]]}
			{methods:[[add][add_assign]]}
		)}};

	(@interpret::oper(sub $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::mode($($rest)*) -> (
			$($args)* {kind:binary}
			{traits:[[::std::ops::Sub][::std::ops::SubAssign]]}
			{methods:[[sub][sub_assign]]}
		)}};

	(@interpret::oper(mul $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::mode($($rest)*) -> (
			$($args)* {kind:binary}
			{traits:[[::std::ops::Mul][::std::ops::MulAssign]]}
			{methods:[[mul][mul_assign]]}
		)}};

	(@interpret::oper(div $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::mode($($rest)*) -> (
			$($args)* {kind:binary}
			{traits:[[::std::ops::Div][::std::ops::DivAssign]]}
			{methods:[[div][div_assign]]}
		)}};

	(@interpret::oper(rem $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::mode($($rest)*) -> (
			$($args)* {kind:binary}
			{traits:[[::std::ops::Rem][::std::ops::RemAssign]]}
			{methods:[[rem][rem_assign]]}
		)}};

	(@interpret::oper(neg $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::mode($($rest)*) -> (
			$($args)* {kind:unary}
			{traits:[[::std::ops::Neg]]}
			{methods:[[neg]]}
		)}};

	(@interpret::oper(bitand $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::mode($($rest)*) -> (
			$($args)* {kind:binary}
			{traits:[[::std::ops::BitAnd][::std::ops::BitAndAssign]]}
			{methods:[[bitand][bitand_assign]]}
		)}};

	(@interpret::oper(bitor $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::mode($($rest)*) -> (
			$($args)* {kind:binary}
			{traits:[[::std::ops::BitOr][::std::ops::BitOrAssign]]}
			{methods:[[bitor][bitor_assign]]}
		)}};

	(@interpret::oper(bitxor $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::mode($($rest)*) -> (
			$($args)* {kind:binary}
			{traits:[[::std::ops::BitXor][::std::ops::BitXorAssign]]}
			{methods:[[bitxor][bitxor_assign]]}
		)}};

	(@interpret::oper(not $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::mode($($rest)*) -> (
			$($args)* {kind:unary}
			{traits:[[::std::ops::Not]]}
			{methods:[[not]]}
		)}};

	// or 'pure', but that's a reserved keyword and some editors give it scare-highlighting
	(@interpret::mode(normal $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::self($($rest)*) -> ($($args)* {mode:normal}) }};
	(@interpret::mode(assign $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::self($($rest)*) -> ($($args)* {mode:assign}) }};
	(@interpret::mode(: $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::self($($rest)*) -> ($($args)* {mode:normal}) }};
	(@interpret::mode(= $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::self($($rest)*) -> ($($args)* {mode:assign}) }};

	(@interpret::self(&Self $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::other($($rest)*) -> ($($args)* {recv_form:[&x.0]}) }};
	(@interpret::self(^Self $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::other($($rest)*) -> ($($args)* {recv_form:[x.0]}) }};
	(@interpret::self(Self $($rest:tt)*) -> ($($args:tt)*)) => {
		newtype_ops__!{ @interpret::other($($rest)*) -> ($($args)* {recv_form:[x.0]}) }};

	// NOTE: here, we take advantage of the fact that the second type
	//       is the final argument in two ways:
	//  1. To allow it to be easily omitted for unary ops.
	//  2. To allow it to be an arbitrary type without requiring delimiters around it.
	//     (the first type is always Self, so no inconsistency is felt)
	// FIXME: This is a sucky thing to take advantage of because it
	//        means we can't do cartesian product.

	// operations between two newtypes
	(@interpret::other(&Self) -> ($($args:tt)*)) => {
		newtype_ops__!{ @postprocess($($args)* {arg:[#ref]} {arg_form:[&x.0]}) }};
	(@interpret::other(^Self) -> ($($args:tt)*)) => {
		newtype_ops__!{ @postprocess($($args)* {arg:[#value]} {arg_form:[x.0]}) }};
	(@interpret::other(Self) -> ($($args:tt)*)) => {
		newtype_ops__!{ @postprocess($($args)* {arg:[#value]} {arg_form:[x.0]}) }};
	(@interpret::other() -> ($($args:tt)*)) => {
		newtype_ops__!{ @postprocess($($args)* {arg:[#value]} {arg_form:[x.0]}) }};
	// operations between newtype and another type U for which (T or &T):Add<U>.
	(@interpret::other(^$($rest:tt)+) -> ($($args:tt)*)) => {
		newtype_ops__!{ @postprocess($($args)* {arg:[$($rest)*]} {arg_form:[x]}) }};
	(@interpret::other($($rest:tt)+) -> ($($args:tt)*)) => {
		newtype_ops__!{ @postprocess($($args)* {arg:[$($rest)*]} {arg_form:[x]}) }};

	//----------------------
	// @postprocess

	(@postprocess(
		$value_ty:tt $kind:tt $traits:tt $methods:tt $mode:tt $recv_form:tt $arg:tt $arg_form:tt
	)) => {
		newtype_ops__!{ @postprocess::blacklist(
			[$mode $kind $recv_form $arg_form $arg] // for @postprocess::blacklist
			[$arg $value_ty $recv_form]             // for @postprocess::true_types
			$traits $methods $recv_form $arg_form // initial list for @postprocess::almost_there
		) }
	};

	// HACK: some arguments render other arguments inert; for instance, an impl for 'neg'
	//       has no argument type.  Bad combos do result from the cartesian product syntax,
	//       and unfortunately we cannot just ignore the nonsensical arguments because then
	//       we wil end up producing multiple conflicting impls.
	// Therefore, for each case where an argument is rendered nonsensical, only one of its
	// possible values will be accepted.  Anything else is—with apologies—silently ignored.

	// Make 'unary' require 'normal'
	// At the same time, fold mode into kind for three possible values: unary, binary, assign
	(@postprocess::blacklist([{mode:assign} {kind:unary}  $($more:tt)*] $($rest:tt)*)) => {
		};
	(@postprocess::blacklist([{mode:assign} {kind:binary} $($more:tt)*] $($rest:tt)*)) => {
		newtype_ops__!{ @postprocess::blacklist([{kind:assign} $($more)*] $($rest)*) }};
	(@postprocess::blacklist([{mode:normal} {kind:unary} $($more:tt)*] $($rest:tt)*)) => {
		newtype_ops__!{ @postprocess::blacklist([{kind:unary} $($more)*] $($rest)*) }};
	(@postprocess::blacklist([{mode:normal} {kind:binary} $($more:tt)*] $($rest:tt)*)) => {
		newtype_ops__!{ @postprocess::blacklist([{kind:binary} $($more)*] $($rest)*) }};

	// Make assign require a '^Self' receiver  (form = [x.0])
	(@postprocess::blacklist([{kind:assign} {recv_form:[&x.0]} $arg_form:tt $arg:tt] $($rest:tt)*)) => { };
	// Make 'unary' require a '^Self' argument
	(@postprocess::blacklist([{kind:unary} $recv_form:tt {arg_form:[&x.0]} $arg:tt] $($rest:tt)*)) => { };
	(@postprocess::blacklist([{kind:unary} $recv_form:tt {arg_form:[x]}    $arg:tt] $($rest:tt)*)) => { };

	// FIXME: Awful hack:
	// We deliberately ignore TraitAssign impls where the argument is borrowed, to make mass impls
	//  involving primitive wrapped types easier. (the standard library primitives lack impls
	//  for AddAssign<&Self> and etc.)
	// This obviously has nasty consequences for newtype wrappers around non-primitives for which
	//  such impls potentially *could* be valid.
	(@postprocess::blacklist([{kind:assign} $recv_form:tt $arg_form:tt {arg:[#ref]}] $($rest:tt)*)) => { };
	(@postprocess::blacklist([{kind:assign} $recv_form:tt $arg_form:tt {arg:[&$($arg:tt)+]}] $($rest:tt)*)) => { };

	(@postprocess::blacklist([{kind:$kind:tt} $($dropped:tt)*] $($rest:tt)*)) => {
		newtype_ops__!{ @postprocess::true_types($($rest)* {kind: $kind}) }};

	// Replace the #value and #ref placeholders with Self-based types
	(@postprocess::true_types(
		[{arg:[#value]} {value_ty:[$($T:tt)*]} $recv_form:tt] $($rest:tt)*
	)) => { newtype_ops__!{ @postprocess::true_types::2(
		[{value_ty:[$($T)*]} $recv_form {arg:[$($T)*]}] $($rest)*
	)}};
	(@postprocess::true_types(
		[{arg:[#ref]} {value_ty:[$($T:tt)*]} $recv_form:tt] $($rest:tt)*
	)) => { newtype_ops__!{ @postprocess::true_types::2(
		[{value_ty:[$($T)*]} $recv_form {arg:[&$($T)*]}] $($rest)*
	)}};
	(@postprocess::true_types(
		[{arg:[$($arg:tt)*]} {value_ty:[$($T:tt)*]} $recv_form:tt] $($rest:tt)*
	)) => { newtype_ops__!{ @postprocess::true_types::2(
		[{value_ty:[$($T)*]} $recv_form {arg:[$($arg)*]}] $($rest)*
	)}};

	// Generate lifetimes.
	(@postprocess::true_types::2(
		[{value_ty:[$($T:tt)*]} {recv_form:[&$($recv_form:tt)*]} {arg:[&$($arg:tt)*]}] $($rest:tt)*
	)) => {
		newtype_ops__!{ @postprocess::almost_there(
			$($rest)* {tpars:[<'a,'b>]} {recv:[&'a $($T)*]} {arg:[&'b $($arg)*]} {out:[$($T)*]}
		)}
	};

	(@postprocess::true_types::2(
		[{value_ty:[$($T:tt)*]} {recv_form:[&$($recv_form:tt)*]} {arg:[$($arg:tt)*]}] $($rest:tt)*
	)) => {
		newtype_ops__!{ @postprocess::almost_there(
			$($rest)* {tpars:[<'a>]} {recv:[&'a $($T)*]} {arg:[$($arg)*]} {out:[$($T)*]}
		)}
	};

	(@postprocess::true_types::2(
		[{value_ty:[$($T:tt)*]} {recv_form:[$($recv_form:tt)*]} {arg:[&$($arg:tt)*]}] $($rest:tt)*
	)) => {
		newtype_ops__!{ @postprocess::almost_there(
			$($rest)* {tpars:[<'b>]} {recv:[$($T)*]} {arg:[&'b $($arg)*]} {out:[$($T)*]}
		)}
	};

	(@postprocess::true_types::2(
		[{value_ty:[$($T:tt)*]} {recv_form:[$($recv_form:tt)*]} {arg:[$($arg:tt)*]}] $($rest:tt)*
	)) => {
		newtype_ops__!{ @postprocess::almost_there(
			$($rest)* {tpars:[]} {recv:[$($T)*]} {arg:[$($arg)*]} {out:[$($T)*]}
		)}
	};

	// at this stage we explicitly match the labels in each argument to ensure everything
	//  in order (literally)
	(@postprocess::almost_there(
		{traits:$traits:tt} {methods:$methods:tt}
		{recv_form:$recv_form:tt} {arg_form:$arg_form:tt}
		{kind:$kind:tt}                                               // from blacklist
		{tpars:$tpars:tt} {recv:$Recv:tt} {arg:$Arg:tt} {out:$Out:tt} // from true_types
	)) => {
		newtype_ops__!{
			@impl::$kind
			traits:$traits methods:$methods
			tpars:$tpars recv:$Recv arg:$Arg out:$Out
			forms:[$recv_form $arg_form]
		}
	};

	(@impl::unary
		traits:[[$($Trait:tt)*]]
		methods:[[$meth:ident]]
		tpars:[$($tpars:tt)*] recv:[$Recv:ty] arg:$_Arg:tt out:[$Out:path]
		forms:[[$($form1:tt)*] $_form2:tt]
	) => {
		impl$($tpars)* $($Trait)* for $Recv {
			type Output = $Out;
			fn $meth(self) -> $Out {
				let this = self;
				$Out(
					newtype_ops__!(@helper::delegate [$($form1)*] [this] [0]).$meth()
				)
			}
		}
	};

	(@impl::binary
		traits:[[$($Trait:tt)*] $_TraitAssign:tt]
		methods:[[$meth:ident] $_meth_assign:tt]
		tpars:[$($tpars:tt)*] recv:[$Recv:ty] arg:[$Arg:ty] out:[$Out:path]
		forms:[[$($form1:tt)*][$($form2:tt)*]]
	) => {
		impl$($tpars)* $($Trait)*<$Arg> for $Recv {
			type Output = $Out;
			fn $meth(self, other: $Arg) -> $Out {
				let this = self;
				$Out(
					newtype_ops__!(@helper::delegate [$($form1)*]  [this] [0])
					.$meth(
					newtype_ops__!(@helper::delegate [$($form2)*] [other] [0])
					)
				)
			}
		}
	};

	(@impl::assign
		traits:[$_Trait:tt [$($TraitAssign:tt)*]]
		methods:[$_meth:tt [$meth_assign:ident]]
		tpars:[$($tpars:tt)*] recv:[$Recv:ty] arg:[$Arg:ty] out:$_Out:tt
		forms:[$_form1:tt [$($form2:tt)*]]
	) => {
		impl$($tpars)* $($TraitAssign)*<$Arg> for $Recv {
			fn $meth_assign(&mut self, other: $Arg) {
				let this = self;
				newtype_ops__!(@helper::delegate   [&mut x.0]  [this] [0])
				.$meth_assign(
				newtype_ops__!(@helper::delegate [$($form2)*] [other] [0])
				)
			}
		}
	};

	(@helper::delegate [&mut x.0] [$id:ident] [$fld:tt]) => { &mut $id.$fld };
	(@helper::delegate     [&x.0] [$id:ident] [$fld:tt]) => {     &$id.$fld };
	(@helper::delegate      [x.0] [$id:ident] [$fld:tt]) => {      $id.$fld };
	(@helper::delegate        [x] [$id:ident] [$fld:tt]) => {           $id };
}

#[cfg(test)]
mod tests {

	mod foo { #[derive(Eq,PartialEq,Copy,Clone,Debug)] pub struct Foo(pub i32); }
	pub struct Bar(pub i32);
	newtype_ops!{ {[foo::Foo][Bar]} integer {:=} {^&}Self {^&}Self }
	newtype_ops!{ {[foo::Foo][Bar]} integer {:=} {^&}Self {^&}i32 }

	#[derive(PartialEq,Clone,Debug)] // ensure impls don't implicitly require Copy
	pub struct Baz(pub f32);
	newtype_ops!{ [Baz] {add sub div neg} {:=} {^&}Self {^&}Self }
	newtype_ops!{ [Baz] {add sub div neg} {:=} {^&}Self {^&}f32 }

	#[derive(PartialEq,Clone,Debug)]
	pub struct MyString(String);
	newtype_ops!{ [MyString] {add} {:} ^Self &{Self str} }

	#[test] fn test_test_test_test() {
		use tests::foo::Foo;
		assert_eq!(Foo(5), Foo(2) + Foo(3));
		assert_eq!(Foo(5), Foo(2) + &Foo(3));
		assert_eq!(Foo(5), &Foo(2) + Foo(3));
		assert_eq!(Foo(5), &Foo(2) + &Foo(3));

		assert_eq!(Foo(4), Foo(8) / 2);
		assert_eq!(Foo(4), &Foo(8) / 2);
		assert_eq!(Foo(4), Foo(8) / &2);
		assert_eq!(Foo(4), &Foo(8) / &2);

		assert_eq!(Foo(-3), -Foo(3));
		assert_eq!(Foo(-3), -&Foo(3));

		assert_eq!(Baz(5.), Baz(2.) + Baz(3.));
		assert_eq!(Baz(5.), Baz(2.) + &Baz(3.));
		assert_eq!(Baz(5.), &Baz(2.) + Baz(3.));
		assert_eq!(Baz(5.), &Baz(2.) + &Baz(3.));

		assert_eq!(Baz(4.), Baz(8.) / 2.);
		assert_eq!(Baz(4.), &Baz(8.) / 2.);
		assert_eq!(Baz(4.), Baz(8.) / &2.);
		assert_eq!(Baz(4.), &Baz(8.) / &2.);

		assert_eq!(Baz(-3.), -Baz(3.));
		assert_eq!(Baz(-3.), -&Baz(3.));
	}
}
