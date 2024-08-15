package scutil.lang.tc

import minitest.*

import scutil.lang.*

object SummonTest extends SimpleTestSuite {
	test("can summon a Monoid for String") {
		summon[Monoid[String]]
		()
	}

	test("can summon a Semigroup for String") {
		summon[Semigroup[String]]
		()
	}

	//-----------------------------------------------------------------------------

	test("can summon a Monoid for ByteString") {
		summon[Monoid[ByteString]]
		()
	}

	test("can summon a Semigroup for ByteString") {
		summon[Semigroup[ByteString]]
		()
	}

	//-----------------------------------------------------------------------------

	test("can summon a TraversedMonad for Option") {
		summon[TraversedMonad[Option]]
		()
	}

	test("can summon a Traversed for Option") {
		summon[Traversed[Option]]
		()
	}

	test("can summon a Monad for Option") {
		summon[Monad[Option]]
		()
	}

	test("can summon an Applicative for Option") {
		summon[Applicative[Option]]
		()
	}

	test("can summon a Functor for Option") {
		summon[Functor[Option]]
		()
	}

	//-----------------------------------------------------------------------------

	test("can summon a TraversedMonad for Nes") {
		summon[TraversedMonad[Nes]]
		()
	}

	test("can summon a Traversed for Nes") {
		summon[Traversed[Nes]]
		()
	}

	test("can summon a Monad for Nes") {
		summon[Monad[Nes]]
		()
	}

	test("can summon an Applicative for Nes") {
		summon[Applicative[Nes]]
		()
	}

	test("can summon a Functor for Nes") {
		summon[Functor[Nes]]
		()
	}

	//-----------------------------------------------------------------------------

	test("can summon an Applicative for Validated[Vector[String],_]") {
		summon[Applicative[Validated[Vector[String],_]]]
		()
	}

	test("can summon a Functor for Validated[Vector[String],_]") {
		summon[Functor[Validated[Vector[String],_]]]
		()
	}
}
