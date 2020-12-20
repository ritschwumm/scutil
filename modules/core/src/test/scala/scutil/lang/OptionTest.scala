package scutil.lang

import minitest._

import scutil.core.implicits._

object OptionTest extends SimpleTestSuite {
	test("Option extension and ApplicativeExtension should not clash") {
		// both OptionImplicits and ApplicativeSyntax provide a tuple method
		assertEquals(
			Option(1) tuple Option(2),
			Option((1,2))
		)
	}

	test("Option.zip returns an Option") {
		// both OptionImplicits and ApplicativeSyntax provide a tuple method
		assertEquals(
			Option(1) zip Option(2),
			Option((1,2))
		)
	}

	test("Option.partition returns Options") {
		// both OptionImplicits and ApplicativeSyntax provide a tuple method
		assertEquals(
			Option(1) partition (_ == 1),
			(Some(1), None)
		)
	}

	test("Option.toVector does not go through iterable") {
		assertEquals(
			Option(1).toVector,
			Vector(1)
		)
	}

	test("Option.toSet does not go through iterable") {
		assertEquals(
			Option(1).toSet,
			Set(1)
		)
	}

	test("Option.toSeq does not go through iterable") {
		assertEquals(
			Option(1).toSeq,
			Seq(1)
		)
	}
}
