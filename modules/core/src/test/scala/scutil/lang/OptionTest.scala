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
}
