package scutil.regex

import minitest._

import scutil.regex.implicits._

object RegexTest extends SimpleTestSuite {
	test("regex macro should just work") {
		assertEquals(
			re".*".pattern.pattern,
			".*".r.pattern.pattern
		)
	}
}
