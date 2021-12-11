package scutil.regex

import minitest.*

import scutil.regex.implicits.*

object RegexTest extends SimpleTestSuite {
	test("regex macro should just work") {
		assertEquals(
			re".*".pattern.pattern,
			".*".r.pattern.pattern
		)
	}
}
