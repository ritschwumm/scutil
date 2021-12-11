package scutil.text

import minitest.*

import scutil.text.extension.StringContextExtensions.*

object TextTest extends SimpleTestSuite {
	test("stripMarginOnly should pass through blank input") {
		assertEquals(
			Text stripMarginOnly "",
			""
		)
	}

	test("stripMarginOnly should ignore non-matching lines") {
		assertEquals(
			Text stripMarginOnly "test\n|test\ntest",
			"test"
		)
	}

	test("stripMarginOnly should ignore blank lines") {
		assertEquals(
			Text stripMarginOnly "\n|test\n\n|foo\n\n\n|bar",
			"test\nfoo\nbar"
		)
	}

	test("stripMarginOnly should ignore the last linefeed") {
		assertEquals(
			Text stripMarginOnly "|test\n",
			"test"
		)
	}

	test("stripMarginOnly should properly strip without whitespace") {
		assertEquals(
			Text stripMarginOnly "|test",
			"test"
		)
	}

	test("stripMarginOnly should properly strip with blanks whitespace") {
		assertEquals(
			Text stripMarginOnly "  |test",
			"test"
		)
	}

	test("stripMarginOnly should properly strip with tabs whitespace") {
		assertEquals(
			Text stripMarginOnly "\t\t|test",
			"test"
		)
	}

	test("stripMarginOnly should properly strip with mixed whitespace") {
		assertEquals(
			Text stripMarginOnly " \t \t |test",
			"test"
		)
	}

	test("stripMarginOnly should work with multiple lines") {
		assertEquals(
			Text stripMarginOnly "  |test\n foo\n| bar",
			"test\n bar"
		)
	}

	//------------------------------------------------------------------------------

	test("strip interpolator just work") {
		assertEquals(
			strip"""
			|one
			| two
			""",
			"one\n two"
		)
	}

	test("table helper should format correctly") {
		assertEquals(
			Text table Vector(Vector("a","bb","ccc"),Vector("ddd","cccc","e")),
			Vector(
				"┌───┬────┬───┐",
				"│a  │bb  │ccc│",
				"├───┼────┼───┤",
				"│ddd│cccc│e  │",
				"└───┴────┴───┘"
			)
		)
	}
}
