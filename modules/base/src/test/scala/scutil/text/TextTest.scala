package scutil.text

import org.specs2.mutable._

import scutil.text.pimp.StringContextImplicits._

class TextTest extends Specification {
	"stripMarginOnly" should {
		"pass through blank input" in {
			Text stripMarginOnly "" mustEqual ""
		}
		"ignore non-matching lines" in {
			Text stripMarginOnly "test\n|test\ntest" mustEqual "test"
		}
		"ignore blank lines" in {
			Text stripMarginOnly "\n|test\n\n|foo\n\n\n|bar" mustEqual "test\nfoo\nbar"
		}
		"ignore the last linefeed" in {
			Text stripMarginOnly "|test\n" mustEqual "test"
		}
		"properly strip without whitespace" in {
			Text stripMarginOnly "|test" mustEqual "test"
		}
		"properly strip with blanks whitespace" in {
			Text stripMarginOnly "  |test" mustEqual "test"
		}
		"properly strip with tabs whitespace" in {
			Text stripMarginOnly "\t\t|test" mustEqual "test"
		}
		"properly strip with mixed whitespace" in {
			Text stripMarginOnly " \t \t |test" mustEqual "test"
		}
		"work with multiple lines" in {
			Text stripMarginOnly "  |test\n foo\n| bar" mustEqual "test\n bar"
		}
	}

	"strip interpolator" should {
		"just work" in {
			strip"""
			|one
			| two
			""" mustEqual "one\n two"
		}
	}

	"table helper" should {
		"format correctly" in {
			Text table Vector(Vector("a","bb","ccc"),Vector("ddd","cccc","e")) mustEqual
			Vector(
				"┌───┬────┬───┐",
				"│a  │bb  │ccc│",
				"├───┼────┼───┤",
				"│ddd│cccc│e  │",
				"└───┴────┴───┘"
			)
		}
	}
}
