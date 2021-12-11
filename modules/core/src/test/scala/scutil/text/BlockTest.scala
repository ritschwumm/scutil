package scutil.text

import minitest.*

import scutil.text.literals.*

object BlockTest extends SimpleTestSuite {
	test("cooked Block interpolator should apply escape codes") {
		assertEquals(
			tb"""test\ntest""",
			"test\ntest"
		)
	}

	test("raw Block interpolator should leave escape codes alone") {
		assertEquals(
			rtb"""test\ntest""",
			"test\\ntest"
		)
	}

	//------------------------------------------------------------------------------

	test("Block interpolator should generate 0") {
		assertEquals(
			tb"""foo""",
			"foo"
		)
	}

	test("Block interpolator should generate 1") {
		assertEquals(
			tb"""
			foo
			""",
			"foo"
		)
	}

	test("Block interpolator should generate 2") {
		assertEquals(
			tb"""
			foo
			bar
			""",
			"foo\nbar"
		)
	}

	test("Block interpolator should generate 3") {
		assertEquals(
			tb"""
			${"foo"}
			bar
			""",
			"foo\nbar"
		)
	}

	test("Block interpolator should generate 4") {
		assertEquals(
			tb"""
			foo
			${"bar"}
			""",
			"foo\nbar"
		)
	}

	test("Block interpolator should generate 5") {
		assertEquals(
			tb"""
			foo
			${"bar"}
			quux
			""",
			"foo\nbar\nquux"
		)
	}

	test("Block interpolator should generate 6") {
		assertEquals(
			tb"""
			${"foo"}
			bar
			${"quux"}
			""",
			"foo\nbar\nquux"
		)
	}

	test("Block interpolator should generate 7") {
		assertEquals(
			tb"""
				foo
			bar
			""",
			"\tfoo\nbar"
		)
	}

	test("Block interpolator should generate 8") {
		assertEquals(
			tb"""
			foo
				bar
			""",
			"foo\n\tbar"
		)
	}

	test("Block interpolator should generate 9") {
		assertEquals(
			tb"""
			foo
				${"bar"}
			""",
			"foo\n\tbar"
		)
	}

	test("Block interpolator should generate 10") {
		assertEquals(
			tb"""
				${"foo"}
			bar
			""",
			"\tfoo\nbar"
		)
	}

	test("Block interpolator should generate 11") {
		assertEquals(
			tb"""
			foo
			${"bar\nquux\nwibble"}
			xyzzy
			""",
			"foo\nbar\nquux\nwibble\nxyzzy"
		)
	}

	test("Block interpolator should generate 12") {
		assertEquals(
			tb"""
			foo
				${"bar\nquux\nwibble"}
			xyzzy
			""",
			"foo\n\tbar\n\tquux\n\twibble\nxyzzy"
		)
	}

	test("Block interpolator should generate 13") {
		assertEquals(
			tb"""
			${"bar"} ${"quux"}
			""",
			"bar quux"
		)
	}

	test("Block interpolator should generate 14") {
		assertEquals(
			tb"""
			foo
				${"bar"} ${"quux"}
			wibble
			""",
			"foo\n\tbar quux\nwibble"
		)
	}

	test("Block interpolator should generate 15") {
		assertEquals(
			tb"""
			${"foo\nbar"} ${"quux\nwibble"}
			""",
			"foo\nbar quux\nwibble"
		)
	}

	test("Block interpolator should generate 16") {
		assertEquals(
			tb"""
			a
				${"foo\nbar"} ${"quux\nwibble"}
			b
			""",
			"a\n\tfoo\n\tbar quux\n\twibble\nb"
		)
	}

	test("Block interpolator should generate 17") {
		assertEquals(
			tb"""
			""",
			""
		)
	}

	test("Block interpolator should generate 18") {
		assertEquals(
			tb"""
				foo
			""",
			"foo"
		)
	}

	test("Block interpolator should generate 19") {
		assertEquals(
			tb"""
				${"foo\nbar"}
			""",
			"foo\nbar"
		)
	}

	test("Block interpolator should generate 20") {
		assertEquals(
			tb"""
				${"foo\n\nbar"}
			""",
			"foo\n\nbar"
		)
	}

	test("Block interpolator should generate 20") {
		assertEquals(
			tb"""
			a
				${"foo\n\nbar"}
			b
			""",
			"a\n\tfoo\n\t\n\tbar\nb"
		)
	}

	test("Block interpolator should generate 21") {
		assertEquals(
			tb"""
				${""}
				${"a"}
			""",
			"\na"
		)
	}

	test("Block interpolator should generate 22") {
		assertEquals(
			tb"""
				${"a"}
				${""}
			""",
			"a\n"
		)
	}

	test("Block interpolator should generate 23") {
		assertEquals(
			tb"""
				${""}
				${""}
			""",
			"\n"
		)
	}
}
