package scutil.color

import minitest.*

import scutil.color.implicits.*

object HexColorLiteralsTest extends SimpleTestSuite {
	test("HexColorLiteralsTest should decode a valid rgb color black") {
		assertEquals(
			rgb"000000",
			RGB(0,0,0)
		)
	}

	test("HexColorLiterals should decode a valid rgb color white") {
		assertEquals(
			rgb"ffffff",
			RGB(1,1,1)
		)
	}

	test("HexColorLiterals should decode a valid rgba color transparent black") {
		assertEquals(
			rgba"00000000",
			RGBA(RGB(0,0,0), Alpha(0))
		)
	}

	test("HexColorLiterals should decode a valid rgba color opaque white") {
		assertEquals(
			rgba"ffffffff",
			RGBA(RGB(1,1,1), Alpha(1))
		)
	}

	/*
	test("HexColorMacros should fail compiling an invalid color") {
		assertDoesNotCompile {
			rgb"xxxxxx"
		}
	}
	*/
}
