package scutil.color

import minitest._

import scutil.color.implicits._

object HexColorMacrosTest extends SimpleTestSuite {
	test("HexColorMacros should decode a valid color") {
		assertEquals(
			rgb"000000",
			RGB(0,0,0)
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
