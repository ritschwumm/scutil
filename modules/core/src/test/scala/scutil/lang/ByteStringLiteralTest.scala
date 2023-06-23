package scutil.lang

import minitest.*

import scutil.lang.implicits.*

object ByteStringLiteralTest extends SimpleTestSuite {
	test("ByteStringLiteral should decode 00") {
		val it	= bytes"00"
		typed[ByteString](it)
		assertEquals(it, ByteString.of(0x00.toByte))
	}

	test("ByteStringLiteral should decode 7f") {
		val it	= bytes"7f"
		typed[ByteString](it)
		assertEquals(it, ByteString.of(0x7f.toByte))
	}

	test("ByteStringLiteral should decode 80") {
		val it	= bytes"80"
		typed[ByteString](it)
		assertEquals(it, ByteString.of(0x80.toByte))
	}

	test("ByteStringLiteral should decode ff") {
		val it	= bytes"ff"
		typed[ByteString](it)
		assertEquals(it, ByteString.of(0xff.toByte))
	}

	//------------------------------------------------------------------------------

	test("ByteStringLiteral should decode 000000") {
		val it	= bytes"000000"
		typed[ByteString](it)
		assertEquals(it, ByteString.of(0x00.toByte, 0x00.toByte, 0x00.toByte))
	}

	test("ByteStringLiteral should decode 7fffff") {
		val it	= bytes"7fffff"
		typed[ByteString](it)
		assertEquals(it, ByteString.of(0x7f.toByte, 0xff.toByte, 0xff.toByte))
	}

	test("ByteStringLiteral should decode 800000") {
		val it	= bytes"800000"
		typed[ByteString](it)
		assertEquals(it, ByteString.of(0x80.toByte, 0x00.toByte, 0x00.toByte))
	}

	test("ByteStringLiteral should decode ffffff") {
		val it	= bytes"ffffff"
		typed[ByteString](it)
		assertEquals(it, ByteString.of(0xff.toByte, 0xff.toByte, 0xff.toByte))
	}

	//------------------------------------------------------------------------------

	test("ByteStringLiteral should handle upper case letters") {
		val it	= bytes"Ea"
		typed[ByteString](it)
		assertEquals(it, ByteString.of(0xea.toByte))
	}

	test("ByteStringLiteral should handle blanks in the middle") {
		val it	= bytes"12 34"
		typed[ByteString](it)
		assertEquals(it, ByteString.of(0x12.toByte, 0x34.toByte))
	}

	test("ByteStringLiteral should blanks at the start") {
		val it	= bytes" 12 34"
		typed[ByteString](it)
		assertEquals(it, ByteString.of(0x12.toByte, 0x34.toByte))
	}

	test("ByteStringLiteral should blanks at the end") {
		val it	= bytes"12 34 "
		typed[ByteString](it)
		assertEquals(it, ByteString.of(0x12.toByte, 0x34.toByte))
	}

	test("ByteStringLiteral should whitespace in general") {
		val it	=
			bytes"""
				12
				34
				56
			"""
		typed[ByteString](it)
		assertEquals(it, ByteString.of(0x12.toByte, 0x34.toByte, 0x56.toByte))
	}

	/*
	test("ByteStringLiteral prevent blanks in the wrong place") {
		assertDoesNotCompile {
			bytes"1 23 4"
		}
	}

	test("ByteStringLiteral should fail compiling an invalid string") {
		assertDoesNotCompile {
			bytes"xx" == 0
		}
	}
	*/
}
