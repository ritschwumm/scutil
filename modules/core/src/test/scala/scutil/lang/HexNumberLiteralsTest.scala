package scutil.lang

import minitest.*

import scutil.lang.implicits.*

object HexNumberLiteralsTest extends SimpleTestSuite {
	test("HexNumberLiterals should decode 00") {
		val it	= byte"00"
		typed[Byte](it)
		assertEquals(it, 0.toByte)
	}

	test("HexNumberLiterals should decode 7f") {
		val it	= byte"7f"
		typed[Byte](it)
		assertEquals(it, 127.toByte)
	}

	test("HexNumberLiterals should decode 80") {
		val it	= byte"80"
		typed[Byte](it)
		assertEquals(it, 128.toByte)
	}

	test("HexNumberLiterals should decode ff") {
		val it	= byte"ff"
		typed[Byte](it)
		assertEquals(it, 255.toByte)
	}

	//------------------------------------------------------------------------------

	test("HexNumberLiterals should decode 0000") {
		val it	= short"0000"
		typed[Short](it)
		assertEquals(it, 0.toShort)
	}

	test("HexNumberLiterals should decode 7fff") {
		val it	= short"7fff"
		typed[Short](it)
		assertEquals(it, 32767.toShort)
	}

	test("HexNumberLiterals should decode 8000") {
		val it	= short"8000"
		typed[Short](it)
		assertEquals(it, 32768.toShort)
	}

	test("HexNumberLiterals should decode ffff") {
		val it	= short"ffff"
		typed[Short](it)
		assertEquals(it, 65535.toShort)
	}

	//------------------------------------------------------------------------------

	test("HexNumberLiterals should decode 00000000") {
		val it	= int"00000000"
		typed[Int](it)
		assertEquals(it, 0.toInt)
	}

	test("HexNumberLiterals should decode 7fffffff") {
		val it	= int"7fffffff"
		typed[Int](it)
		assertEquals(it, 2147483647.toInt)
	}

	test("HexNumberLiterals should decode 80000000") {
		val it	= int"80000000"
		typed[Int](it)
		assertEquals(it, -2147483648.toInt)
	}

	test("HexNumberLiterals should decode ffffffff") {
		val it	= int"ffffffff"
		typed[Int](it)
		assertEquals(it, -1.toInt)
	}

	//------------------------------------------------------------------------------

	test("HexNumberLiterals should decode 0000000000000000") {
		val it	= long"0000000000000000"
		typed[Long](it)
		assertEquals(it, 0L)
	}

	test("HexNumberLiterals should decode 7fffffffffffffff") {
		val it	= long"7fffffffffffffff"
		typed[Long](it)
		assertEquals(it, 9223372036854775807L)
	}

	test("HexNumberLiterals should decode 8000000000000000") {
		val it	= long"8000000000000000"
		typed[Long](it)
		assertEquals(it, -9223372036854775808L)
	}

	test("HexNumberLiterals should decode ffffffffffffffff") {
		val it	= long"ffffffffffffffff"
		typed[Long](it)
		assertEquals(it, -1L)
	}

	//------------------------------------------------------------------------------

	test("HexNumberLiterals should handle upper case letters") {
		val it	= byte"Ea"
		typed[Byte](it)
		assertEquals(it, 234.toByte)
	}

	/*
	test("HexNumberLiterals should fail compiling an invalid color") {
		assertDoesNotCompile {
			hex"xx" == 0
		}
	}
	*/
}

