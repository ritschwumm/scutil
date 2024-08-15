package scutil.bit

import minitest.*

object BitTest extends SimpleTestSuite {
	test("unsigned should work for positive bytes") {
		assertEquals(
			functions.unsignedByte(1.toByte),
			1.toShort
		)
	}

	test("unsigned should work for negative bytes") {
		assertEquals(
			functions.unsignedByte(-1.toByte),
			255.toShort
		)
	}

	test("unsigned should work for positive shorts") {
		assertEquals(
			functions.unsignedShort(1.toShort),
			1.toInt
		)
	}

	test("unsigned should work for negative shorts") {
		assertEquals(
			functions.unsignedShort(-1.toShort),
			65535.toInt
		)
	}

	test("unsigned should work for positive ints") {
		assertEquals(
			functions.unsignedInt(1.toInt),
			1.toLong
		)
	}

	test("unsigned should work for negative ints") {
		assertEquals(
			functions.unsignedInt(-1.toInt),
			0xffffffffL
		)
	}

	test("unsigned should work for positive longs") {
		assertEquals(
			functions.unsignedLong(1.toLong),
			BigInt(1)
		)
	}

	test("unsigned should work for negative longs") {
		assertEquals(
			functions.unsignedLong(-1.toLong),
			BigInt("18446744073709551615")
		)
	}

	//------------------------------------------------------------------------------

	test("swapEndian should work for small shorts") {
		assertEquals(
			functions.swapEndianShort(0x1234.toShort),
			0x3412.toShort
		)
	}

	test("swapEndian should work for big shorts") {
		assertEquals(
			functions.swapEndianShort(0x9876.toShort),
			0x7698.toShort
		)
	}

	test("swapEndian should work for small ints") {
		assertEquals(
			functions.swapEndianInt(0x12345678),
			0x78563412
		)
	}

	test("swapEndian should work for big ints") {
		assertEquals(
			functions.swapEndianInt(0x98765432),
			0x32547698
		)
	}

	test("swapEndian should work for small longs") {
		assertEquals(
			functions.swapEndianLong(0x1234567890123456L),
			0x5634129078563412L
		)
	}

	test("swapEndian should work for big longs") {
		assertEquals(
			functions.swapEndianLong(0x9876543210987654L),
			0x5476981032547698L
		)
	}
}
