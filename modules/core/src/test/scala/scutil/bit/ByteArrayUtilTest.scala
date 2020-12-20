package scutil.bit

import minitest._

object ByteArrayUtilTest extends SimpleTestSuite {
	test("reversing array") {
		assertArrayEquals(
			ByteArrayUtil reverse Array[Byte](1,2,3,4,5),
			Array[Byte](5,4,3,2,1)
		)
	}

	//------------------------------------------------------------------------------

	test("creating arrays should work for little endian shorts") {
		assertArrayEquals(
			ByteArrayUtil fromLittleEndianShort 0x1234.toShort,
			Array[Byte](0x34, 0x12)
		)
	}

	test("creating arrays should work for little endian ints") {
		assertArrayEquals(
			ByteArrayUtil fromLittleEndianInt 0x12345678,
			Array[Byte](0x78, 0x56, 0x34, 0x12)
		)
	}

	test("creating arrays should work for little endian longs") {
		assertArrayEquals(
			ByteArrayUtil fromLittleEndianLong 0x0112233445566778L,
			Array[Byte](0x78, 0x67, 0x56, 0x45, 0x34, 0x23, 0x12, 0x01)
		)
	}

	test("creating arrays should work big endian for shorts") {
		assertArrayEquals(
			ByteArrayUtil fromBigEndianShort 0x1234.toShort,
			Array[Byte](0x12, 0x34)
		)
	}

	test("creating arrays should work for big endian ints") {
		assertArrayEquals(
			ByteArrayUtil fromBigEndianInt 0x12345678,
			Array[Byte](0x12, 0x34, 0x56, 0x78)
		)
	}

	test("creating arrays should work for big endian longs") {
		assertArrayEquals(
			ByteArrayUtil fromBigEndianLong 0x0112233445566778L,
			Array[Byte](0x01, 0x12, 0x23, 0x34, 0x45, 0x56, 0x67, 0x78)
		)
	}

	//------------------------------------------------------------------------------

	test("changing arrays should roundtrip a big endian long") {
		val b		= 4607182418800017408L

		val array	= Array.ofDim[Byte](8)
		ByteArrayUtil.putBigEndianLong(array, 0, b)
		val bb	= ByteArrayUtil.getBigEndianLong(array, 0)

		assertEquals(bb, b)
	}

	test("changing arrays should roundtrip a little endian long") {
		val b		= 4607182418800017408L

		val array	= Array.ofDim[Byte](8)
		ByteArrayUtil.putLittleEndianLong(array, 0, b)
		val bb	= ByteArrayUtil.getLittleEndianLong(array, 0)

		assertEquals(bb, b)
	}

	test("changing arrays should roundtrip a big endian int") {
		val b		= 4607182418800017408L.toInt

		val array	= Array.ofDim[Byte](4)
		ByteArrayUtil.putBigEndianInt(array, 0, b)
		val bb	= ByteArrayUtil.getBigEndianInt(array, 0)

		assertEquals(bb, b)
	}

	test("changing arrays should roundtrip a little endian int") {
		val b		= 4607182418800017408L.toInt

		val array	= Array.ofDim[Byte](4)
		ByteArrayUtil.putLittleEndianInt(array, 0, b)
		val bb	= ByteArrayUtil.getLittleEndianInt(array, 0)

		assertEquals(bb, b)
	}

	//------------------------------------------------------------------------------

	// TODO minitest have hints
	private def assertArrayEquals(a:Array[Byte], b:Array[Byte]):Unit	= {
		assertEquals(a.toSeq, b.toSeq)
	}
}
