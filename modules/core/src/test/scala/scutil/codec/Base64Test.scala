package scutil.codec

import minitest.*

import scutil.lang.*

object Base64Test extends SimpleTestSuite {
	test("Base64 should handle roundtripping 0 bytes at all") {
		val bytes	= ByteString.empty
		val round	= Base64.decodeByteString(Base64.encodeByteString(bytes))
		assertEquals(round.isDefined, true)
	}
	test("Base64 should correctly roundtrip 0 bytes") {
		val bytes	= ByteString()
		val round	= Base64.decodeByteString(Base64.encodeByteString(bytes))
		assertEquals(
			round,
			Some(bytes)
		)
	}

	test("Base64 should handle roundtripping 1 bytes at all") {
		val bytes	= ByteString(0)
		val round	= Base64.decodeByteString(Base64.encodeByteString(bytes))
		assertEquals(round.isDefined, true)
	}
	test("Base64 should correctly roundtrip 1 bytes") {
		val bytes	= ByteString(0)
		val round	= Base64.decodeByteString(Base64.encodeByteString(bytes))
		assertEquals(
			round,
			Some(bytes)
		)
	}

	test("Base64 should handle roundtripping 2 bytes at all") {
		val bytes	= ByteString(1,2)
		val round	= Base64.decodeByteString(Base64.encodeByteString(bytes))
		assertEquals(round.isDefined, true)
	}
	test("Base64 should correctly roundtrip 2 bytes") {
		val bytes	= ByteString(1,2)
		val round	= Base64.decodeByteString(Base64.encodeByteString(bytes))
		assertEquals(
			round,
			Some(bytes)
		)
	}

	test("Base64 should handle roundtripping 3 bytes at all") {
		val bytes	= ByteString(3,4,5)
		val round	= Base64.decodeByteString(Base64.encodeByteString(bytes))
		assertEquals(round.isDefined, true)
	}
	test("Base64 should correctly roundtrip 2 bytes") {
		val bytes	= ByteString(3,4,5)
		val round	= Base64.decodeByteString(Base64.encodeByteString(bytes))
		assertEquals(
			round,
			Some(bytes)
		)
	}

	test("Base64 should handle roundtripping 4 bytes at all") {
		val bytes	= ByteString(6,7,8,9)
		val round	= Base64.decodeByteString(Base64.encodeByteString(bytes))
		assertEquals(round.isDefined, true)
	}
	test("Base64 should correctly roundtrip 2 bytes") {
		val bytes	= ByteString(6,7,8,9)
		val round	= Base64.decodeByteString(Base64.encodeByteString(bytes))
		assertEquals(
			round,
			Some(bytes)
		)
	}

	test("Base64 should handle roundtripping every possible byte at all") {
		val bytes	= possible
		val round	= Base64.decodeByteString(Base64.encodeByteString(bytes))
		assertEquals(round.isDefined, true)
	}
	test("Base64 should correctly roundtrip every possible byte bytes") {
		val bytes	= possible
		val round	= Base64.decodeByteString(Base64.encodeByteString(bytes))
		assertEquals(
			round,
			Some(bytes)
		)
	}

	//------------------------------------------------------------------------------

	private val	possible =
		ByteString.makeWithArray(256) { tmp =>
			for (i <- 0 until tmp.length) tmp(i) = i.toByte
		}
}
