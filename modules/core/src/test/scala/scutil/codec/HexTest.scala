package scutil.codec

import minitest.*

import scutil.lang.*

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
object HexTest extends SimpleTestSuite {
	test("Hex should decode an empty string") {
		assertEquals(
			Hex decodeByteString "",
			Some(ByteString.empty)
		)
	}

	test("Hex should fail with uneven number of chars (1)") {
		assertEquals(
			Hex decodeByteString "0",
			None
		)
	}

	test("Hex should fail with uneven number of chars (5)") {
		assertEquals(
			Hex decodeByteString "00000",
			None
		)
	}

	test("Hex should fail with unexpected characters") {
		assertEquals(
			Hex decodeByteString "xx",
			None
		)
	}

	test("Hex should decode a single byte") {
		assertEquals(
			Hex decodeByteString "7f",
			Some(ByteString(127.toByte))
		)
	}

	test("Hex should decode a single byte") {
		assertEquals(
			Hex decodeByteString "f6",
			Some(ByteString(246.toByte))
		)
	}

	test("Hex should decode multiple bytes (3)") {
		assertEquals(
			Hex decodeByteString "10abf6",
			Some(ByteString(16.toByte, 171.toByte, 246.toByte))
		)
	}

	test("Hex should decode uppercase letters") {
		assertEquals(
			Hex decodeByteString "FF",
			Some(ByteString(255.toByte))
		)
	}
}
