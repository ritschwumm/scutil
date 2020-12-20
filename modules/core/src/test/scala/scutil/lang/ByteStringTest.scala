package scutil.lang

import minitest._

import scutil.lang.tc._

object ByteStringTest extends SimpleTestSuite {
	test("ByteString should have a working monoid") {
		val xs	= Vector(ByteString fromUtf8String "a", ByteString fromUtf8String "b")
		val out	= (xs foldLeft Monoid[ByteString].empty)(Monoid[ByteString].combine)

		assertEquals(
			out,
			(ByteString fromUtf8String "ab")
		)
	}

	//------------------------------------------------------------------------------

	test("ByteString should split inside") {
		assertEquals(
			ByteString.of(1,2,3,4,5,6) splitAt 2,
			Some((ByteString.of(1,2), ByteString.of(3,4,5,6)))
		)
	}

	test("ByteString should split at the start") {
		assertEquals(
			ByteString.of(1,2,3,4,5,6) splitAt 0,
			Some((ByteString.empty, ByteString.of(1,2,3,4,5,6)))
		)
	}

	test("ByteString should split at the end") {
		assertEquals(
			ByteString.of(1,2,3,4,5,6) splitAt 6,
			Some((ByteString.of(1,2,3,4,5,6), ByteString.empty))
		)
	}

	test("ByteString should not split before start") {
		assertEquals(
			ByteString.of(1,2,3,4,5,6) splitAt 0-1,
			None
		)
	}

	test("ByteString should not split after end") {
		assertEquals(
			ByteString.of(1,2,3,4,5,6) splitAt 6+1,
			None
		)
	}

	//------------------------------------------------------------------------------

	test("ByteString should roundtrip big endian long") {
		val b	= 4607182418800017408L
		val bb	= ByteString.fromBigEndianLong(b).toBigEndianLong.getOrElse(sys error "oops")
		assertEquals(
			bb,
			b
		)
	}
}
