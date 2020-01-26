package scutil.lang

import org.specs2.mutable._

import scutil.lang.tc._

class ByteStringTest extends Specification {
	"ByteString" should {
		"have a working monoid" in {
			val xs	= Vector(ByteString fromUtf8String "a", ByteString fromUtf8String "b")
			val out	= (xs foldLeft Monoid[ByteString].empty)(Monoid[ByteString].concat)

			out mustEqual (ByteString fromUtf8String "ab")
		}

		"split inside" in {
			ByteString.of(1,2,3,4,5,6) splitAt 2 mustEqual Some((ByteString.of(1,2), ByteString.of(3,4,5,6)))
		}

		"split at the start" in {
			ByteString.of(1,2,3,4,5,6) splitAt 0 mustEqual Some((ByteString.empty, ByteString.of(1,2,3,4,5,6)))
		}

		"split at the end" in {
			ByteString.of(1,2,3,4,5,6) splitAt 6 mustEqual Some((ByteString.of(1,2,3,4,5,6), ByteString.empty))
		}

		"not split before start" in {
			ByteString.of(1,2,3,4,5,6) splitAt 0-1 mustEqual None
		}

		"not split after end" in {
			ByteString.of(1,2,3,4,5,6) splitAt 6+1 mustEqual None
		}

		"roundtrip big endian long" in {
			val b	= 4607182418800017408L
			val bb	= ByteString.fromBigEndianLong(b).toBigEndianLong.getOrElse(sys error "oops")
			bb mustEqual b
		}
	}
}
