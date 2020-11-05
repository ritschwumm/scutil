package scutil.codec

import org.specs2.mutable._

import scutil.lang._

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class Base64Test extends Specification {
	val	possible =
		(ByteString makeWithArray 256) { tmp =>
			for (i <- 0 until tmp.length) tmp(i) = i.toByte
		}

	"Base64" should {
		"handle roundtripping 0 bytes at all" in {
			val bytes	= ByteString.empty
			val round	= Base64 decodeByteString (Base64 encodeByteString bytes)
			round must beSome[ByteString]
		}
		"correctly roundtrip 0 bytes" in {
			val bytes	= ByteString()
			val round	= Base64 decodeByteString (Base64 encodeByteString bytes)
			round.get mustEqual bytes
		}

		"handle roundtripping 1 bytes at all" in {
			val bytes	= ByteString(0)
			val round	= Base64 decodeByteString (Base64 encodeByteString bytes)
			round must beSome[ByteString]
		}
		"correctly roundtrip 1 bytes" in {
			val bytes	= ByteString(0)
			val round	= Base64 decodeByteString (Base64 encodeByteString bytes)
			round.get mustEqual bytes
		}

		"handle roundtripping 2 bytes at all" in {
			val bytes	= ByteString(1,2)
			val round	= Base64 decodeByteString (Base64 encodeByteString bytes)
			round must beSome[ByteString]
		}
		"correctly roundtrip 2 bytes" in {
			val bytes	= ByteString(1,2)
			val round	= Base64 decodeByteString (Base64 encodeByteString bytes)
			round.get mustEqual bytes
		}

		"handle roundtripping 3 bytes at all" in {
			val bytes	= ByteString(3,4,5)
			val round	= Base64 decodeByteString (Base64 encodeByteString bytes)
			round must beSome[ByteString]
		}
		"correctly roundtrip 2 bytes" in {
			val bytes	= ByteString(3,4,5)
			val round	= Base64 decodeByteString (Base64 encodeByteString bytes)
			round.get mustEqual bytes
		}

		"handle roundtripping 4 bytes at all" in {
			val bytes	= ByteString(6,7,8,9)
			val round	= Base64 decodeByteString (Base64 encodeByteString bytes)
			round must beSome[ByteString]
		}
		"correctly roundtrip 2 bytes" in {
			val bytes	= ByteString(6,7,8,9)
			val round	= Base64 decodeByteString (Base64 encodeByteString bytes)
			round.get mustEqual bytes
		}

		"handle roundtripping every possible byte at all" in {
			val bytes	= possible
			val round	= Base64 decodeByteString (Base64 encodeByteString bytes)
			round must beSome[ByteString]
		}
		"correctly roundtrip every possible byte bytes" in {
			val bytes	= possible
			val round	= Base64 decodeByteString (Base64 encodeByteString bytes)
			round.get mustEqual bytes
		}
	}
}
