package scutil.codec

import org.specs2.mutable._

import scutil.lang._

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class HexTest extends Specification {
	"Hex" should {
		"decode an empty string" in {
			(Hex decodeByteString "").get mustEqual ByteString.empty
		}
		"fail with uneven number of chars (1)" in {
			(Hex decodeByteString "0") must beNone
		}
		"fail with uneven number of chars (5)" in {
			(Hex decodeByteString "00000") must beNone
		}
		"fail with unexpected characters" in {
			(Hex decodeByteString "xx") must beNone
		}
		"decode a single byte" in {
			(Hex decodeByteString "7f").get mustEqual ByteString(127.toByte)
		}
		"decode a single byte" in {
			(Hex decodeByteString "f6").get mustEqual ByteString(246.toByte)
		}
		"decode multiple bytes (3)" in {
			(Hex decodeByteString "10abf6").get mustEqual ByteString(16.toByte, 171.toByte, 246.toByte)
		}
		"decode uppercase letters" in {
			(Hex decodeByteString "FF").get mustEqual ByteString(255.toByte)
		}
	}
}
