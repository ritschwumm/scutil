package scutil.lang

import org.specs2.mutable._

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
class HexTest extends Specification {
	"Hex" should {
		"decode an empty string" in {
			(Hex decodeByteArray "").get must beEqualTo(Array.empty[Byte])
		}
		"fail with uneven number of chars (1)" in {
			(Hex decodeByteArray "0") must beNone
		}
		"fail with uneven number of chars (5)" in {
			(Hex decodeByteArray "00000") must beNone
		}
		"fail with unexpected characters" in {
			(Hex decodeByteArray "xx") must beNone
		}
		"decode a single byte" in {
			(Hex decodeByteArray "7f").get must beEqualTo(Array[Byte](127.toByte))
		}
		"decode a single byte" in {
			(Hex decodeByteArray "f6").get must beEqualTo(Array[Byte](246.toByte))
		}
		"decode multiple bytes (3)" in {
			(Hex decodeByteArray "10abf6").get must beEqualTo(Array[Byte](16.toByte, 171.toByte, 246.toByte))
		}
		"decode uppercase letters" in {
			(Hex decodeByteArray "FF").get must beEqualTo(Array[Byte](255.toByte))
		}
	}
}
