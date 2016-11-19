package scutil.bit

import org.specs2.mutable._

import scutil.{ bit => self }

class BitTest extends Specification {
	"Unsigned" should {
		"work for positive bytes" in {
			(self unsignedByte 1.toByte) mustEqual 1.toShort
		}
		"work for negative bytes" in {
			(self unsignedByte -1.toByte) mustEqual 255.toShort
		}
		"work for positive shorts" in {
			(self unsignedShort 1.toShort) mustEqual 1.toInt
		}
		"work for negative shorts" in {
			(self unsignedShort -1.toShort) mustEqual 65535.toInt
		}
		"work for positive ints" in {
			(self unsignedInt 1.toInt) mustEqual 1.toLong
		}
		"work for negative ints" in {
			(self unsignedInt -1.toInt) mustEqual 0xffffffffL
		}
		"work for positive longs" in {
			(self unsignedLong 1.toLong) mustEqual BigInt(1)
		}
		"work for negative longs" in {
			(self unsignedLong -1.toLong) mustEqual BigInt("18446744073709551615")
		}
	}
	
	"swapEndian" should {
		"work for small shorts" in {
			(self swapEndianShort 0x1234.toShort)	mustEqual 0x3412.toShort
		}
		"work for big shorts" in {
			(self swapEndianShort 0x9876.toShort)	mustEqual 0x7698.toShort
		}
		"work for small ints" in {
			(self swapEndianInt 0x12345678)	mustEqual 0x78563412
		}
		"work for big ints" in {
			(self swapEndianInt 0x98765432)	mustEqual 0x32547698
		}
		"work for small longs" in {
			(self swapEndianLong 0x1234567890123456L)	mustEqual 0x5634129078563412L
		}
		"work for big longs" in {
			(self swapEndianLong 0x9876543210987654L)	mustEqual 0x5476981032547698L
		}
	}
}
