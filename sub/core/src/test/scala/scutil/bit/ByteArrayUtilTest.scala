package scutil.bit

import org.specs2.mutable._

class ByteArrayUtilTest extends Specification {
	"reversing array" should {
		"work" in {
			ByteArrayUtil reverse Array[Byte](1,2,3,4,5) mustEqual Array[Byte](5,4,3,2,1)
		}
	}

	"creating arrays" should {
		"work for little endian shorts" in {
			(ByteArrayUtil littleEndianShort 0x1234.toShort)		mustEqual Array[Byte](0x34, 0x12)
		}
		"work for little endian ints" in {
			(ByteArrayUtil littleEndianInt 0x12345678)				mustEqual Array[Byte](0x78, 0x56, 0x34, 0x12)
		}
		"work for little endian longs" in {
			(ByteArrayUtil littleEndianLong 0x0112233445566778L)	mustEqual Array[Byte](0x78, 0x67, 0x56, 0x45, 0x34, 0x23, 0x12, 0x01)
		}
		"work big endian for shorts" in {
			(ByteArrayUtil bigEndianShort 0x1234.toShort)		mustEqual Array[Byte](0x12, 0x34)
		}
		"work for big endian ints" in {
			(ByteArrayUtil bigEndianInt 0x12345678)				mustEqual Array[Byte](0x12, 0x34, 0x56, 0x78)
		}
		"work for big endian longs" in {
			(ByteArrayUtil bigEndianLong 0x0112233445566778L)	mustEqual Array[Byte](0x01, 0x12, 0x23, 0x34, 0x45, 0x56, 0x67, 0x78)
		}
	}
}
