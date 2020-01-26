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
			(ByteArrayUtil fromLittleEndianShort 0x1234.toShort)		mustEqual Array[Byte](0x34, 0x12)
		}
		"work for little endian ints" in {
			(ByteArrayUtil fromLittleEndianInt 0x12345678)				mustEqual Array[Byte](0x78, 0x56, 0x34, 0x12)
		}
		"work for little endian longs" in {
			(ByteArrayUtil fromLittleEndianLong 0x0112233445566778L)	mustEqual Array[Byte](0x78, 0x67, 0x56, 0x45, 0x34, 0x23, 0x12, 0x01)
		}
		"work big endian for shorts" in {
			(ByteArrayUtil fromBigEndianShort 0x1234.toShort)			mustEqual Array[Byte](0x12, 0x34)
		}
		"work for big endian ints" in {
			(ByteArrayUtil fromBigEndianInt 0x12345678)					mustEqual Array[Byte](0x12, 0x34, 0x56, 0x78)
		}
		"work for big endian longs" in {
			(ByteArrayUtil fromBigEndianLong 0x0112233445566778L)		mustEqual Array[Byte](0x01, 0x12, 0x23, 0x34, 0x45, 0x56, 0x67, 0x78)
		}
	}

	"changing arrays" should {
		"roundtrip a big endian long" in {
			val b		= 4607182418800017408L

			val array	= Array.ofDim[Byte](8)
			ByteArrayUtil putBigEndianLong	(array, 0, b)
			val bb	= ByteArrayUtil getBigEndianLong (array, 0)

			bb mustEqual b
		}

		"roundtrip a little endian long" in {
			val b		= 4607182418800017408L

			val array	= Array.ofDim[Byte](8)
			ByteArrayUtil putLittleEndianLong	(array, 0, b)
			val bb	= ByteArrayUtil getLittleEndianLong (array, 0)

			bb mustEqual b
		}

		"roundtrip a big endian int" in {
			val b		= 4607182418800017408L.toInt

			val array	= Array.ofDim[Byte](4)
			ByteArrayUtil putBigEndianInt	(array, 0, b)
			val bb	= ByteArrayUtil getBigEndianInt (array, 0)

			bb mustEqual b
		}

		"roundtrip a little endian int" in {
			val b		= 4607182418800017408L.toInt

			val array	= Array.ofDim[Byte](4)
			ByteArrayUtil putLittleEndianInt	(array, 0, b)
			val bb	= ByteArrayUtil getLittleEndianInt (array, 0)

			bb mustEqual b
		}
	}
}
