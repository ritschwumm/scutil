package scutil.math

import org.specs2.mutable._

import scutil.{ math => Math }

class MathTest extends Specification {
	"Unsigned" should {
		"work for positive bytes" in {
			(Math unsigned 1.toByte) == 1.toShort
		}
		"work for negative bytes" in {
			(Math unsigned -1.toByte) == 255.toShort
		}
		"work for positive shorts" in {
			(Math unsigned 1.toShort) == 1.toInt
		}
		"work for negative shorts" in {
			(Math unsigned -1.toShort) == 65535.toInt
		}
		"work for positive ints" in {
			(Math unsigned 1.toInt) == 1.toLong
		}
		"work for negative ints" in {
			(Math unsigned -1.toInt) == 0xffffffffL
		}
		"work for positive longs" in {
			(Math unsigned 1.toLong) == BigInt(1)
		}
		"work for negative longs" in {
			(Math unsigned -1.toLong) == BigInt("18446744073709551615")
		}
	}
	
	"swapEndian" should {
		"work for small shorts" in {
			(Math swapEndian 0x1234.toShort)	== 0x3412.toShort
		}
		"work for big shorts" in {
			(Math swapEndian 0x9876.toShort)	== 0x7698.toShort
		}
		"work for small ints" in {
			(Math swapEndian 0x12345678)	== 0x78563412
		}
		"work for big ints" in {
			(Math swapEndian 0x98765432)	== 0x32547698
		}
		"work for small longs" in {
			(Math swapEndian 0x1234567890123456L)	== 0x5634129078563412L
		}
		"work for big longs" in {
			(Math swapEndian 0x9876543210987654L)	== 0x5476981032547698L
		}
	}
}
