package scutil.math

import org.specs2.mutable._

import scutil.{ math => self }

import scutil.math.implicits._

class MathTest extends Specification {
	"Ordering" should {
		"be contravariant" in {
			trait Numb
			trait Inte extends Numb
			
			val i:Inte	= null
			val n:Numb	= i
			
			val no:Ordering[Numb]	= null
			val io:Ordering[Inte]	= no.vary[Inte]
			
			success
		}
	}
	
	"Unsigned" should {
		"work for positive bytes" in {
			(self unsignedByte 1.toByte) == 1.toShort
		}
		"work for negative bytes" in {
			(self unsignedByte -1.toByte) == 255.toShort
		}
		"work for positive shorts" in {
			(self unsignedShort 1.toShort) == 1.toInt
		}
		"work for negative shorts" in {
			(self unsignedShort -1.toShort) == 65535.toInt
		}
		"work for positive ints" in {
			(self unsignedInt 1.toInt) == 1.toLong
		}
		"work for negative ints" in {
			(self unsignedInt -1.toInt) == 0xffffffffL
		}
		"work for positive longs" in {
			(self unsignedLong 1.toLong) == BigInt(1)
		}
		"work for negative longs" in {
			(self unsignedLong -1.toLong) == BigInt("18446744073709551615")
		}
	}
	
	"swapEndian" should {
		"work for small shorts" in {
			(self swapEndianShort 0x1234.toShort)	== 0x3412.toShort
		}
		"work for big shorts" in {
			(self swapEndianShort 0x9876.toShort)	== 0x7698.toShort
		}
		"work for small ints" in {
			(self swapEndianInt 0x12345678)	== 0x78563412
		}
		"work for big ints" in {
			(self swapEndianInt 0x98765432)	== 0x32547698
		}
		"work for small longs" in {
			(self swapEndianLong 0x1234567890123456L)	== 0x5634129078563412L
		}
		"work for big longs" in {
			(self swapEndianLong 0x9876543210987654L)	== 0x5476981032547698L
		}
	}
}
