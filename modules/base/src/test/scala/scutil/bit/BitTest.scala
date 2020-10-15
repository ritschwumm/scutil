package scutil.bit

import java.lang.{
	//Math	=> JMath,
	Float	=> JFloat,
	Double	=> JDouble
}


import org.specs2.mutable._

class BitTest extends Specification {
	"Unsigned" should {
		"work for positive bytes" in {
			(functions unsignedByte 1.toByte) mustEqual 1.toShort
		}
		"work for negative bytes" in {
			(functions unsignedByte -1.toByte) mustEqual 255.toShort
		}
		"work for positive shorts" in {
			(functions unsignedShort 1.toShort) mustEqual 1.toInt
		}
		"work for negative shorts" in {
			(functions unsignedShort -1.toShort) mustEqual 65535.toInt
		}
		"work for positive ints" in {
			(functions unsignedInt 1.toInt) mustEqual 1.toLong
		}
		"work for negative ints" in {
			(functions unsignedInt -1.toInt) mustEqual 0xffffffffL
		}
		"work for positive longs" in {
			(functions unsignedLong 1.toLong) mustEqual BigInt(1)
		}
		"work for negative longs" in {
			(functions unsignedLong -1.toLong) mustEqual BigInt("18446744073709551615")
		}
	}

	"swapEndian" should {
		"work for small shorts" in {
			(functions swapEndianShort 0x1234.toShort)	mustEqual 0x3412.toShort
		}
		"work for big shorts" in {
			(functions swapEndianShort 0x9876.toShort)	mustEqual 0x7698.toShort
		}
		"work for small ints" in {
			(functions swapEndianInt 0x12345678)	mustEqual 0x78563412
		}
		"work for big ints" in {
			(functions swapEndianInt 0x98765432)	mustEqual 0x32547698
		}
		"work for small longs" in {
			(functions swapEndianLong 0x1234567890123456L)	mustEqual 0x5634129078563412L
		}
		"work for big longs" in {
			(functions swapEndianLong 0x9876543210987654L)	mustEqual 0x5476981032547698L
		}
	}

	"FloatUtil" should {
		"detect normal float" in {
			FloatUtil.denormal(JFloat.MIN_NORMAL) mustEqual false
		}

		"detect denormal float" in {
			FloatUtil.denormal(JFloat.MIN_NORMAL / 2) mustEqual true
		}

		"detect +0 float as not denormal" in {
			FloatUtil.denormal(0f) mustEqual false
		}

		"detect -0 float as not denormal" in {
			FloatUtil.denormal(-0f) mustEqual false
		}

		"detect Infinity float as not denormal" in {
			FloatUtil.denormal(1f / 0f) mustEqual false
		}

		"detect NaN float as not denormal" in {
			FloatUtil.denormal(0f / 0f) mustEqual false
		}

		"flush denormal float to zero" in {
			FloatUtil.ftz(JFloat.MIN_NORMAL / 2) mustEqual 0f
		}
	}

	"DoubleUtil" should {
		"detect normal double" in {
			DoubleUtil.denormal(JDouble.MIN_NORMAL) mustEqual false
		}

		"detect denormal double" in {
			DoubleUtil.denormal(JDouble.MIN_NORMAL / 2) mustEqual true
		}

		"detect +0 double as not denormal" in {
			DoubleUtil.denormal(0d) mustEqual false
		}

		"detect -0 double as not denormal" in {
			DoubleUtil.denormal(-0d) mustEqual false
		}

		"detect Infinity double as not denormal" in {
			DoubleUtil.denormal(1d / 0d) mustEqual false
		}

		"detect NaN double as not denormal" in {
			DoubleUtil.denormal(0d / 0d) mustEqual false
		}

		"flush denormal double to zero" in {
			DoubleUtil.ftz(JDouble.MIN_NORMAL / 2) mustEqual 0d
		}
	 }
}
