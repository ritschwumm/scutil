package scutil.number

import org.specs2.mutable._

import java.math.{
	BigDecimal	=> JBigDecimal,
	BigInteger	=> JBigInteger
}

import scutil.lang.implicits._
import scutil.number.implicits._

class BigRationalTest extends Specification {
	"bigrational macros" should {
		"compile" in {
			br"7/11" mustEqual (BigRational.fromLongs (7,11) getOrError "oops")
		}
	}

	"bigrational syntax" should {
		"convert an int" in {
			1.toBigRational mustEqual BigRational.one
		}
		"convert a long" in {
			1L.toBigRational mustEqual BigRational.one
		}
		"convert a JBigInteger" in {
			new JBigInteger("1").toBigRational mustEqual BigRational.one
		}
		"convert a JBigDecimal" in {
			new JBigDecimal("1").toBigRational mustEqual BigRational.one
		}
	}
}
