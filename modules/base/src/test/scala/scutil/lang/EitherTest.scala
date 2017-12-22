package scutil.lang

import org.specs2.mutable._

import scutil.base.implicits._

class EitherTest extends Specification {
	"Either" should {
		"do successful ap" in {
			val func:Either[String,Int=>Int]	= Either right (_ * 2)
			val value:Either[String,Int]		= Either right 7
			func ap value mustEqual Right(14)
		}
		"do successful pa" in {
			val func:Either[String,Int=>Int]	= Either right (_ * 2)
			val value:Either[String,Int]		= Either right 7
			value pa func mustEqual Right(14)
		}
		"abort function-first in ap" in {
			val func:Either[String,Int=>Int]	= Either left "bug"
			val value:Either[String,Int]		= Either left "error"
			func ap value mustEqual Left("bug")
		}
		"abort function-first in pa" in {
			val func:Either[String,Int=>Int]	= Either left "bug"
			val value:Either[String,Int]		= Either left "error"
			value pa func mustEqual Left("bug")
		}
	}
}
