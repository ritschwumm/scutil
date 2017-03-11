package scutil.lang

import org.specs2.mutable._

class TriedTest extends Specification {
	"Tried" should {
		"do successful ap" in {
			val func:Tried[String,Int=>Int]	= Tried win (_ * 2)
			val value:Tried[String,Int]		= Tried win 7
			func ap value mustEqual Win(14)
		}
		"do successful pa" in {
			val func:Tried[String,Int=>Int]	= Tried win (_ * 2)
			val value:Tried[String,Int]		= Tried win 7
			value pa func mustEqual Win(14)
		}
		"abort function-first in ap" in {
			val func:Tried[String,Int=>Int]	= Tried fail "bug"
			val value:Tried[String,Int]		= Tried fail "error"
			func ap value mustEqual Fail("bug")
		}
		"abort function-first in pa" in {
			val func:Tried[String,Int=>Int]	= Tried fail "bug"
			val value:Tried[String,Int]		= Tried fail "error"
			value pa func mustEqual Fail("bug")
		}
	}
}
