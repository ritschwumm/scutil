package scutil.lang

import org.specs2.mutable._

class ValidatedTest extends Specification {
	"Validated" should {
		type E = Nes[String]
		def E(s:String):E				= Nes single s
		def EE(s:String, ss:String*):E	= Nes multi (s,ss:_*)

		"do successful ap" in {
			val func:Validated[E,Int=>Int]	= Validated good (_ * 2)
			val value:Validated[E,Int]		= Validated good 7
			func ap value mustEqual Good(14)
		}
		"do successful pa" in {
			val func:Validated[E,Int=>Int]	= Validated good (_ * 2)
			val value:Validated[E,Int]		= Validated good 7
			value pa func mustEqual Good(14)
		}
		"concat function-first in ap" in {
			val func:Validated[E,Int=>Int]	= Validated bad E("bug")
			val value:Validated[E,Int]		= Validated bad E("error")
			func ap value mustEqual Bad(EE("bug", "error"))
		}
		"concat function-first in pa" in {
			val func:Validated[E,Int=>Int]	= Validated bad E("bug")
			val value:Validated[E,Int]		= Validated bad E("error")
			value pa func mustEqual Bad(EE("bug", "error"))
		}
	}
}
