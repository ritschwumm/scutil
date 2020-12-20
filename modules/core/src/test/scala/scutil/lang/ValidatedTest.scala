package scutil.lang

import minitest._

object ValidatedTest extends SimpleTestSuite {
	type E = Nes[String]
	def E(s:String):E				= Nes.single(s)
	def EE(s:String, ss:String*):E	= Nes.of(s,ss:_*)

	//------------------------------------------------------------------------------

	test("Validated should do successful ap") {
		val func:Validated[E,Int=>Int]	= Validated good (_ * 2)
		val value:Validated[E,Int]		= Validated good 7
		assertEquals(
			func ap value,
			Validated.good(14)
		)
	}

	test("Validated should do successful pa") {
		val func:Validated[E,Int=>Int]	= Validated good (_ * 2)
		val value:Validated[E,Int]		= Validated good 7
		assertEquals(
			value pa func,
			Validated.good(14)
		)
	}

	test("Validated should combine function-first in ap") {
		val func:Validated[E,Int=>Int]	= Validated bad E("bug")
		val value:Validated[E,Int]		= Validated bad E("error")
		assertEquals(
			func ap value,
			Validated.bad(EE("bug", "error"))
		)
	}

	test("Validated should combine function-first in pa") {
		val func:Validated[E,Int=>Int]	= Validated bad E("bug")
		val value:Validated[E,Int]		= Validated bad E("error")
		assertEquals(
			value pa func,
			Validated.bad(EE("bug", "error"))
		)
	}
}
