package scutil.lang

import minitest.*

object ValidatedTest extends SimpleTestSuite {
	type E = Nes[String]
	def E(s:String):E				= Nes.one(s)
	def EE(s:String, ss:String*):E	= Nes.of(s,ss*)

	//------------------------------------------------------------------------------

	test("Validated should do successful ap") {
		val func:Validated[E,Int=>Int]	= Validated valid (_ * 2)
		val value:Validated[E,Int]		= Validated valid 7
		assertEquals(
			func ap value,
			Validated.valid(14)
		)
	}

	test("Validated should combine function-first in ap") {
		val func:Validated[E,Int=>Int]	= Validated invalid E("bug")
		val value:Validated[E,Int]		= Validated invalid E("error")
		assertEquals(
			func ap value,
			Validated.invalid(EE("bug", "error"))
		)
	}
}
