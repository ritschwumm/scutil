package scutil.lang.tc

import minitest.*

import scutil.core.implicits.*
import scutil.lang.*

object FunctorTest extends SimpleTestSuite {
	test("as works for Validated[Vector[String],Unit]") {
		val value:Validated[Vector[String],Unit]	= Validated.valid(())
		val result	= value.as(1)

		assertEquals(
			result,
			Validated.valid(1)
		)
	}
}
