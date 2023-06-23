package scutil.lang

import minitest.*

import scutil.core.implicits.*

object EitherTest extends SimpleTestSuite {
	test("Either should do successful ap") {
		val func:Either[String,Int=>Int]	= Either right (_ * 2)
		val value:Either[String,Int]		= Either right 7
		assertEquals(
			func ap value,
			Right(14)
		)
	}

	test("Either should abort function-first in ap") {
		val func:Either[String,Int=>Int]	= Either left "bug"
		val value:Either[String,Int]		= Either left "error"
		assertEquals(
			func ap value,
			Left("bug")
		)
	}

	test("Either should convert to Try") {
		val func:Either[Throwable,Int]	= Either right 1
		assertEquals(
			func.toTry,
			scala.util.Success(1)
		)
	}
}
