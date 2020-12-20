package scutil.lang

import minitest._

import scutil.core.implicits._

object EitherTest extends SimpleTestSuite {
	test("Either should do successful ap") {
		val func:Either[String,Int=>Int]	= Either right (_ * 2)
		val value:Either[String,Int]		= Either right 7
		assertEquals(
			func ap value,
			Right(14)
		)
	}

	test("Either should do successful pa") {
		val func:Either[String,Int=>Int]	= Either right (_ * 2)
		val value:Either[String,Int]		= Either right 7
		assertEquals(
			value pa func,
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

	test("Either should abort function-first in pa") {
		val func:Either[String,Int=>Int]	= Either left "bug"
		val value:Either[String,Int]		= Either left "error"
		assertEquals(
			value pa func,
			Left("bug")
		)
	}
}
