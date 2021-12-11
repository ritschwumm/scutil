package scutil.lang

import scala.util.Try

import minitest.*

import scutil.core.implicits.{ given, * }

object TryTest extends SimpleTestSuite {
	test("Try should convert to Either") {
		assertEquals(
			Try("test").toEither,
			Right("test")
		)
	}

	test("Try should convert to Validated") {
		assertEquals(
			Try("test").toValidated,
			Validated.Valid("test")
		)
	}

	test("Try should convert to EitherT") {
		assertEquals(
			Try("test").toEitherT[List],
			EitherT.right[List,Throwable,String]("test")
		)
	}
}
