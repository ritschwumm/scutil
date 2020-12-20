package scutil.number

import minitest._

import java.math.{
	BigDecimal	=> JBigDecimal,
	BigInteger	=> JBigInteger
}

import scutil.lang.implicits._
import scutil.number.implicits._

object BigRationalTest extends SimpleTestSuite {
	test("bigrational macros should compile") {
		assertEquals(
			br"7/11",
			BigRational.fromLongs (7,11) getOrError "oops"
		)
	}

	//------------------------------------------------------------------------------

	test("bigrational syntax should convert an int") {
		assertEquals(
			1.toBigRational,
			BigRational.one
		)
	}

	test("bigrational syntax should convert a long") {
		assertEquals(
			1L.toBigRational,
			BigRational.one
		)
	}

	test("bigrational syntax should convert a JBigInteger") {
		assertEquals(
			new JBigInteger("1").toBigRational,
			BigRational.one
		)
	}

	test("bigrational syntax should convert a JBigDecimal") {
		assertEquals(
			new JBigDecimal("1").toBigRational,
			BigRational.one
		)
	}
}
