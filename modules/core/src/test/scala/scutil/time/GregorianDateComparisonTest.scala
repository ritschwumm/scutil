package scutil.time

import minitest.*

object GregorianDateComparisonTest extends SimpleTestSuite {
	test("GregorianDate should compare equal days correctly") {
		assertEquals(
			GregorianDate(7,8,2012) compare GregorianDate(7,8,2012),
			0
		)
	}

	//------------------------------------------------------------------------------

	test("GregorianDate should compare earlier day correctly") {
		assertEquals(
			GregorianDate(6,7,2012) compare GregorianDate(7,8,2012),
			-1
		)
	}

	test("GregorianDate should compare later day correctly") {
		assertEquals(
			GregorianDate(8,8,2012) compare GregorianDate(7,8,2012),
			+1
		)
	}

	//------------------------------------------------------------------------------

	test("GregorianDate should compare earlier month correctly") {
		assertEquals(
			GregorianDate(7,7,2012) compare GregorianDate(7,8,2012),
			-1
		)
	}

	test("GregorianDate should compare later month correctly") {
		assertEquals(
			GregorianDate(7,9,2012) compare GregorianDate(7,8,2012),
			+1
		)
	}

	//------------------------------------------------------------------------------

	test("GregorianDate should compare earlier year correctly") {
		assertEquals(
			GregorianDate(7,8,2011) compare GregorianDate(7,8,2012),
			-1
		)
	}

	test("GregorianDate should compare later year correctly") {
		assertEquals(
			GregorianDate(7,8,2013) compare GregorianDate(7,8,2012),
			+1
		)
	}

	//------------------------------------------------------------------------------

	test("GregorianDate should prefer year over month") {
		assertEquals(
			GregorianDate(7,8,2012) compare GregorianDate(7,9,2013),
			-1
		)
	}

	test("GregorianDate should prefer month over day") {
		assertEquals(
			GregorianDate(7,8,2012) compare GregorianDate(6,9,2012),
			-1
		)
	}
}
