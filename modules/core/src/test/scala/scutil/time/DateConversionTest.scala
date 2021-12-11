package scutil.time

import minitest.*

object DateConversionTest extends SimpleTestSuite {
	test("convert JulianDay 2457700 to GregorianDate 7.11.2016") {
		assertEquals(
			JulianDay(2457700).toGregorianDate,
			GregorianDate(7,11,2016)
		)
	}

	test("convert GregorianDate 7.11.2016 to JulianDay 2457700") {
		assertEquals(
			GregorianDate(7,11,2016).toJulianDay,
			JulianDay(2457700)
		)
	}
}
