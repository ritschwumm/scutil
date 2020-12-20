package scutil.time

import minitest._

object GregorianDateWeekdayTest extends SimpleTestSuite {
	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(1,1,2005).weekday,
			Weekday.Saturday
		)
	}

	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(2,1,2005).weekday,
			Weekday.Sunday
		)
	}

	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(31,12,2005).weekday,
			Weekday.Saturday
		)
	}

	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(1,1,2006).weekday,
			Weekday.Sunday
		)
	}

	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(2,1,2006).weekday,
			Weekday.Monday
		)
	}

	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(31,12,2006).weekday,
			Weekday.Sunday
		)
	}

	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(1,1,2007).weekday,
			Weekday.Monday
		)
	}

	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(30,12,2007).weekday,
			Weekday.Sunday
		)
	}

	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(31,12,2007).weekday,
			Weekday.Monday
		)
	}

	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(1,1,2008).weekday,
			Weekday.Tuesday
		)
	}

	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(28,12,2008).weekday,
			Weekday.Sunday
		)
	}

	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(29,12,2008).weekday,
			Weekday.Monday
		)
	}
	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(30,12,2008).weekday,
			Weekday.Tuesday
		)
	}

	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(31,12,2008).weekday,
			Weekday.Wednesday
		)
	}

	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(1,1,2009).weekday,
			Weekday.Thursday
		)
	}

	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(31,12,2009).weekday,
			Weekday.Thursday
		)
	}

	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(1,1,2010).weekday,
			Weekday.Friday
		)
	}

	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(2,1,2010).weekday,
			Weekday.Saturday
		)
	}

	test("GregorianDate should have the correct week day") {
		assertEquals(
			GregorianDate(3,1,2010).weekday,
			Weekday.Sunday
		)
	}
}
