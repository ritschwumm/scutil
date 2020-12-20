package scutil.time

import minitest._

object CalendarWeekDatesTest extends SimpleTestSuite {
	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(39, 2008) gregorianDayAt Weekday.Saturday,
			GregorianDate(27,9,2008)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(53, 2004) gregorianDayAt Weekday.Saturday,
			GregorianDate(1,1,2005)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(53, 2004) gregorianDayAt Weekday.Sunday,
			GregorianDate(2,1,2005)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(52, 2005) gregorianDayAt Weekday.Saturday,
			GregorianDate(31,12,2005)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(52, 2005) gregorianDayAt Weekday.Sunday,
			GregorianDate(1,1,2006)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(1, 2006) gregorianDayAt Weekday.Monday,
			GregorianDate(2,1,2006)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(52, 2006) gregorianDayAt Weekday.Sunday,
			GregorianDate(31,12,2006)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(1, 2007) gregorianDayAt Weekday.Monday,
			GregorianDate(1,1,2007)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(52, 2007) gregorianDayAt Weekday.Sunday,
			GregorianDate(30,12,2007)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(1, 2008) gregorianDayAt Weekday.Monday,
			GregorianDate(31,12,2007)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(1, 2008) gregorianDayAt Weekday.Tuesday,
			GregorianDate(1,1,2008)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(52, 2008) gregorianDayAt Weekday.Sunday,
			GregorianDate(28,12,2008)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(1, 2009) gregorianDayAt Weekday.Monday,
			GregorianDate(29,12,2008)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(1, 2009) gregorianDayAt Weekday.Tuesday,
			GregorianDate(30,12,2008)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(1, 2009) gregorianDayAt Weekday.Wednesday,
			GregorianDate(31,12,2008)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(1, 2009) gregorianDayAt Weekday.Thursday,
			GregorianDate(1,1,2009)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(53, 2009) gregorianDayAt Weekday.Thursday,
			GregorianDate(31,12,2009)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(53, 2009) gregorianDayAt Weekday.Friday,
			GregorianDate(1,1,2010)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(53, 2009) gregorianDayAt Weekday.Saturday,
			GregorianDate(2,1,2010)
		)
	}

	test("CalendarWeek should calculate the correct date") {
		assertEquals(
			CalendarWeek(53, 2009) gregorianDayAt Weekday.Sunday,
			GregorianDate(3,1,2010)
		)
	}
}
