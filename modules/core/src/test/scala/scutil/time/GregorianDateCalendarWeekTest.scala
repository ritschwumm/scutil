package scutil.time

import minitest._

object GregorianDateCalendarWeekTest extends SimpleTestSuite {
	test("GregorianDate should calculate the correct week for 22.12.2003") {
		assertEquals(
			GregorianDate(22, 12, 2003).calendarWeek,
			CalendarWeek(52, 2003)
		)
	}

	test("GregorianDate should calculate the correct week for 28.12.2003") {
		assertEquals(
			GregorianDate(28, 12, 2003).calendarWeek,
			CalendarWeek(52, 2003)
		)
	}

	test("GregorianDate should calculate the correct week for 29.12.2003") {
		assertEquals(
			GregorianDate(29, 12, 2003).calendarWeek,
			CalendarWeek(1, 2004)
		)
	}

	test("GregorianDate should calculate the correct week for 4.1.2004") {
		assertEquals(
			GregorianDate(4, 1, 2004).calendarWeek,
			CalendarWeek(1, 2004)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(1,1,2005).calendarWeek,
			CalendarWeek(53, 2004)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(2,1,2005).calendarWeek,
			CalendarWeek(53, 2004)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(31,12,2005).calendarWeek,
			CalendarWeek(52, 2005)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(1,1,2006).calendarWeek,
			CalendarWeek(52, 2005)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(2,1,2006).calendarWeek,
			CalendarWeek(1, 2006)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(31,12,2006).calendarWeek,
			CalendarWeek(52, 2006)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(1,1,2007).calendarWeek,
			CalendarWeek(1, 2007)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(30,12,2007).calendarWeek,
			CalendarWeek(52, 2007)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(31,12,2007).calendarWeek,
			CalendarWeek(1, 2008)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(1,1,2008).calendarWeek,
			CalendarWeek(1, 2008)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(28,12,2008).calendarWeek,
			CalendarWeek(52, 2008)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(29,12,2008).calendarWeek,
			CalendarWeek(1, 2009)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(30,12,2008).calendarWeek,
			CalendarWeek(1, 2009)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(31,12,2008).calendarWeek,
			CalendarWeek(1, 2009)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(1,1,2009).calendarWeek,
			CalendarWeek(1, 2009)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(31,12,2009).calendarWeek,
			CalendarWeek(53, 2009)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(1,1,2010).calendarWeek,
			CalendarWeek(53, 2009)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(2,1,2010).calendarWeek,
			CalendarWeek(53, 2009)
		)
	}

	test("GregorianDate should calculate the correct week") {
		assertEquals(
			GregorianDate(3,1,2010).calendarWeek,
			CalendarWeek(53, 2009)
		)
	}
}
