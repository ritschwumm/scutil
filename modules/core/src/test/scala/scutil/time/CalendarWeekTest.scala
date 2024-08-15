package scutil.time

import minitest.*

object CalendarWeekTest extends SimpleTestSuite {
	test("CalendarWeek should roundtrip index") {
		0 until 10000 by 3 foreach { i =>
			val jd	= JulianDay.epoch.move(i)
			val x1	= jd.calendarWeek
			val idx	= x1.toIndex
			val x2	= CalendarWeek.fromIndex(idx)
			assertEquals(x1, x2)
		}
	}
}
