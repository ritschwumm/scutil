package scutil.time

import minitest.*

object MonthYearTest extends SimpleTestSuite {
	test("MonthYear should roundtrip index") {
		0 until 10000 by 3 foreach { i =>
			val jd	= JulianDay.epoch move i
			val x1	= jd.monthYear
			val idx	= x1.toIndex
			val x2	= MonthYear fromIndex idx
			assertEquals(x1, x2)
		}
	}
}
