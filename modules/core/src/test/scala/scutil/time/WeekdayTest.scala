package scutil.time

import minitest.*

object WeekdayTest extends SimpleTestSuite {
	test("Weekday should create useful orderings (1)") {
		assertEquals(
			Weekday.all.sorted(using Weekday.ordering(Weekday.Monday)),
			Weekday.all
		)
	}

	test("Weekday should create useful orderings (2)") {
		assertEquals(
			Weekday.all.reverse.sorted(using Weekday.ordering(Weekday.Monday)),
			Weekday.all
		)
	}

	test("Weekday should create useful orderings (1)") {
		assertEquals(
			Weekday.all.sorted(using Weekday.ordering(Weekday.Sunday)),
			Weekday.all.drop(7-1) ++ Weekday.all.dropRight(0+1)
		)
	}

	test("Weekday should create useful orderings (2)") {
		assertEquals(
			Weekday.all.reverse.sorted(using Weekday.ordering(Weekday.Sunday)),
			Weekday.all.drop(7-1) ++ Weekday.all.dropRight(0+1)
		)
	}
}
