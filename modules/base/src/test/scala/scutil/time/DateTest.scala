package scutil.time

import org.specs2.mutable._

class DateTest extends Specification {
	"conversion" should {
		"convert JulianDay 2457700 to GregorianDate 7.11.2016" in {
			JulianDay(2457700).toGregorianDate mustEqual GregorianDate(7,11,2016)
		}
		"convert GregorianDate 7.11.2016 to JulianDay 2457700" in {
			GregorianDate(7,11,2016).toJulianDay mustEqual JulianDay(2457700)
		}
	}

	"GregorianDate comparison" should {
		"compare equal days correctly" in {
			GregorianDate(7,8,2012) compare GregorianDate(7,8,2012) mustEqual 0
		}

		"compare earlier day correctly" in {
			GregorianDate(6,7,2012) compare GregorianDate(7,8,2012) mustEqual -1
		}
		"compare later day correctly" in {
			GregorianDate(8,8,2012) compare GregorianDate(7,8,2012) mustEqual +1
		}

		"compare earlier month correctly" in {
			GregorianDate(7,7,2012) compare GregorianDate(7,8,2012) mustEqual -1
		}
		"compare later month correctly" in {
			GregorianDate(7,9,2012) compare GregorianDate(7,8,2012) mustEqual +1
		}

		"compare earlier year correctly" in {
			GregorianDate(7,8,2011) compare GregorianDate(7,8,2012) mustEqual -1
		}
		"compare later year correctly" in {
			GregorianDate(7,8,2013) compare GregorianDate(7,8,2012) mustEqual +1
		}

		"prefer year over month" in {
			GregorianDate(7,8,2012) compare GregorianDate(7,9,2013) mustEqual -1
		}
		"prefer month over day" in {
			GregorianDate(7,8,2012) compare GregorianDate(6,9,2012) mustEqual -1
		}

		"have the correct week day" in {
			GregorianDate(1,1,2005).weekday mustEqual Weekday.Saturday
		}
		"have the correct week day" in {
			GregorianDate(2,1,2005).weekday mustEqual Weekday.Sunday
		}
		"have the correct week day" in {
			GregorianDate(31,12,2005).weekday mustEqual Weekday.Saturday
		}
		"have the correct week day" in {
			GregorianDate(1,1,2006).weekday mustEqual Weekday.Sunday
		}
		"have the correct week day" in {
			GregorianDate(2,1,2006).weekday mustEqual Weekday.Monday
		}
		"have the correct week day" in {
			GregorianDate(31,12,2006).weekday mustEqual Weekday.Sunday
		}
		"have the correct week day" in {
			GregorianDate(1,1,2007).weekday mustEqual Weekday.Monday
		}
		"have the correct week day" in {
			GregorianDate(30,12,2007).weekday mustEqual Weekday.Sunday
		}
		"have the correct week day" in {
			GregorianDate(31,12,2007).weekday mustEqual Weekday.Monday
		}
		"have the correct week day" in {
			GregorianDate(1,1,2008).weekday mustEqual Weekday.Tuesday
		}
		"have the correct week day" in {
			GregorianDate(28,12,2008).weekday mustEqual Weekday.Sunday
		}
		"have the correct week day" in {
			GregorianDate(29,12,2008).weekday mustEqual Weekday.Monday
		}
		"have the correct week day" in {
			GregorianDate(30,12,2008).weekday mustEqual Weekday.Tuesday
		}
		"have the correct week day" in {
			GregorianDate(31,12,2008).weekday mustEqual Weekday.Wednesday
		}
		"have the correct week day" in {
			GregorianDate(1,1,2009).weekday mustEqual Weekday.Thursday
		}
		"have the correct week day" in {
			GregorianDate(31,12,2009).weekday mustEqual Weekday.Thursday
		}
		"have the correct week day" in {
			GregorianDate(1,1,2010).weekday mustEqual Weekday.Friday
		}
		"have the correct week day" in {
			GregorianDate(2,1,2010).weekday mustEqual Weekday.Saturday
		}
		"have the correct week day" in {
			GregorianDate(3,1,2010).weekday mustEqual Weekday.Sunday
		}
	}

	"GregorianDate week day" should {
		"have the correct week day" in {
			GregorianDate(1,1,2005).weekday mustEqual Weekday.Saturday
		}
		"have the correct week day" in {
			GregorianDate(2,1,2005).weekday mustEqual Weekday.Sunday
		}
		"have the correct week day" in {
			GregorianDate(31,12,2005).weekday mustEqual Weekday.Saturday
		}
		"have the correct week day" in {
			GregorianDate(1,1,2006).weekday mustEqual Weekday.Sunday
		}
		"have the correct week day" in {
			GregorianDate(2,1,2006).weekday mustEqual Weekday.Monday
		}
		"have the correct week day" in {
			GregorianDate(31,12,2006).weekday mustEqual Weekday.Sunday
		}
		"have the correct week day" in {
			GregorianDate(1,1,2007).weekday mustEqual Weekday.Monday
		}
		"have the correct week day" in {
			GregorianDate(30,12,2007).weekday mustEqual Weekday.Sunday
		}
		"have the correct week day" in {
			GregorianDate(31,12,2007).weekday mustEqual Weekday.Monday
		}
		"have the correct week day" in {
			GregorianDate(1,1,2008).weekday mustEqual Weekday.Tuesday
		}
		"have the correct week day" in {
			GregorianDate(28,12,2008).weekday mustEqual Weekday.Sunday
		}
		"have the correct week day" in {
			GregorianDate(29,12,2008).weekday mustEqual Weekday.Monday
		}
		"have the correct week day" in {
			GregorianDate(30,12,2008).weekday mustEqual Weekday.Tuesday
		}
		"have the correct week day" in {
			GregorianDate(31,12,2008).weekday mustEqual Weekday.Wednesday
		}
		"have the correct week day" in {
			GregorianDate(1,1,2009).weekday mustEqual Weekday.Thursday
		}
		"have the correct week day" in {
			GregorianDate(31,12,2009).weekday mustEqual Weekday.Thursday
		}
		"have the correct week day" in {
			GregorianDate(1,1,2010).weekday mustEqual Weekday.Friday
		}
		"have the correct week day" in {
			GregorianDate(2,1,2010).weekday mustEqual Weekday.Saturday
		}
		"have the correct week day" in {
			GregorianDate(3,1,2010).weekday mustEqual Weekday.Sunday
		}
	}

	"GregorianDate calendar week day" should {
		"calculate the correct week for 22.12.2003" in {
			GregorianDate(22, 12, 2003).calendarWeek mustEqual CalendarWeek(52, 2003)
		}
		"calculate the correct week for 28.12.2003" in {
			GregorianDate(28, 12, 2003).calendarWeek mustEqual CalendarWeek(52, 2003)
		}
		"calculate the correct week for 29.12.2003" in {
			GregorianDate(29, 12, 2003).calendarWeek mustEqual CalendarWeek(1, 2004)
		}
		"calculate the correct week for 4.1.2004" in {
			GregorianDate(4, 1, 2004).calendarWeek mustEqual CalendarWeek(1, 2004)
		}

		"calculate the correct week" in {
			GregorianDate(1,1,2005).calendarWeek mustEqual CalendarWeek(53, 2004)
		}
		"calculate the correct week" in {
			GregorianDate(2,1,2005).calendarWeek mustEqual CalendarWeek(53, 2004)
		}
		"calculate the correct week" in {
			GregorianDate(31,12,2005).calendarWeek mustEqual CalendarWeek(52, 2005)
		}
		"calculate the correct week" in {
			GregorianDate(1,1,2006).calendarWeek mustEqual CalendarWeek(52, 2005)
		}
		"calculate the correct week" in {
			GregorianDate(2,1,2006).calendarWeek mustEqual CalendarWeek(1, 2006)
		}
		"calculate the correct week" in {
			GregorianDate(31,12,2006).calendarWeek mustEqual CalendarWeek(52, 2006)
		}
		"calculate the correct week" in {
			GregorianDate(1,1,2007).calendarWeek mustEqual CalendarWeek(1, 2007)
		}
		"calculate the correct week" in {
			GregorianDate(30,12,2007).calendarWeek mustEqual CalendarWeek(52, 2007)
		}
		"calculate the correct week" in {
			GregorianDate(31,12,2007).calendarWeek mustEqual CalendarWeek(1, 2008)
		}
		"calculate the correct week" in {
			GregorianDate(1,1,2008).calendarWeek mustEqual CalendarWeek(1, 2008)
		}
		"calculate the correct week" in {
			GregorianDate(28,12,2008).calendarWeek mustEqual CalendarWeek(52, 2008)
		}
		"calculate the correct week" in {
			GregorianDate(29,12,2008).calendarWeek mustEqual CalendarWeek(1, 2009)
		}
		"calculate the correct week" in {
			GregorianDate(30,12,2008).calendarWeek mustEqual CalendarWeek(1, 2009)
		}
		"calculate the correct week" in {
			GregorianDate(31,12,2008).calendarWeek mustEqual CalendarWeek(1, 2009)
		}
		"calculate the correct week" in {
			GregorianDate(1,1,2009).calendarWeek mustEqual CalendarWeek(1, 2009)
		}
		"calculate the correct week" in {
			GregorianDate(31,12,2009).calendarWeek mustEqual CalendarWeek(53, 2009)
		}
		"calculate the correct week" in {
			GregorianDate(1,1,2010).calendarWeek mustEqual CalendarWeek(53, 2009)
		}
		"calculate the correct week" in {
			GregorianDate(2,1,2010).calendarWeek mustEqual CalendarWeek(53, 2009)
		}
		"calculate the correct week" in {
			GregorianDate(3,1,2010).calendarWeek mustEqual CalendarWeek(53, 2009)
		}
	}

	"CalendarWeek dates" should {
		"calculate the correct date" in {
			CalendarWeek(39, 2008) gregorianDayAt Weekday.Saturday mustEqual GregorianDate(27,9,2008)
		}

		"calculate the correct date" in {
			CalendarWeek(53, 2004) gregorianDayAt Weekday.Saturday mustEqual GregorianDate(1,1,2005)
		}
		"calculate the correct date" in {
			CalendarWeek(53, 2004) gregorianDayAt Weekday.Sunday mustEqual GregorianDate(2,1,2005)
		}
		"calculate the correct date" in {
			CalendarWeek(52, 2005) gregorianDayAt Weekday.Saturday mustEqual GregorianDate(31,12,2005)
		}
		"calculate the correct date" in {
			CalendarWeek(52, 2005) gregorianDayAt Weekday.Sunday mustEqual GregorianDate(1,1,2006)
		}
		"calculate the correct date" in {
			CalendarWeek(1, 2006) gregorianDayAt Weekday.Monday mustEqual GregorianDate(2,1,2006)
		}
		"calculate the correct date" in {
			CalendarWeek(52, 2006) gregorianDayAt Weekday.Sunday mustEqual GregorianDate(31,12,2006)
		}
		"calculate the correct date" in {
			CalendarWeek(1, 2007) gregorianDayAt Weekday.Monday mustEqual GregorianDate(1,1,2007)
		}
		"calculate the correct date" in {
			CalendarWeek(52, 2007) gregorianDayAt Weekday.Sunday mustEqual GregorianDate(30,12,2007)
		}
		"calculate the correct date" in {
			CalendarWeek(1, 2008) gregorianDayAt Weekday.Monday mustEqual GregorianDate(31,12,2007)
		}
		"calculate the correct date" in {
			CalendarWeek(1, 2008) gregorianDayAt Weekday.Tuesday mustEqual GregorianDate(1,1,2008)
		}
		"calculate the correct date" in {
			CalendarWeek(52, 2008) gregorianDayAt Weekday.Sunday mustEqual GregorianDate(28,12,2008)
		}
		"calculate the correct date" in {
			CalendarWeek(1, 2009) gregorianDayAt Weekday.Monday mustEqual GregorianDate(29,12,2008)
		}
		"calculate the correct date" in {
			CalendarWeek(1, 2009) gregorianDayAt Weekday.Tuesday mustEqual GregorianDate(30,12,2008)
		}
		"calculate the correct date" in {
			CalendarWeek(1, 2009) gregorianDayAt Weekday.Wednesday mustEqual GregorianDate(31,12,2008)
		}
		"calculate the correct date" in {
			CalendarWeek(1, 2009) gregorianDayAt Weekday.Thursday mustEqual GregorianDate(1,1,2009)
		}
		"calculate the correct date" in {
			CalendarWeek(53, 2009) gregorianDayAt Weekday.Thursday mustEqual GregorianDate(31,12,2009)
		}
		"calculate the correct date" in {
			CalendarWeek(53, 2009) gregorianDayAt Weekday.Friday mustEqual GregorianDate(1,1,2010)
		}
		"calculate the correct date" in {
			CalendarWeek(53, 2009) gregorianDayAt Weekday.Saturday mustEqual GregorianDate(2,1,2010)
		}
		"calculate the correct date" in {
			CalendarWeek(53, 2009) gregorianDayAt Weekday.Sunday mustEqual GregorianDate(3,1,2010)
		}
	}

	"CalendarWeek" should {
		"roundtrip index" in {
			0 until 10000 by 3 map { i =>
				val jd	= JulianDay.epoch move i
				val x1	= jd.calendarWeek
				val idx	= x1.toIndex
				val x2	= CalendarWeek fromIndex idx
				x1 mustEqual x2
			}
		}
	}

	"MonthYear" should {
		"roundtrip index" in {
			0 until 10000 by 3 map { i =>
				val jd	= JulianDay.epoch move i
				val x1	= jd.monthYear
				val idx	= x1.toIndex
				val x2	= MonthYear fromIndex idx
				x1 mustEqual x2
			}
		}
	}

	"Weekday" should {
		"create useful orderings (1)" in {
			Weekday.all.sorted(Weekday.ordering(Weekday.Monday)) mustEqual Weekday.all
		}
		"create useful orderings (2)" in {
			Weekday.all.reverse.sorted(Weekday.ordering(Weekday.Monday)) mustEqual Weekday.all
		}
		"create useful orderings (1)" in {
			Weekday.all.sorted(Weekday.ordering(Weekday.Sunday)) mustEqual Weekday.all.drop(7-1) ++ Weekday.all.dropRight(0+1)
		}
		"create useful orderings (2)" in {
			Weekday.all.reverse.sorted(Weekday.ordering(Weekday.Sunday)) mustEqual Weekday.all.drop(7-1) ++ Weekday.all.dropRight(0+1)
		}
	}
}
