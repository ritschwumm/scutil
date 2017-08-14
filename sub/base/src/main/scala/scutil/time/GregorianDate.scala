package scutil.time

import scutil.math.functions._

object GregorianDate {
	val epoch	= GregorianDate(1, 1, 1970)
}

final case class GregorianDate(day:Int, month:Int, year:Int) extends Ordered[GregorianDate] {
	require(day		>= 1,	s"expected day >= 1, got $day")
	require(day		<= 31,	s"expected day <= 31, got $day")
	require(month	>= 1,	s"expected month >= 1, got $month")
	require(month	<= 12,	s"expected month <= 12, got $month")
	
	def move(offset:Int):GregorianDate	=
			(toJulianDay move offset).toGregorianDate
		
	def until(that:GregorianDate):Int	=
			this.toJulianDay until that.toJulianDay
		
	def compare(that:GregorianDate):Int		= {
		val	y	= this.year		compare that.year;	if (y != 0)	return y
		val	m	= this.month	compare that.month;	if (m != 0)	return m
		val	d	= this.day		compare that.day;	if (d != 0)	return d
		0
	}
	
	def min(that:GregorianDate):GregorianDate	= if (this < that) this else that
	def max(that:GregorianDate):GregorianDate	= if (this > that) this else that
	
	lazy val weekday:Weekday	=
			toJulianDay.weekday
		
	/** ISO 8601 as opposed to US calendar weeks */
	lazy val calendarWeek:CalendarWeek	= {
		val thisJulian	= toJulianDay
		val jan1Julian	= GregorianDate(day = 1, month = 1, year = year).toJulianDay
		
		val dayOfYear	= jan1Julian until thisJulian
		val rawNumber	= (dayOfYear - weekday.index + Weekday.count + Thursday.index) / Weekday.count
		val lastNumber	= (CalendarWeek lastIn year).number
		
			 if (rawNumber < 1)				CalendarWeek lastIn		year-1
		else if (rawNumber > lastNumber)	CalendarWeek firstIn	year+1
		else								CalendarWeek(rawNumber, year)
	}
	
	lazy val toJulianDay:JulianDay	= {
		// 1 for january and february, else 0
		val a	= floorDivLong(14 - month, 12)
		val y	= year + 4800 - a
		// 0 for march, 11 for february
		val m	= month + 12 * a - 3
		val x	=
				day								+
				floorDivLong(153 * m + 2, 5)	+
				365 * y 						+
				floorDivLong(y, 4)				-
				floorDivLong(y, 100)			+
				floorDivLong(y, 400)			-
				32045
		JulianDay(x.toInt)
	}
}
