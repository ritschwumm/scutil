package scutil.time

import scutil.math.functions._

object CalendarWeek {
	def firstIn(year:Int):CalendarWeek	= CalendarWeek(1, year)
	
	def lastIn(year:Int):CalendarWeek	=
			if (longYear(year))	CalendarWeek(53, year)
			else				CalendarWeek(52, year)
		
	private def longYear(year:Int):Boolean	=
			longYears contains moduloInt(year, 2000)
	
	private val longYears:Set[Int]	=
			Set(
				4, 9, 15, 20, 26, 32, 37, 43, 48, 54, 60, 65, 71, 76, 82, 88, 93, 99,
				105, 111, 116, 122, 128, 133, 139, 144, 150, 156, 161, 167, 172, 178, 184, 189, 195,
				201, 207, 212, 218, 224, 229, 235, 240, 246, 252, 257, 263, 268, 274, 280, 285, 291, 296,
				303, 308, 314, 320, 325, 331, 336, 342, 348, 353, 359, 364, 370, 376, 381, 387, 392, 398
			)
}

final case class CalendarWeek(number:Int, year:Int) extends Ordered[CalendarWeek] {
	require(number >= 1,	"expected number >= 1")
	require(number <= 53,	"expected number >= 53")
	
	def compare(that:CalendarWeek):Int		= {
		val	y	= this.year		compare that.year;		if (y != 0)	return y
		val	n	= this.number	compare that.number;	if (n != 0)	return n
		0
	}
	
	def min(that:CalendarWeek):CalendarWeek	= if (this < that) this else that
	def max(that:CalendarWeek):CalendarWeek	= if (this > that) this else that
	
	def gregorianDayOf(day:Weekday):GregorianDate	= {
		val correction	= GregorianDate(4, 1, year).weekday.index + Thursday.index
		val start		= GregorianDate(1, 1, year)
		val offset		= number * Weekday.count + day.index - correction - 1
		start move offset
	}
	
	def julianDayOf(day:Weekday):JulianDay	=
			gregorianDayOf(day).toJulianDay
}
