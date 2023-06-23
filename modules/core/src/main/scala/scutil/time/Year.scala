package scutil.time

import scutil.lang.tc.*
import scutil.math.functions.*

object Year {
	private val longYears:Set[Int]	=
		Set(
			4, 9, 15, 20, 26, 32, 37, 43, 48, 54, 60, 65, 71, 76, 82, 88, 93, 99,
			105, 111, 116, 122, 128, 133, 139, 144, 150, 156, 161, 167, 172, 178, 184, 189, 195,
			201, 207, 212, 218, 224, 229, 235, 240, 246, 252, 257, 263, 268, 274, 280, 285, 291, 296,
			303, 308, 314, 320, 325, 331, 336, 342, 348, 353, 359, 364, 370, 376, 381, 387, 392, 398
		)

	//------------------------------------------------------------------------------
	//## typeclass instances

	given YearShow:Show[Year]	= Show.toStringInstance
}

final case class Year(value:Int) extends Ordered[Year] {
	def move(offset:Int):Year	= Year(value + offset)
	def until(that:Year):Int	= that.value - this.value

	def compare(that:Year):Int	= this.value compare that.value
	def min(that:Year):Year		= if (this < that) this else that
	def max(that:Year):Year		= if (this > that) this else that

	def days:Int	=
		if (gregorianLeap)	366
		else				365

	lazy val firstDay:GregorianDate	= GregorianDate(1,	1,	value)
	lazy val lastDay:GregorianDate	= GregorianDate(31,	12,	value)

	def monthYearAt(month:Month):MonthYear		= MonthYear.fromValues(month, this)
	def calendarWeekAt(number:Int):CalendarWeek	= CalendarWeek(number, value)

	lazy val firstCalendarWeek:CalendarWeek	= calendarWeekAt(1)
	lazy val lastCalendarWeek:CalendarWeek	= calendarWeekAt(calendarWeeks)

	def calendarWeeks:Int	=
		if (longCalendarWeeks)	53
		else					52

	lazy val longCalendarWeeks:Boolean	=
		Year.longYears contains moduloInt(value, 2000)

	lazy val gregorianLeap:Boolean	=
		(value % 4 == 0)	&&
		!(value % 100 == 0) ||
		(value % 400 == 0)

	override def toString:String	= value.toString
}
