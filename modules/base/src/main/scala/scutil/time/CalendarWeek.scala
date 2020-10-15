package scutil.time

import scutil.lang.tc._

object CalendarWeek {
	val days	= 7

	def fromIndex(index:Int):CalendarWeek	=
		JulianDay(index * days).calendarWeek

	//------------------------------------------------------------------------------
	//## typeclass instances

	implicit val CalendarWeekShow:Show[CalendarWeek]	= Show.toStringInstance
}

/** ISO 8601 week, number is 1..53 */
final case class CalendarWeek(number:Int, year:Int) extends Ordered[CalendarWeek] {
	val yearValue	= Year(year)

	require(number >= 1,						"expected number >= 1")
	require(number <= yearValue.calendarWeeks,	s"expected number <= ${yearValue.calendarWeeks.toString} in a ${if(yearValue.longCalendarWeeks) "long" else "short"} year}")

	def leaping:Boolean	= number == 53

	// TODO time ugly
	def move(offset:Int):CalendarWeek	=
		(firstDay.toJulianDay move offset*CalendarWeek.days).calendarWeek

	// TODO time ugly
	def until(that:CalendarWeek):Int	=
		(this.firstDay.toJulianDay until that.firstDay.toJulianDay) / CalendarWeek.days

	def compare(that:CalendarWeek):Int		= {
		val	y	= this.year		compare that.year;		if (y != 0)	return y
		val	n	= this.number	compare that.number;	if (n != 0)	return n
		0
	}
	def min(that:CalendarWeek):CalendarWeek	= if (this < that) this else that
	def max(that:CalendarWeek):CalendarWeek	= if (this > that) this else that

	lazy val firstDay:GregorianDate	= gregorianDayAt(Weekday.Monday)
	lazy val lastDay:GregorianDate	= gregorianDayAt(Weekday.Sunday)

	def gregorianDayAt(day:Weekday):GregorianDate	= {
		val correction	= GregorianDate(4, 1, year).weekday.index + Weekday.Thursday.index
		val start		= GregorianDate(1, 1, year)
		val offset		= number * Weekday.count + day.index - correction - 1
		start move offset
	}

	def julianDayAt(day:Weekday):JulianDay	=
		gregorianDayAt(day).toJulianDay

	lazy val toIndex:Int	=
		julianDayAt(Weekday.Monday).value / CalendarWeek.days

	override def toString:String	= s"KW${number.toString}/${year.toString}"

	def toIso8601:String	= year.toString + "W" + (if (number <= 10) "0" else "") + number.toString
}
