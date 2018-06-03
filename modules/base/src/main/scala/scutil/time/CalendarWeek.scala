package scutil.time

import scutil.lang.tc._

object CalendarWeek extends CalendarWeekInstances {
	val days	= 7
	
	def fromIndex(index:Int):CalendarWeek	=
			JulianDay(index*7).calendarWeek
}

/** number in 1..53 */
final case class CalendarWeek(number:Int, year:Int) extends Ordered[CalendarWeek] {
	// TODO check countInYear here
	require(number >= 1,	"expected number >= 1")
	require(number <= 53,	"expected number >= 53")
	
	// TODO ugly
	def move(offset:Int):CalendarWeek	= 
			(firstDay.toJulianDay move offset*CalendarWeek.days).calendarWeek
	// TODO ugly
	def until(that:CalendarWeek):Int	=
			(this.firstDay.toJulianDay until that.firstDay.toJulianDay) / 7
	
	def compare(that:CalendarWeek):Int		= {
		val	y	= this.year		compare that.year;		if (y != 0)	return y
		val	n	= this.number	compare that.number;	if (n != 0)	return n
		0
	}
	def min(that:CalendarWeek):CalendarWeek	= if (this < that) this else that
	def max(that:CalendarWeek):CalendarWeek	= if (this > that) this else that
	
	lazy val firstDay:GregorianDate	= gregorianDayAt(Monday)
	lazy val lastDay:GregorianDate	= gregorianDayAt(Sunday)
		
	def gregorianDayAt(day:Weekday):GregorianDate	= {
		val correction	= GregorianDate(4, 1, year).weekday.index + Thursday.index
		val start		= GregorianDate(1, 1, year)
		val offset		= number * Weekday.count + day.index - correction - 1
		start move offset
	}
	
	def julianDayAt(day:Weekday):JulianDay	=
			gregorianDayAt(day).toJulianDay
		
	lazy val toIndex:Int	=
			julianDayAt(Monday).value / 7
		
	override def toString:String	= s"KW${number}/${year}"
}

trait CalendarWeekInstances {
	implicit val CalendarWeekShow:Show[CalendarWeek]	= Show.toStringInstance
}
