package scutil.time

import java.util._

object Gregorian {
	def today(tz:TimeZone):Gregorian	= {
		fromCalendar(new GregorianCalendar(tz))
	}
	
	def fromCalendar(cal:Calendar):Gregorian	=
			Gregorian(
				(cal get Calendar.DAY_OF_MONTH) + 0,
				(cal get Calendar.MONTH)		+ 1,
				(cal get Calendar.YEAR)			+ 0)
				
	def atMilliInstant(instant:MilliInstant, tz:TimeZone):Gregorian	= {
		val	cal	= new GregorianCalendar(tz)
		cal setTime instant.toDate
		fromCalendar(cal)
	}
}

final case class Gregorian(day:Int, month:Int, year:Int) extends Ordered[Gregorian] {
	def compare(that:Gregorian):Int		= {
		val	y	= this.year		compare that.year;	if (y != 0)	return y
		val	m	= this.month	compare that.month;	if (m != 0)	return m
		val	d	= this.day		compare that.day;	if (d != 0)	return d
		0
	}
	
	def min(that:Gregorian):Gregorian	= if (this < that) this else that
	def max(that:Gregorian):Gregorian	= if (this > that) this else that
	
	def toGregorianCalendar(tz:TimeZone):Option[GregorianCalendar]	= {
		val	cal	= new GregorianCalendar(tz)
		cal setLenient false
		intoCalendar(cal)
		try { cal.getTime; Some(cal) }
		catch { case e:Exception => None }
	}
	
	def intoCalendar(cal:Calendar) {
		cal set (Calendar.DAY_OF_MONTH,	day		- 0)
		cal set (Calendar.MONTH,		month	- 1)
		cal set (Calendar.YEAR,			year	- 0)
	}
}
