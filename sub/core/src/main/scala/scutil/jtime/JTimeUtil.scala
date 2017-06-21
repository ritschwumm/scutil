package scutil.jtime

import java.util._
import java.text.SimpleDateFormat

import scutil.lang._
import scutil.time._

object JTimeUtil {
	def milliInstantToGregorian(instant:MilliInstant, tz:TimeZone):Gregorian	= {
		val	cal	= new GregorianCalendar(tz)
		cal setTime milliInstantToDate(instant)
		calendarToGregorian(cal)
	}
	
	//------------------------------------------------------------------------------
	
	def gregorianToday(tz:TimeZone):Gregorian	=
			calendarToGregorian(new GregorianCalendar(tz))
				
	def calendarToGregorian(cal:Calendar):Gregorian	=
			Gregorian(
				(cal get Calendar.DAY_OF_MONTH) + 0,
				(cal get Calendar.MONTH)		+ 1,
				(cal get Calendar.YEAR)			+ 0)
	
	def gregorianToCalendar(it:Gregorian, tz:TimeZone):Option[GregorianCalendar]	= {
		val	cal	= new GregorianCalendar(tz)
		cal setLenient false
		gregorianIntoCalendar(it, cal)
		try { cal.getTime; Some(cal) }
		catch { case e:Exception => None }
	}
	
	def gregorianIntoCalendar(it:Gregorian, cal:Calendar) {
		cal set (Calendar.DAY_OF_MONTH,	it.day		- 0)
		cal set (Calendar.MONTH,		it.month	- 1)
		cal set (Calendar.YEAR,			it.year	- 0)
	}
	
	//------------------------------------------------------------------------------
	
	val milliInstantDateBijection	= Bijection[MilliInstant,Date](milliInstantToDate, dateToMilliInstant)
	
	def dateToMilliInstant(it:Date):MilliInstant	=
			MilliInstant(it.getTime)
		
	def milliInstantToDate(it:MilliInstant):Date	=
			new Date(it.millis)
	
	//------------------------------------------------------------------------------
	
	def dateToISO8601(date:Date):String	= {
		val	df	= new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
		df setTimeZone (TimeZone getTimeZone "UTC")
		df format date
	}
}
