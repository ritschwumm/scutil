package scutil.jtime

import java.util._
import java.text.SimpleDateFormat

import scutil.lang._
import scutil.time._

object JTimeUtil {
	def milliInstantToGregorianDate(instant:MilliInstant, tz:TimeZone):GregorianDate	= {
		val	cal	= new GregorianCalendar(tz)
		cal setTime milliInstantToDate(instant)
		calendarToGregorianDate(cal)
	}
	
	//------------------------------------------------------------------------------
	
	def gregorianDateToday(tz:TimeZone):GregorianDate	=
			calendarToGregorianDate(new GregorianCalendar(tz))
				
	def calendarToGregorianDate(cal:Calendar):GregorianDate	=
			GregorianDate(
				(cal get Calendar.DAY_OF_MONTH) + 0,
				(cal get Calendar.MONTH)		+ 1,
				(cal get Calendar.YEAR)			+ 0
			)
	
	def gregorianDateToCalendar(it:GregorianDate, tz:TimeZone):Option[GregorianCalendar]	= {
		val	cal	= new GregorianCalendar(tz)
		cal setLenient false
		gregorianDateIntoCalendar(it, cal)
		try { cal.getTime; Some(cal) }
		catch { case e:Exception => None }
	}
	
	def gregorianDateIntoCalendar(it:GregorianDate, cal:Calendar) {
		cal set (Calendar.DAY_OF_MONTH,	it.day		- 0)
		cal set (Calendar.MONTH,		it.month	- 1)
		cal set (Calendar.YEAR,			it.year		- 0)
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
