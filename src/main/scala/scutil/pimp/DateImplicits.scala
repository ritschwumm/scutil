package scutil.pimp

import java.util.Date
import java.util.TimeZone
import java.text.SimpleDateFormat

import scutil.time.MilliInstant

object DateImplicits extends DateImplicits

trait DateImplicits {
	implicit def toDateExt(delegate:Date) = new DateExt(delegate)
}

final class DateExt(delegate:Date) {
	def format(fmt:String):String	= 
			new SimpleDateFormat(fmt) format delegate
	
	def format(fmt:String, tz:TimeZone):String	= {
		val df	= new SimpleDateFormat(fmt)
		df setTimeZone tz
		df format delegate
	}
	
	def toMilliInstant:MilliInstant	= MilliInstant fromDate delegate
}
