package scutil.pimp

import java.util.Date
import java.util.TimeZone
import java.text.SimpleDateFormat

import scutil.time.MilliInstant

object DateImplicits extends DateImplicits

trait DateImplicits {
	implicit def toDateExt(peer:Date) = new DateExt(peer)
}

final class DateExt(peer:Date) {
	def format(fmt:String, tz:TimeZone = TimeZone.getDefault):String	= {
		val df	= new SimpleDateFormat(fmt)
		df setTimeZone tz
		df format peer
	}
	
	def toMilliInstant:MilliInstant	= 
			MilliInstant fromDate peer
}
