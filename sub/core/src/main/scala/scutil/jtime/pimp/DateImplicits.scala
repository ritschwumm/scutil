package scutil.jtime.pimp

import java.util.Date
import java.util.TimeZone
import java.text.SimpleDateFormat

import scutil.time._
import scutil.jtime._

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
	
	def toISO8601:String	=
			JTimeUtil dateToISO8601 peer
	
	def toMilliInstant:MilliInstant	=
			JTimeUtil dateToMilliInstant peer
}
