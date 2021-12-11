package scutil.jtime.extension

import java.util.Date
import java.util.TimeZone
import java.text.SimpleDateFormat

import scutil.time.*
import scutil.jtime.*

object DateExtensions {
	implicit final class DateExt(peer:Date) {
		def format(fmt:String, tz:TimeZone = TimeZone.getDefault):String	= {
			val df	= new SimpleDateFormat(fmt)
			df setTimeZone tz
			df format peer
		}

		def toIso8601:String			= JTimeUtil instantToIso8601 peer.toInstant
		def toMilliInstant:MilliInstant	= JTimeUtil dateToMilliInstant peer
	}
}
