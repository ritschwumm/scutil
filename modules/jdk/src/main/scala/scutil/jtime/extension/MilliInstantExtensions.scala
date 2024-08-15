package scutil.jtime.extension

import java.util.*
import java.time.Instant
import java.time.format.DateTimeFormatterBuilder

import scutil.time.*
import scutil.jtime.*

object MilliInstantExtensions {
	// DateTimeFormatter.ISO_INSTANT as usd in Instant.toString leaves out the milliseconds when they are 0
	private val isoInstantMilli	= new DateTimeFormatterBuilder().appendInstant(3).toFormatter();

	extension (peer:MilliInstant) {
		def toGregorianDate(tz:TimeZone):GregorianDate	=
			JTimeUtil.milliInstantToGregorianDate(peer, tz)

		def toDate:Date	=
			JTimeUtil.milliInstantToDate(peer)

		def toInstant:Instant	=
			JTimeUtil.milliInstantToInstant(peer)

		/** always includes milliseconds */
		def toIso8601:String	=
			isoInstantMilli.format(toInstant)
	}
}
