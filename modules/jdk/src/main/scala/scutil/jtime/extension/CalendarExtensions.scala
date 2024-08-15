package scutil.jtime.extension

import java.util.Calendar

import scutil.time.*
import scutil.jtime.*

object CalendarExtensions {
	extension (peer:Calendar) {
		def toGregorianDate:GregorianDate	= JTimeUtil.calendarToGregorianDate(peer)
	}
}
