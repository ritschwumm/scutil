package scutil.jtime.extension

import java.util.Calendar

import scutil.time.*
import scutil.jtime.*

object CalendarExtensions {
	implicit final class CalendarExt(peer:Calendar) {
		def toGregorianDate:GregorianDate	= JTimeUtil.calendarToGregorianDate(peer)
	}
}
