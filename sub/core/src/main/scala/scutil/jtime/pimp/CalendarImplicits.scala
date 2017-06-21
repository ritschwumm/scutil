package scutil.jtime.pimp

import java.util.Calendar

import scutil.time._
import scutil.jtime._

object CalendarImplicits extends CalendarImplicits

trait CalendarImplicits {
	implicit def toCalendarExt(peer:Calendar) = new CalendarExt(peer)
}

final class CalendarExt(peer:Calendar) {
	def toGregorian:Gregorian	= JTimeUtil calendarToGregorian peer
}
