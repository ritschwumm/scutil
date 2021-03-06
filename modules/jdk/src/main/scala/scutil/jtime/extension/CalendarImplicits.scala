package scutil.jtime.extension

import java.util.Calendar

import scutil.time._
import scutil.jtime._

object CalendarImplicits extends CalendarImplicits

trait CalendarImplicits {
	implicit final class CalendarExt(peer:Calendar) {
		def toGregorianDate:GregorianDate	= JTimeUtil calendarToGregorianDate peer
	}
}
