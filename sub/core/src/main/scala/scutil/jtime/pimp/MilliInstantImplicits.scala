package scutil.jtime.pimp

import java.util._

import scutil.time._
import scutil.jtime._

object MilliInstantImplicits extends MilliInstantImplicits

trait MilliInstantImplicits {
    implicit def toMilliInstantExt(peer:MilliInstant)	= new MilliInstantExt(peer)
}

final class MilliInstantExt(peer:MilliInstant) {
	def toGregorian(tz:TimeZone):Gregorian	=
			JTimeUtil milliInstantToGregorian (peer, tz)
			
	def toDate:Date	=
			JTimeUtil milliInstantToDate peer
		
	def toISO8601:String	=
			JTimeUtil dateToISO8601 toDate
}
