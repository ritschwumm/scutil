package scutil.jtime.pimp

import java.util._
import java.time.Instant

import scutil.time._
import scutil.jtime._

object MilliInstantImplicits extends MilliInstantImplicits

trait MilliInstantImplicits {
	implicit final class MilliInstantExt(peer:MilliInstant) {
		def toGregorianDate(tz:TimeZone):GregorianDate	=
				JTimeUtil milliInstantToGregorianDate (peer, tz)
				
		def toDate:Date	=
				JTimeUtil milliInstantToDate peer
			
		def toInstant:Instant	=
				JTimeUtil milliInstantToInstant peer
			
		def toISO8601:String	=
				JTimeUtil dateToISO8601 toDate
	}
}
