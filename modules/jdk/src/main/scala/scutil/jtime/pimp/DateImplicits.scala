package scutil.jtime.pimp

import java.time.Instant

import scutil.time._
import scutil.jtime._

object InstantImplicits extends InstantImplicits

trait InstantImplicits {
	implicit final class InstantExt(peer:Instant) {
		@deprecated("use toIso8601", "0.182.0")
		def toISO8601:String			= toIso8601
		def toIso8601:String			= peer.toString
		def toMilliInstant:MilliInstant	= JTimeUtil instantToMilliInstant peer
	}
}
