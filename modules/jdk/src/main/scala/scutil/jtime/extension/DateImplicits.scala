package scutil.jtime.extension

import java.time.Instant

import scutil.time._
import scutil.jtime._

object InstantImplicits extends InstantImplicits

trait InstantImplicits {
	implicit final class InstantExt(peer:Instant) {
		def toIso8601:String			= peer.toString
		def toMilliInstant:MilliInstant	= JTimeUtil instantToMilliInstant peer
	}
}
