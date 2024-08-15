package scutil.jtime.extension

import java.time.Instant

import scutil.time.*
import scutil.jtime.*

object InstantExtensions {
	extension (peer:Instant) {
		def toIso8601:String			= peer.toString
		def toMilliInstant:MilliInstant	= JTimeUtil.instantToMilliInstant(peer)
	}
}
