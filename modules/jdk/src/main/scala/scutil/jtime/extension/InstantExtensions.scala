package scutil.jtime.extension

import java.time.Instant

import scutil.time.*
import scutil.jtime.*

object InstantExtensions {
	implicit final class InstantExt(peer:Instant) {
		def toIso8601:String			= peer.toString
		def toMilliInstant:MilliInstant	= JTimeUtil instantToMilliInstant peer
	}
}
