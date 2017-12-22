package scutil.jtime.pimp

import scutil.time._
import scutil.text._

object MilliDurationImplicits extends MilliDurationImplicits

trait MilliDurationImplicits {
	implicit final class MilliDurationExt(peer:MilliDuration) {
		def toHumanString:String	= Human fullMilliDuration peer.millis
	}
}
