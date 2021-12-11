package scutil.net.extension

import java.net._

import scutil.time._

object SocketImplicits {
	implicit final class SocketExt(peer:Socket) {
		def setSoTimeoutDuration(timeout:MilliDuration):Unit	=
			peer setSoTimeout timeout.millis.toInt
	}
}
