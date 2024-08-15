package scutil.net.extension

import java.net.*

import scutil.time.*

object SocketExtensions {
	implicit final class SocketExt(peer:Socket) {
		def setSoTimeoutDuration(timeout:MilliDuration):Unit	=
			peer.setSoTimeout(timeout.millis.toInt)
	}
}
