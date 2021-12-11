package scutil.net.extension

import java.net.*

import scutil.time.*

object ServerSocketExtensions {
	implicit final class ServerSocketExt(peer:ServerSocket) {
		def setSoTimeoutDuration(timeout:MilliDuration):Unit	=
			peer setSoTimeout timeout.millis.toInt
	}
}
