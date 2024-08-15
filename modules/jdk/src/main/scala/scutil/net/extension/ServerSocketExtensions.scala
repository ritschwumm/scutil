package scutil.net.extension

import java.net.*

import scutil.time.*

object ServerSocketExtensions {
	extension (peer:ServerSocket) {
		def setSoTimeoutDuration(timeout:MilliDuration):Unit	=
			peer.setSoTimeout(timeout.millis.toInt)
	}
}
