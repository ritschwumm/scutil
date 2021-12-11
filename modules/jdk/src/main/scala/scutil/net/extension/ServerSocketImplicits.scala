package scutil.net.extension

import java.net._

import scutil.time._

object ServerSocketImplicits {
	implicit final class ServerSocketExt(peer:ServerSocket) {
		def setSoTimeoutDuration(timeout:MilliDuration):Unit	=
			peer setSoTimeout timeout.millis.toInt
	}
}
