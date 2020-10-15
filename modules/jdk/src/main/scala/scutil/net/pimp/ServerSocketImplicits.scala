package scutil.net.pimp

import java.net._

import scutil.time._

object ServerSocketImplicits extends ServerSocketImplicits

trait ServerSocketImplicits {
	implicit final class ServerSocketExt(peer:ServerSocket) {
		def setSoTimeoutDuration(timeout:MilliDuration):Unit	=
			peer setSoTimeout timeout.millis.toInt
	}
}
