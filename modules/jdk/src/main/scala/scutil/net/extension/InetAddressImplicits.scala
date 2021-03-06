package scutil.net.extension

import java.net._

object InetAddressImplicits extends InetAddressImplicits

trait InetAddressImplicits {
	implicit final class InetAddressExt(peer:InetAddress) {
		def socketAddress(port:Int):InetSocketAddress	=
			new InetSocketAddress(peer, port)
	}
}
