package scutil.net.extension

import java.net.*

object InetAddressExtensions {
	implicit final class InetAddressExt(peer:InetAddress) {
		def socketAddress(port:Int):InetSocketAddress	=
			new InetSocketAddress(peer, port)
	}
}
