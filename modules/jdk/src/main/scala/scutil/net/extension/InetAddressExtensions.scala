package scutil.net.extension

import java.net.*

object InetAddressExtensions {
	extension (peer:InetAddress) {
		def socketAddress(port:Int):InetSocketAddress	=
			new InetSocketAddress(peer, port)
	}
}
