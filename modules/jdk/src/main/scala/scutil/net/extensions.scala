package scutil.net

import scutil.net.extension._

object extensions extends extensions
trait extensions
	extends	InetAddressImplicits
	with	ServerSocketImplicits
	with	SocketImplicits
