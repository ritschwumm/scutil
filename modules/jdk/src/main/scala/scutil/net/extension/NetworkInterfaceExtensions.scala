package scutil.net.extension

import java.net.NetworkInterface

object NetworkInterfaceExtensions {
	extension(peer:NetworkInterface) {
		def getHardwareAddressOption:Option[Array[Byte]]	=
			Option(peer.getHardwareAddress)

		def getDisplayNameOption:Option[String]	=
			Option(peer.getDisplayName)
	}
}
