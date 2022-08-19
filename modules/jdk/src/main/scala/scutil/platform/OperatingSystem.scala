package scutil.platform

import java.util.Locale
import scutil.jdk.implicits.*

object OperatingSystem {
	val	all:Set[OperatingSystem]	= Set(OSX, Windows, Linux)

	def current:Option[OperatingSystem]	= {
		val	osName	= SystemProperties.os.name.toLowerCase(Locale.US)
			 if (osName contains "linux")		Some(Linux)
		else if (osName contains "windows")		Some(Windows)
		else if (osName contains "mac os x")	Some(OSX)
		else									None
	}
}

enum OperatingSystem {
	case Linux
	case Windows
	case OSX
}
