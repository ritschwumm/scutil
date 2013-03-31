package scutil.os

import scutil.SystemProperties

object OperatingSystem {
	val	all:Set[OperatingSystem]	= Set(OSX, Windows, Linux)
	
	def current:Option[OperatingSystem]	= {
		val	osName	= SystemProperties.os.name.toLowerCase
			 if (osName contains "linux")		Some(Linux)
		else if (osName contains "windows")		Some(Windows)
		else if (osName contains "mac os x")	Some(OSX)
		else									None
	}
}

/** supported operating systems */
sealed abstract class OperatingSystem
case object OSX		extends OperatingSystem
case object Windows	extends	OperatingSystem
case object Linux	extends	OperatingSystem
