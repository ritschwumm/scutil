package scutil.platform

import java.util.Locale
import java.io.File
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

/** supported operating systems */
sealed abstract class OperatingSystem

case object OSX		extends OperatingSystem {
	val applicationSupport:File	= Platform.homeDir / "Library" / "Application Support"
	val cache:File				= Platform.homeDir / "Library" / "Cache"
	val logs:File				= Platform.homeDir / "Library" / "Logs"
}

case object Windows	extends	OperatingSystem {
	// NOTE In XP, Roaming was <user>\Application Data and Local was <user>\Local Settings\Application Data
	val localAppData:Option[File]	= Platform env "LOCALAPPDATA"	map (new File(_))
	val appData:Option[File]		= Platform env "APPDATA"		map (new File(_))
}

case object Linux	extends	OperatingSystem {
	def dotFile(name:String):File	= Platform.homeDir / s".$name"
	val dotLocal:File				= Platform.homeDir / ".local"
	val dotLocalShare:File			= dotLocal / "share"
	val dotCache:File				= Platform.homeDir / ".cache"
}
