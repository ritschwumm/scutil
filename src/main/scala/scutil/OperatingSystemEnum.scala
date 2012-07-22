package scutil

object OperatingSystemEnum {
	object OperatingSystem {
		val	all	= Set(OSX, Windows, Linux)
		
		def byId(id:String):Option[OperatingSystem]	= all find { _.id == id }
		
		def current:Option[OperatingSystem]	= {
			val	osName	= SystemProperties.os.name.toLowerCase
				 if (osName contains "linux")		Some(Linux)
			else if (osName contains "windows")		Some(Windows)
			else if (osName contains "mac os x")	Some(OSX)
			else									None
		}
	}
	
	/** supported operating systems */
	sealed abstract class OperatingSystem(val id:String)
	case object OSX		extends OperatingSystem("osx")
	case object Windows	extends	OperatingSystem("windows")
	case object Linux	extends	OperatingSystem("linux")
}
