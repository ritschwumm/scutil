package scutil.log

object LogLevels {
	sealed abstract class LogLevel(val name:String)
	case object ERROR	extends LogLevel("ERROR")
	case object WARN	extends LogLevel("WARN")
	case object INFO	extends LogLevel("INFO")
	case object DEBUG	extends LogLevel("DEBUG")
}
