package scutil.log

object LogLevelEnum {
	object LogLevel {
		/** in order of severity */
		val	all:Seq[LogLevel]	= Vector(
			LogTrace,
			LogDebug,
			LogInfo,
			LogWarn,
			LogError,
			LogFatal
		)
	}
	
	sealed abstract class LogLevel private[LogLevelEnum](val name:String, private val severity:Int) extends Ordered[LogLevel] {
		def compare(that:LogLevel):Int	= this.severity compare that.severity
	}
	
	/** fine-grained tracing */
	case object LogTrace	extends LogLevel("TRACE",	0)
	
	/** information for developers */
	case object LogDebug	extends LogLevel("DEBUG",	1)
	
	/** information for users */
	case object LogInfo		extends LogLevel("INFO",	2)
	
	/** recoverable error */
	case object LogWarn		extends LogLevel("WARN",	3)
	
	/** application may keep running */
	case object LogError	extends LogLevel("ERROR",	4)
	
	/** application cannot keep running */
	case object LogFatal	extends LogLevel("FATAL",	5)
}
