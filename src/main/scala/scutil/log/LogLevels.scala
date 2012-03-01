package scutil.log

object LogLevels {
	sealed abstract class LogLevel(val name:String, private val priority:Int) extends Ordered[LogLevel] {
		def compare(that:LogLevel):Int	= this.priority compare that.priority
	}
	
	/** application cannot keep running */
	case object LogFatal	extends LogLevel("FATAL",	5)
	/** application may keep running */
	case object LogError	extends LogLevel("ERROR",	4)
	/** recoverable error */
	case object LogWarn		extends LogLevel("WARN",	3)
	/** information for users */
	case object LogInfo		extends LogLevel("INFO",	2)
	/** information for developers */
	case object LogDebug	extends LogLevel("DEBUG",	1)
	/** fine-grained tracing */
	case object LogTrace	extends LogLevel("TRACE",	0)
}
