package scutil.log

object LogLevel {
	/** in order of increasing severity */
	val	all:Seq[LogLevel]	=
			Vector(
				TRACE,
				DEBUG,
				INFO,
				WARN,
				ERROR,
				FATAL
			)
}

sealed abstract class LogLevel extends Ordered[LogLevel] {
	val name:String
	protected val severity:Int 
	
	def compare(that:LogLevel):Int	= 
			this.severity compare that.severity
}

/** fine-grained tracing */
case object TRACE extends LogLevel {
	val name	= "TRACE"
	protected val severity	= 0
}

/** information for developers */
case object DEBUG extends LogLevel {
	val name	= "DEBUG"
	protected val severity	= 1
}

/** information for users */
case object INFO extends LogLevel {
	val name	= "INFO"
	protected val severity	= 2
}

/** recoverable error */
case object WARN extends LogLevel {
	val name	= "WARN"
	protected val severity	= 3
}

/** application may keep running */
case object ERROR extends LogLevel {
	val name	= "ERROR"
	protected val severity	= 4
}

/** application cannot keep running */
case object FATAL extends LogLevel {
	val name	= "FATAL"
	protected val severity	= 5
}
