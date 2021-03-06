package scutil.log

object LogLevel {
	/** in order of increasing severity */
	val	all:Seq[LogLevel]	=
		Vector[LogLevel](
			TRACE,
			DEBUG,
			INFO,
			WARN,
			ERROR,
			FATAL
		)
}

sealed abstract class LogLevel extends Ordered[LogLevel] {
	protected val severity:Int

	def compare(that:LogLevel):Int	= this.severity compare that.severity
	def min(that:LogLevel):LogLevel	= if (this < that) this else that
	def max(that:LogLevel):LogLevel	= if (this > that) this else that
}

/** fine-grained tracing */
case object TRACE extends LogLevel {
	protected val severity	= 0
}

/** information for developers */
case object DEBUG extends LogLevel {
	protected val severity	= 1
}

/** information for users */
case object INFO extends LogLevel {
	protected val severity	= 2
}

/** recoverable error */
case object WARN extends LogLevel {
	protected val severity	= 3
}

/** application may keep running */
case object ERROR extends LogLevel {
	protected val severity	= 4
}

/** application cannot keep running */
case object FATAL extends LogLevel {
	protected val severity	= 5
}
