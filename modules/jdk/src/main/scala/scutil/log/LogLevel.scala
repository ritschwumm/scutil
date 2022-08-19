package scutil.log

object LogLevel {
	/** in order of increasing severity */
	val	all:Seq[LogLevel]	= LogLevel.values.toSeq
}

enum LogLevel extends Ordered[LogLevel] {
	/** fine-grained tracing */
	case TRACE

	/** information for developers */
	case DEBUG

	/** information for users */
	case INFO

	/** recoverable error */
	case WARN

	/** application may keep running */
	case ERROR

	/** application cannot keep running */
	case FATAL

	def compare(that:LogLevel):Int	= this.ordinal compare that.ordinal
	def min(that:LogLevel):LogLevel	= if (this < that) this else that
	def max(that:LogLevel):LogLevel	= if (this > that) this else that
}

// necessary because we use log levels to extend them with the actual log functions
export LogLevel.{
	TRACE,
	DEBUG,
	INFO,
	WARN,
	ERROR,
	FATAL,
}
