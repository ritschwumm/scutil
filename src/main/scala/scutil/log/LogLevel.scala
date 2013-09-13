package scutil.log

import scutil.lang.SourceLocation

object LogLevel {
	/** in order of severity */
	val	all:Seq[LogLevel]	= Vector(
		TRACE,
		DEBUG,
		INFO,
		WARN,
		ERROR,
		FATAL
	)
}

sealed abstract class LogLevel private[log](val name:String, private val severity:Int) extends Ordered[LogLevel] {
	def compare(that:LogLevel):Int	= this.severity compare that.severity
}

/** fine-grained tracing */
case object TRACE	extends LogLevel("TRACE",	0)

/** information for developers */
case object DEBUG	extends LogLevel("DEBUG",	1)

/** information for users */
case object INFO	extends LogLevel("INFO",	2)

/** recoverable error */
case object WARN	extends LogLevel("WARN",	3)

/** application may keep running */
case object ERROR	extends LogLevel("ERROR",	4)

/** application cannot keep running */
case object FATAL	extends LogLevel("FATAL",	5)
