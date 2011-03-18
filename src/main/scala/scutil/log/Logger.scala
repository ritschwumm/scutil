package scutil.log

trait Logger {
	import LogLevels._
	
	def log(trace:Option[StackTraceElement], level:LogLevel, elements:Seq[Any])
}
