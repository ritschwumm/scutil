package scutil.log

/** provides a LogHandler and syntax to turn LogLevels into logging methods */
trait Logging extends LoggingSyntax {
	def logHandler:LogHandler	= DefaultLogHandler
}
