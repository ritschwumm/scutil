package scutil.log

/** provides a LogHandler for LogLevelWithApply */
trait Logging {
	// NOTE this can be overridden
	implicit def logHandler:LogHandler	= DefaultLogHandler
}
