package scutil

package object log {
	/** make LogLevel usable to log to an implicit LogHandler as provided by Logging */
	implicit class LogLevelWithApply(level:LogLevel) {
		def apply(elements:Any*)(implicit logHandler:LogHandler, sourceLocation:SourceLocation) {
			logHandler handle LogEvent(level, sourceLocation, elements)
		}
	}
}
