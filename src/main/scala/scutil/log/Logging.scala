package scutil.log

import scutil.lang.SourceLocation

/** provides a LogHandler and turns LogLevels into logging methods */
trait Logging {
	def logHandler:LogHandler	= DefaultLogHandler
	
	implicit class LogLevelAsLogger(level:LogLevel) {
		def apply(elements:Any*)(implicit sl:SourceLocation) {
			logHandler handle LogEvent(level, elements, sl)
		}
		
		def timing[T](what:String)(block: =>T)(implicit sl:SourceLocation):T	= {
			val (out,dur)	= LogTime measure block
			apply(what, dur.asHumanString)
			out
		}
	}
}
