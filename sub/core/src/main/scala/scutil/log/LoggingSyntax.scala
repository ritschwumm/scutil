package scutil.log

import scutil.lang.SourceLocation
import scutil.time.MilliInstant
import scutil.collection.implicits._

trait LoggingSyntax {
	def logHandler:LogHandler
	
	implicit class LogLevelAsLogger(level:LogLevel) {
		def apply(elements:Any*)(implicit sl:SourceLocation) {
			logHandler handle LogEvent(level, (elements:Traversable[Any]).toISeq, MilliInstant.now, sl)
		}
		
		def time[T](what:String)(block: =>T)(implicit sl:SourceLocation):T	= {
			val (out, dur)	= LogTime measure block
			apply(what, dur.toHumanString)
			out
		}
	}
}
