package scutil.log

import scutil.lang._
import scutil.lang.tc.Show
import scutil.time.MilliInstant
import scutil.jtime.implicits._

trait LoggingSyntax {
	def logHandler:LogHandler
	
	implicit def StringAsLogValue(it:String):LogValue		= LogString(it)
	implicit def ThrowableAsLogValue(it:Throwable):LogValue	= LogThrowable(it)
	implicit def ShowAsLogValue[T:Show](it:T):LogValue		= LogString(Show doit it)
	
	implicit class LogLevelAsLogger(level:LogLevel) {
		def apply(elements:LogValue*)(implicit sl:SourceLocation) {
			log(elements.toVector)
		}
		
		def time[T](what:LogValue*)(block: =>T)(implicit sl:SourceLocation):T	= {
			val (out, dur)	= LogTime measure block
			val elements	= what.toVector :+ LogString(dur.toHumanString)
			log(elements)(sl)
			out
		}
		
		def log(elements:ISeq[LogValue])(implicit sl:SourceLocation) {
			logHandler handle LogEvent(level, elements, MilliInstant.now, sl)
		}
	}
}
