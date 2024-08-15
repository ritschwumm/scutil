package scutil.log

import scutil.lang.*
import scutil.time.MilliInstant

/** provides a LogHandler and syntax to turn LogLevels into logging methods */
trait Logging {
	implicit final class LogLevelAsLogger(level:LogLevel) {
		def apply(elements:LogValue*)(using sl:SourceLocation):Unit = {
			log(elements.toVector)
		}

		def time[T](what:LogValue*)(block: =>T)(using sl:SourceLocation):T	= {
			val (out, dur)	= LogTime.measure(block)
			val elements	= what.toVector :+ LogValue.string(dur.toHumanString)
			log(elements)(using sl)
			out
		}

		def log(elements:Seq[LogValue])(using sl:SourceLocation):Unit = {
			logHandler.handle(LogEvent(level, elements, MilliInstant.now(), sl))
		}
	}

	def logHandler:LogHandler	= DefaultLogHandler
}
