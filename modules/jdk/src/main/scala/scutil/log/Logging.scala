package scutil.log

import scala.Conversion.into

import scutil.lang.*
import scutil.time.MilliInstant

/** provides a LogHandler and syntax to turn LogLevels into logging methods */
trait Logging {
	extension (level:LogLevel) {
		def apply(elements:into[LogValue]*)(using sl:SourceLocation):Unit = {
			log(elements.toVector)
		}

		def time[T](what:into[LogValue]*)(block: =>T)(using sl:SourceLocation):T	= {
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
