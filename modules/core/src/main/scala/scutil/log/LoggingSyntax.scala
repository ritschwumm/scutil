package scutil.log

import scutil.lang._
import scutil.time.MilliInstant
import scutil.jtime.implicits._

trait LoggingSyntax {
	def logHandler:LogHandler

	@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
	implicit class LogLevelAsLogger(level:LogLevel) {
		def apply(elements:LogValue*)(implicit sl:SourceLocation):Unit = {
			log(elements.toVector)
		}

		def time[T](what:LogValue*)(block: =>T)(implicit sl:SourceLocation):T	= {
			val (out, dur)	= LogTime measure block
			val elements	= what.toVector :+ (LogValue string dur.toHumanString)
			log(elements)(sl)
			out
		}

		def log(elements:Seq[LogValue])(implicit sl:SourceLocation):Unit = {
			logHandler handle LogEvent(level, elements, MilliInstant.now, sl)
		}
	}
}
