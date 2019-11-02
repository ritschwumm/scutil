package scutil.log

import scutil.lang._
import scutil.lang.tc.Show
import scutil.time.MilliInstant
import scutil.jtime.implicits._

trait LoggingSyntax {
	def logHandler:LogHandler

	implicit def StringAsLogValue(it:String):LogValue				= LogValue string		it
	implicit def ThrowableAsLogValue(it:Throwable):LogValue			= LogValue throwable	it
	implicit def MultipleAsLogValue(it:Seq[LogValue]):LogValue		= LogValue multiple		it

	implicit def ShowAsLogValue[T:Show](it:T):LogValue				= LogValue string (Show doit it)

	implicit def SeqShowAsLogValue[T](it:Seq[T])(implicit S:Show[T]):LogValue	= LogValue multiple (it map S.show map LogValue.string)
	implicit def SetShowAsLogValue[T](it:Set[T])(implicit S:Show[T]):LogValue	= SeqShowAsLogValue(it.toVector)
	implicit def NesShowAsLogValue[T](it:Nes[T])(implicit S:Show[T]):LogValue	= SeqShowAsLogValue(it.toSeq)

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
