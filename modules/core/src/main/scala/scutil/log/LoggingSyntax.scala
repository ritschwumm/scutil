package scutil.log

import scutil.lang._
import scutil.lang.tc.Show
import scutil.time.MilliInstant
import scutil.jtime.implicits._

trait LoggingSyntax {
	def logHandler:LogHandler

	implicit def StringAsLogValue(it:String):LogValue				= LogValue string		it
	implicit def ThrowableAsLogValue(it:Throwable):LogValue			= LogValue throwable	it
	implicit def MultipleAsLogValue(it:ISeq[LogValue]):LogValue		= LogValue multiple		it

	implicit def ShowAsLogValue[T:Show](it:T):LogValue				= LogValue string (Show doit it)

	implicit def ISeqShowAsLogValue[T](it:ISeq[T])(implicit S:Show[T]):LogValue	= LogValue multiple (it map S.show map LogValue.string)
	implicit def SetShowAsLogValue[T](it:Set[T])(implicit S:Show[T]):LogValue	= ISeqShowAsLogValue(it.toVector)
	implicit def NesShowAsLogValue[T](it:Nes[T])(implicit S:Show[T]):LogValue	= ISeqShowAsLogValue(it.toISeq)

	@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
	implicit class LogLevelAsLogger(level:LogLevel) {
		def apply(elements:LogValue*)(implicit sl:SourceLocation) {
			log(elements.toVector)
		}

		def time[T](what:LogValue*)(block: =>T)(implicit sl:SourceLocation):T	= {
			val (out, dur)	= LogTime measure block
			val elements	= what.toVector :+ (LogValue string dur.toHumanString)
			log(elements)(sl)
			out
		}

		def log(elements:ISeq[LogValue])(implicit sl:SourceLocation) {
			logHandler handle LogEvent(level, elements, MilliInstant.now, sl)
		}
	}
}
