package scutil.log

import scala.language.implicitConversions

import scutil.lang.*
import scutil.lang.tc.Show

object LogValue {
	implicit def StringAsLogValue(it:String):LogValue				= LogValue string		it
	implicit def ThrowableAsLogValue(it:Throwable):LogValue			= LogValue throwable	it
	implicit def MultipleAsLogValue(it:Seq[LogValue]):LogValue		= LogValue multiple		it

	implicit def ShowAsLogValue[T:Show](it:T):LogValue				= LogValue string (Show doit it)

	implicit def SeqShowAsLogValue[T](it:Seq[T])(using S:Show[T]):LogValue	= LogValue multiple (it map S.show map LogValue.string)
	implicit def SetShowAsLogValue[T](it:Set[T])(using S:Show[T]):LogValue	= SeqShowAsLogValue(it.toVector)
	implicit def NesShowAsLogValue[T](it:Nes[T])(using S:Show[T]):LogValue	= SeqShowAsLogValue(it.toSeq)

	//------------------------------------------------------------------------------

	def string(it:String):LogValue			= LogString(it)
	def throwable(it:Throwable):LogValue	= LogThrowable(it)
	def multiple(it:Seq[LogValue]):LogValue	= LogMultiple(it)
	def variable(it:LogValue*):LogValue		= LogMultiple(it)

	//------------------------------------------------------------------------------

	final case class LogString(value:String)			extends LogValue
	final case class LogThrowable(value:Throwable)		extends LogValue
	final case class LogMultiple(values:Seq[LogValue])	extends LogValue
}

sealed trait LogValue {
	def atoms:Seq[LogAtom]	=
		this match {
			case LogValue.LogString(x)		=> Vector(LogAtom.LogString(x))
			case LogValue.LogThrowable(x)	=> Vector(LogAtom.LogThrowable(x))
			case LogValue.LogMultiple(x)	=> x flatMap (_.atoms)
		}
}
