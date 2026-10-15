package scutil.log

import scala.language.implicitConversions

import scutil.lang.*
import scutil.lang.tc.Show

object LogValue {
	implicit def StringAsLogValue(it:String):LogValue			= string(it)
	implicit def ThrowableAsLogValue(it:Throwable):LogValue		= throwable(it)
	implicit def MultipleAsLogValue(it:Seq[LogValue]):LogValue	= multiple(it)

	implicit def ShowAsLogValue[T:Show](it:T):LogValue			= show(it)

	implicit def IterableShowAsLogValue[T:Show](it:Iterable[T]):LogValue	= LogValue.multiple(it.toVector.map(show))
	implicit def NesShowAsLogValue[T:Show](it:Nes[T]):LogValue				= IterableShowAsLogValue(it.toSeq)

	//------------------------------------------------------------------------------

	def string(it:String):LogValue			= LogString(it)
	def throwable(it:Throwable):LogValue	= LogThrowable(it)
	def multiple(it:Seq[LogValue]):LogValue	= LogMultiple(it)
	def variable(it:LogValue*):LogValue		= LogMultiple(it)
	def show[T:Show as S](it:T):LogValue	= LogString(S.show(it))
}

enum LogValue {
	case LogString(value:String)
	case LogThrowable(value:Throwable)
	case LogMultiple(values:Seq[LogValue])

	def atoms:Seq[LogAtom]	=
		this match {
			case LogValue.LogString(x)		=> Vector(LogAtom.LogString(x))
			case LogValue.LogThrowable(x)	=> Vector(LogAtom.LogThrowable(x))
			case LogValue.LogMultiple(x)	=> x.flatMap(_.atoms)
		}
}
