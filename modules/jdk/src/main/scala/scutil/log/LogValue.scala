package scutil.log

import scutil.lang.*
import scutil.lang.tc.Show

object LogValue {
	given Conversion[String, LogValue]			= LogValue.string
	given Conversion[Throwable, LogValue]		= LogValue.throwable
	given Conversion[Seq[LogValue], LogValue]	= LogValue.multiple

	given [T:Show]	=> Conversion[T, LogValue]	= show

	given [T:Show as S]	=> Conversion[Iterable[T], LogValue]	= it => LogValue.multiple(it.toVector.map(show))
	given [T:Show as S]	=> Conversion[Nes[T], LogValue]			= it => LogValue.multiple(it.toVector.map(show))

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
