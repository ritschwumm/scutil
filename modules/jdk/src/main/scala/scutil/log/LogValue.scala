package scutil.log

import scala.Conversion.into

import scutil.lang.*
import scutil.lang.tc.Show

object LogValue {
	def of(value:into[LogValue]):LogValue		= value
	def ofN(values:into[LogValue]*):LogValue	= of(values)

	//-----------------------------------------------------------------------------

	given Conversion[String, LogValue]			= LogString(_)
	given Conversion[Throwable, LogValue]		= LogThrowable(_)
	given Conversion[Seq[LogValue], LogValue]	= LogMultiple(_)

	given [T:Show]	=> Conversion[T, LogValue]				= it => show(it)
	given [T:Show]	=> Conversion[Iterable[T], LogValue]	= it => LogMultiple(it.toVector.map(show))
	given [T:Show]	=> Conversion[Nes[T], LogValue]			= it => LogMultiple(it.toVector.map(show))

	private def show[T:Show as S](it:T):LogValue	= LogString(S.show(it))
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
