package scutil.log

import scutil.lang._

sealed trait LogAtom
sealed trait LogCompound

object LogValue {
	def string(it:String):LogValue				= LogString(it)
	def throwable(it:Throwable):LogValue		= LogThrowable(it)
	def multiple(it:ISeq[LogValue]):LogValue	= LogMultiple(it)
	def variable(it:LogValue*):LogValue			= LogMultiple(it.toVector)
}

sealed trait LogValue {
	def atoms:ISeq[LogAtom]	=
			this match {
				case x@LogString(_)		=> Vector(x)
				case x@LogThrowable(_)	=> Vector(x)
				case LogMultiple(x)		=> x flatMap (_.atoms)
			}
}
final case class LogString(value:String)			extends LogValue with LogAtom
final case class LogThrowable(value:Throwable)		extends LogValue with LogAtom
final case class LogMultiple(values:ISeq[LogValue])	extends LogValue with LogCompound