package scutil.log

object LogAtom {
	final case class LogString(value:String)			extends LogAtom
	final case class LogThrowable(value:Throwable)		extends LogAtom
}

sealed trait LogAtom
