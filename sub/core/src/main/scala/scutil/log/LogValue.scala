package scutil.log

import scutil.lang._

object LogValue {
	object P {
		val LogString:Prism[LogValue,String]		= Prism partial ({ case scutil.log.LogString(x)		=> x }, LogString.apply)
		val LogThrowable:Prism[LogValue,Throwable]	= Prism partial ({ case scutil.log.LogThrowable(x)	=> x }, LogThrowable.apply)
	}
}
sealed trait LogValue
final case class LogString(value:String)		extends LogValue
final case class LogThrowable(value:Throwable)	extends LogValue
