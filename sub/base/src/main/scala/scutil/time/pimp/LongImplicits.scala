package scutil.time.pimp

import scutil.time._

object LongImplicits extends LongImplicits

trait LongImplicits {
    implicit def toTimeLongExt(peer:Long)	= new LongExt(peer)
}

/** provide conversion from Long to MilliDuration */
final class LongExt(peer:Long) {
	def weeks:MilliDuration		= MilliDuration.week	 *! peer
	def days:MilliDuration		= MilliDuration.day		 *! peer
	def hours:MilliDuration		= MilliDuration.hour	 *! peer
	def minutes:MilliDuration	= MilliDuration.minute	 *! peer
	def seconds:MilliDuration	= MilliDuration.second	 *! peer
	def millis:MilliDuration	= MilliDuration.milli	 *! peer
}
