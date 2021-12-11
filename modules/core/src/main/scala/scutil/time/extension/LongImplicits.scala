package scutil.time.extension

import scutil.time._

object LongImplicits {
	/** provide conversion from Long to MilliDuration */
	implicit final class TimeLongExt(peer:Long) {
		def weeks:MilliDuration		= MilliDuration.week	 *! peer
		def days:MilliDuration		= MilliDuration.day		 *! peer
		def hours:MilliDuration		= MilliDuration.hour	 *! peer
		def minutes:MilliDuration	= MilliDuration.minute	 *! peer
		def seconds:MilliDuration	= MilliDuration.second	 *! peer
		def millis:MilliDuration	= MilliDuration.milli	 *! peer
	}
}
