package scutil

package object time {
	implicit class LongWithMilliDuration(peer:Long) {
		def weeks:MilliDuration		= MilliDuration.week	 *! peer 
		def days:MilliDuration		= MilliDuration.day		 *! peer 
		def hours:MilliDuration		= MilliDuration.hour	 *! peer 
		def minutes:MilliDuration	= MilliDuration.minute	 *! peer 
		def seconds:MilliDuration	= MilliDuration.second	 *! peer 
		def millis:MilliDuration	= MilliDuration.milli	 *! peer 
	}
}
