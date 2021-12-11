package scutil.log

import scutil.time.*

object LogTime {
	/** measure how much time a calculation took */
	def measure[T](block: =>T):(T, MilliDuration)	= {
		val	before	= MilliInstant.now()
		val out		= block
		val after	= MilliInstant.now()
		(out, after - before)
	}
}
