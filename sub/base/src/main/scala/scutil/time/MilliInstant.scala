package scutil.time

import scala.math.Ordered

import scutil.lang._

object MilliInstant {
	def zero:MilliInstant	= MilliInstant(0)
	def now:MilliInstant	= MilliInstant(System.currentTimeMillis)
	
	val newType	= Bijection[MilliInstant,Long](_.millis, MilliInstant.apply)
}

final case class MilliInstant(millis:Long) extends Ordered[MilliInstant] {
	def - (that:MilliInstant):MilliDuration	= MilliDuration(this.millis - that.millis)
	
	def +! (duration:MilliDuration):MilliInstant	= MilliInstant(millis + duration.millis)
	def -! (duration:MilliDuration):MilliInstant	= MilliInstant(millis - duration.millis)
	
	def compare(that:MilliInstant):Int		= this.millis compare that.millis
	def min(that:MilliInstant):MilliInstant	= if (this < that) this else that
	def max(that:MilliInstant):MilliInstant	= if (this > that) this else that
}
