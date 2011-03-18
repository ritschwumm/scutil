package scutil.time

import scala.math.Ordered

object Instant {
	def zero:Instant	= Instant(0)
	def now:Instant		= Instant(System.currentTimeMillis)
}

case class Instant(millis:Long) extends Ordered[Instant] {
	def + (duration:Duration):Instant	= Instant(millis+duration.millis)
	def - (duration:Duration):Instant	= Instant(millis-duration.millis)
	
	def compare(that:Instant):Int		= this.millis compare that.millis
	
	def until(that:Instant):Duration	= Duration(that.millis - this.millis)
	// def - (that:Instant):Duration	= Duration(this.millis - that.millis)
	// def + (that:Instant):Duration	= Duration(this.millis + that.millis)
}
