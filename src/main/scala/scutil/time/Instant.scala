package scutil.time

import java.util.Date

import scala.math.Ordered

object Instant {
	def zero:Instant	= Instant(0)
	def now:Instant		= Instant(System.currentTimeMillis)
	
	def fromDate(date:Date):Instant	= Instant(date.getTime)
}

case class Instant(millis:Long) extends Ordered[Instant] {
	def + (duration:Duration):Instant	= Instant(millis+duration.millis)
	def - (duration:Duration):Instant	= Instant(millis-duration.millis)
	
	def before(that:Instant):Duration	= Duration(that.millis - this.millis)
	def after(that:Instant):Duration	= Duration(this.millis - that.millis)
	
	def compare(that:Instant):Int		= this.millis compare that.millis
	def min(that:Instant):Instant		= if (this < that) this else that
	def max(that:Instant):Instant		= if (this > that) this else that
	
	def toDate:Date	= new Date(millis)
}
