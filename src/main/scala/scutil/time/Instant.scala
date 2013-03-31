package scutil.time

import java.util.Date
import java.util.TimeZone
import java.text.SimpleDateFormat

import scala.math.Ordered

import scutil.lang._

object Instant {
	def zero:Instant	= Instant(0)
	def now:Instant		= Instant(System.currentTimeMillis)
	
	def fromDate(date:Date):Instant	= Instant(date.getTime)
	
	val newType	= Bijection[Instant,Long](_.millis, Instant.apply)
}

case class Instant(millis:Long) extends Ordered[Instant] {
	// def + (that:Instant):Duration		= Duration(this.millis + that.millis)
	def - (that:Instant):Duration		= Duration(this.millis - that.millis)
	
	def + (duration:Duration):Instant	= Instant(millis + duration.millis)
	def - (duration:Duration):Instant	= Instant(millis - duration.millis)
	
	def compare(that:Instant):Int		= this.millis compare that.millis
	def min(that:Instant):Instant		= if (this < that) this else that
	def max(that:Instant):Instant		= if (this > that) this else that
	
	def toDate:Date	= new Date(millis)
	
	def toISO8601:String	= {
		val	df	= new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
		df setTimeZone (TimeZone getTimeZone "UTC")
		df format toDate
	}
}
