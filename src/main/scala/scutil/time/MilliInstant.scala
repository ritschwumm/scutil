package scutil.time

import java.util.Date
import java.util.TimeZone
import java.text.SimpleDateFormat

import scala.math.Ordered

import scutil.lang._

object MilliInstant {
	def zero:MilliInstant	= MilliInstant(0)
	def now:MilliInstant	= MilliInstant(System.currentTimeMillis)
	
	def fromDate(date:Date):MilliInstant	= MilliInstant(date.getTime)
	
	val newType			= Bijection[MilliInstant,Long](_.millis, MilliInstant.apply)
	val dateBijection	= Bijection[MilliInstant,Date](_.toDate, MilliInstant.fromDate)
}

final case class MilliInstant(millis:Long) extends Ordered[MilliInstant] {
	// def + (that:MilliInstant):MilliDuration		= MilliDuration(this.millis + that.millis)
	def - (that:MilliInstant):MilliDuration		= MilliDuration(this.millis - that.millis)
	
	def + (duration:MilliDuration):MilliInstant	= MilliInstant(millis + duration.millis)
	def - (duration:MilliDuration):MilliInstant	= MilliInstant(millis - duration.millis)
	
	def compare(that:MilliInstant):Int			= this.millis compare that.millis
	def min(that:MilliInstant):MilliInstant		= if (this < that) this else that
	def max(that:MilliInstant):MilliInstant		= if (this > that) this else that
	
	def toDate:Date	= new Date(millis)
	
	def toISO8601:String	= {
		val	df	= new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
		df setTimeZone (TimeZone getTimeZone "UTC")
		df format toDate
	}
}
