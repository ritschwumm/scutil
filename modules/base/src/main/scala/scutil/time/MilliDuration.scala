package scutil.time

import scala.math.Ordered

import scutil.lang._
import scutil.lang.tc._

object MilliDuration {
	def week:MilliDuration		= day		*! 7
	def day:MilliDuration		= hour		*! 24
	def hour:MilliDuration		= minute	*! 60
	def minute:MilliDuration	= second	*! 60
	def second:MilliDuration	= milli		*! 1000
	def milli:MilliDuration		= MilliDuration(1)
	def zero:MilliDuration		= MilliDuration(0)

	val newType	= Bijection[MilliDuration,Long](_.millis, MilliDuration.apply)

	//------------------------------------------------------------------------------
	//## typeclass instances

	implicit val MilliDurationShow:Show[MilliDuration]	= Show.toStringInstance

	implicit val MilliDuratioMonoid:Monoid[MilliDuration]	=
			Monoid instance (zero, _ + _)
}

final case class MilliDuration(millis:Long) extends Ordered[MilliDuration] {
	def + (that:MilliDuration):MilliDuration	= MilliDuration(this.millis + that.millis)
	def - (that:MilliDuration):MilliDuration	= MilliDuration(this.millis - that.millis)

	def +! (instant:MilliInstant):MilliInstant	= MilliInstant(millis + instant.millis)

	def / (that:MilliDuration):Long		= this.millis / that.millis
	def % (that:MilliDuration):Long		= this.millis % that.millis

	def *! (factor:Long):MilliDuration	= MilliDuration(millis * factor)
	def /! (factor:Long):MilliDuration	= MilliDuration(millis / factor)
	def %! (factor:Long):MilliDuration	= MilliDuration(millis % factor)

	def compare(that:MilliDuration):Int			= this.millis compare that.millis
	def min(that:MilliDuration):MilliDuration	= if (this < that) this else that
	def max(that:MilliDuration):MilliDuration	= if (this > that) this else that

	def micros:Long	= millis*1000
	def nanos:Long	= millis*1000*1000

	override def toString:String	= millis.toString
}
