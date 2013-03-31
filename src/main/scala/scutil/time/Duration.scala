package scutil.time

import scala.math.Ordered

import scutil.lang._
import scutil.Human

object Duration {
	def week:Duration		= day		* 7
	def day:Duration		= hour		* 24
	def hour:Duration		= minute	* 60
	def minute:Duration		= second	* 60
	def second:Duration		= milli		* 1000
	def milli:Duration		= Duration(1)
	def zero:Duration		= Duration(0)
	
	val newType	= Bijection[Duration,Long](_.millis, Duration.apply)
}

case class Duration(millis:Long) extends Ordered[Duration] {
	def + (that:Duration):Duration	= Duration(this.millis + that.millis)
	def - (that:Duration):Duration	= Duration(this.millis - that.millis)
	
	def + (instant:Instant):Instant	= Instant(millis + instant.millis)
	// def - (instant:Instant):Instant	= Instant(millis - instant.millis)
	
	def / (that:Duration):Long		= this.millis / that.millis
	def % (that:Duration):Long		= this.millis % that.millis

	def * (factor:Long):Duration	= Duration(millis * factor)
	def / (factor:Long):Duration	= Duration(millis / factor)
	def % (factor:Long):Duration	= Duration(millis % factor)
	
	def compare(that:Duration):Int	= this.millis compare that.millis
	def min(that:Duration):Duration	= if (this < that) this else that
	def max(that:Duration):Duration	= if (this > that) this else that
	
	// @see Human
	def asHumanString:String	=
			Human full (Human.milliTimeTable, millis)
}
