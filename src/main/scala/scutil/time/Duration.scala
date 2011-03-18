package scutil.time

import scala.math.Ordered

object Duration {
	def week:Duration		= day		* 7
	def day:Duration		= hour		* 24
	def hour:Duration		= minute	* 60
	def minute:Duration		= second	* 60
	def second:Duration		= milli		* 1000
	def milli:Duration		= Duration(1)
	def zero:Duration		= Duration(0)
}

case class Duration(millis:Long) extends Ordered[Duration] {
	def + (that:Duration):Duration	= Duration(this.millis + that.millis)
	def - (that:Duration):Duration	= Duration(this.millis - that.millis)
	
	def * (factor:Long):Duration	= Duration(millis * factor)
	def / (factor:Long):Duration	= Duration(millis / factor)
	
	def compare(that:Duration):Int	= this.millis compare that.millis
}
