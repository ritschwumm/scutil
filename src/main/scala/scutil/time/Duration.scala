package scutil.time

import scala.math.Ordered

import scutil.ext.AnyImplicits._
import scutil.ext.OptionImplicits._

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
	def asHumanString:String	= {
		val parts	=
				Seq(
					(Duration.day,		"d"),
					(Duration.hour,		"h"),
					(Duration.minute,	"m"),
					(Duration.second,	"s"),
					(Duration.milli,	"ms")
				)
		
		val scanned	= 
				(parts scanLeft (millis, 0L, "")) { (remainder_out_xxx:(Long,Long,String), div_unit:(Duration,String)) =>
					val (remainder,out,_)			= remainder_out_xxx
					val (Duration(divider),unit)	= div_unit
					(remainder % divider, remainder / divider, unit)
				}
				
		scanned dropWhile { _._2 == 0 } map { case (_, value, unit) => value + unit } guardBy { _.nonEmpty } cata (
				_ mkString " ",
				"0")
	}
}
