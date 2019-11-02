package scutil.time

import scutil.lang.tc._
import scutil.math.functions._

object JulianDay extends JulianDayInstances {
	val epoch	= GregorianDate.epoch.toJulianDay
}

final case class JulianDay(value:Int) extends Ordered[JulianDay] {
	def move(offset:Int):JulianDay	= JulianDay(value + offset)
	def until(that:JulianDay):Int	= that.value - this.value

	def compare(that:JulianDay):Int		= this.value compare that.value
	def min(that:JulianDay):JulianDay	= if (this < that) this else that
	def max(that:JulianDay):JulianDay	= if (this > that) this else that

	lazy val toGregorianDate:GregorianDate	= {
		val J	= value

		val j	= 1401
		val B	= 274277
		val c	= -38
		val r	= 4
		val v	= 3
		val p	= 1461
		val u	= 5
		val w	= 2
		val s	= 153
		val m	= 2
		val n	= 12
		val y	= 4716

		val f	= J + j + (((4 * J + B) / 146097) * 3) / 4 + c
		val e	= r * f + v
		val g	= moduloLong(e, p) / r
		val h	= u * g + w
		val D	= moduloLong(h, s) / u + 1
		val M	= moduloLong(h / s + m, n) + 1
		val Y	= (e / p) - y + (n + m - M) / n

		GregorianDate(D.toInt, M.toInt, Y.toInt)
	}

	def calendarWeek:CalendarWeek	=
			toGregorianDate.calendarWeek

	def monthYear:MonthYear	=
			toGregorianDate.monthYear

	lazy val weekday:Weekday	=
			Weekday fromIndex value

	override def toString:String	= s"JD${value.toString}"
}

trait JulianDayInstances {
	implicit val JulianDayShow:Show[JulianDay]	= Show.toStringInstance
}
