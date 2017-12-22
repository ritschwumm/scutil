package scutil.time

import scutil.lang.tc._
import scutil.math.functions._

object MonthYear extends MonthYearInstances {
	// TODO smart constructor
	
	def fromValues(month:Month, year:Year):MonthYear	=
			MonthYear(month.index+1, year.value)
		
	def fromIndex(index:Int):MonthYear	=
			MonthYear(
				month	= moduloInt(index, 12) + 1,
				year	= index / 12
			)
}

/** month in 1..12 */
final case class MonthYear(month:Int, year:Int) extends Ordered[MonthYear] {
	require(month >= 1,		s"expected month >= 1, got $month")
	require(month <= 12,	s"expected month <= 12, got $month")
	
	def move(offset:Int):MonthYear	= MonthYear fromIndex (toIndex + offset)
	def until(that:MonthYear):Int	= that.toIndex - this.toIndex
	
	def compare(that:MonthYear):Int		= {
		val	y	= this.year		compare that.year;	if (y != 0)	return y
		val	m	= this.month	compare that.month;	if (m != 0)	return m
		0
	}
	def min(that:MonthYear):MonthYear	= if (this < that) this else that
	def max(that:MonthYear):MonthYear	= if (this > that) this else that
	
	lazy val monthIndex:Int		= month-1
	lazy val monthValue:Month	= Month fromIndex monthIndex
	lazy val yearValue:Year		= Year(year)
	
	def gregorianDateAt(day:Int):GregorianDate	= GregorianDate(day, month, year)
		
	lazy val firstDay:GregorianDate	= gregorianDateAt(1)
	lazy val lastDay:GregorianDate	= gregorianDateAt(days)
	
	lazy val days:Int	= monthValue daysInYear yearValue
	
	lazy val toIndex:Int	= monthIndex + year * 12
	
	override def toString:String	= s"$year-$month"
}

trait MonthYearInstances {
	implicit val MonthYearShow:Show[MonthYear]	= Show.toStringInstance
}
