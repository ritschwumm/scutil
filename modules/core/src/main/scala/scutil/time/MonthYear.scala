package scutil.time

import scutil.lang.tc.*
import scutil.math.functions.*

object MonthYear {
	// TODO time smart constructor

	def fromValues(month:Month, year:Year):MonthYear	=
		MonthYear(month.index+1, year.value)

	def fromIndex(index:Int):MonthYear	=
		MonthYear(
			month	= moduloInt(index, 12) + 1,
			year	= index / 12
		)

	//------------------------------------------------------------------------------
	//## typeclass instances

	given MonthYearShow:Show[MonthYear]	= Show.toStringInstance
}

/** month in 1..12 */
final case class MonthYear(month:Int, year:Int) extends Ordered[MonthYear] {
	require(month >= 1,		s"expected month >= 1, got ${month.toString}")
	require(month <= 12,	s"expected month <= 12, got ${month.toString}")

	def move(offset:Int):MonthYear	= MonthYear.fromIndex(toIndex + offset)
	def until(that:MonthYear):Int	= that.toIndex - this.toIndex

	def compare(that:MonthYear):Int		= {
		val	y	= this.year		compare that.year;	if (y != 0)	return y
		val	m	= this.month	compare that.month;	if (m != 0)	return m
		0
	}
	def min(that:MonthYear):MonthYear	= if (this < that) this else that
	def max(that:MonthYear):MonthYear	= if (this > that) this else that

	lazy val monthIndex:Int		= month-1
	lazy val monthValue:Month	= Month.fromIndex(monthIndex)
	lazy val yearValue:Year		= Year(year)

	// TODO time return an Option for bad day values?
	def gregorianDateAt(day:Int):GregorianDate	= GregorianDate(day, month, year)

	lazy val firstDay:GregorianDate	= gregorianDateAt(1)
	lazy val lastDay:GregorianDate	= gregorianDateAt(days)

	lazy val days:Int	= monthValue.daysInYear(yearValue)

	lazy val toIndex:Int	= monthIndex + year * 12

	override def toString:String	= s"${year.toString}-${month.toString}"
}
