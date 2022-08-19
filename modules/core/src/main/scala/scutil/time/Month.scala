package scutil.time

import scutil.lang.tc.*
import scutil.math.functions.*

object Month {
	val all:Vector[Month]	= values.toVector

	val count	= all.size

	def fromIndex(index:Int):Month	=
		values(moduloInt(index, count))

	def ordering(first:Month):Ordering[Month]	=
		Ordering[Int] on { month =>
			if (month.index >= first.index)	month.index
			else							month.index + count
		}

	//------------------------------------------------------------------------------
	//## typeclass instances

	given MonthShow:Show[Month]	= Show.toStringInstance
}

enum Month {
	// NOTE the order is important here
	case January, February, March, April, May, June, July, August, September, October, November, December

	//------------------------------------------------------------------------------

	def daysInYear(year:Year):Int	=
		days(year.gregorianLeap)

	def days(leapYear:Boolean):Int	=
		this match {
			case January	=> 31
			case February	=> if (leapYear) 29 else 28
			case March		=> 31
			case April		=> 30
			case May		=> 31
			case June		=> 30
			case July		=> 31
			case August		=> 31
			case September	=> 30
			case October	=> 31
			case November	=> 30
			case December	=> 31
		}

	/** starting at january with 0 */
	def index:Int	= this.ordinal

	def monthYearAt(year:Year):MonthYear	=
		MonthYear.fromValues(this, year)

	override def toString:String	=
		this match {
			case January	=> "January"
			case February	=> "February"
			case March		=> "March"
			case April		=> "April"
			case May		=> "May"
			case June		=> "June"
			case July		=> "July"
			case August		=> "August"
			case September	=> "September"
			case October	=> "October"
			case November	=> "November"
			case December	=> "December"
		}
}
