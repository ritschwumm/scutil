package scutil.time

import scutil.lang.tc._
import scutil.math.functions._

object Month extends MonthInstances {
	val all:Vector[Month]	=
			Vector(
				January,
				February,
				March,
				April,
				May,
				June,
				July,
				August,
				September,
				October,
				November,
				December
			)

	val count	= all.size

	def fromIndex(index:Int):Month	=
			moduloInt(index, count) match {
				case 0	=> January
				case 1	=> February
				case 2	=> March
				case 3	=> April
				case 4	=> May
				case 5	=> June
				case 6	=> July
				case 7	=> August
				case 8	=> September
				case 9	=> October
				case 10	=> November
				case 11	=> December
			}

	def ordering(first:Month):Ordering[Month]	=
			Ordering[Int] on { month =>
				if (month.index >= first.index)	month.index
				else							month.index + count
			}

	//------------------------------------------------------------------------------

	case object January		extends Month
	case object February	extends Month
	case object March		extends Month
	case object April		extends Month
	case object May			extends Month
	case object June		extends Month
	case object July		extends Month
	case object August		extends Month
	case object September	extends Month
	case object October		extends Month
	case object November	extends Month
	case object December	extends Month
}

sealed abstract class Month {
	def daysInYear(year:Year):Int	=
			days(year.gregorianLeap)

	def days(leapYear:Boolean):Int	=
			this match {
				case Month.January		=> 31
				case Month.February		=> if (leapYear) 29 else 28
				case Month.March		=> 31
				case Month.April		=> 30
				case Month.May			=> 31
				case Month.June			=> 30
				case Month.July			=> 31
				case Month.August		=> 31
				case Month.September	=> 30
				case Month.October		=> 31
				case Month.November		=> 30
				case Month.December		=> 31
			}

	/** starting at january with 0 */
	def index:Int	=
			this match {
				case Month.January		=> 0
				case Month.February		=> 1
				case Month.March		=> 2
				case Month.April		=> 3
				case Month.May			=> 4
				case Month.June			=> 5
				case Month.July			=> 6
				case Month.August		=> 7
				case Month.September	=> 8
				case Month.October		=> 9
				case Month.November		=> 10
				case Month.December		=> 11
			}

	def monthYearAt(year:Year):MonthYear	= MonthYear fromValues (this, year)

	override def toString:String	=
			this match {
				case Month.January		=> "January"
				case Month.February		=> "February"
				case Month.March		=> "March"
				case Month.April		=> "April"
				case Month.May			=> "May"
				case Month.June			=> "June"
				case Month.July			=> "July"
				case Month.August		=> "August"
				case Month.September	=> "September"
				case Month.October		=> "October"
				case Month.November		=> "November"
				case Month.December		=> "December"
			}
}


trait MonthInstances {
	implicit val MonthShow:Show[Month]	= Show.toStringInstance
}
