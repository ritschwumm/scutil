package scutil.time

import scutil.lang.tc._
import scutil.math.functions._

object Month extends MonthInstances {
	val count	= 12
	
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
}

sealed abstract class Month {
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
			
	/** starting at monday with 0 */
	def index:Int	=
			this match {
				case January	=> 0
				case February	=> 1
				case March		=> 2
				case April		=> 3
				case May		=> 4
				case June		=> 5
				case July		=> 6
				case August		=> 7
				case September	=> 8
				case October	=> 9
				case November	=> 10
				case December	=> 11
			}
			
	def monthYearAt(year:Year):MonthYear	= MonthYear fromValues (this, year)
	
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

trait MonthInstances {
	implicit val MonthShow:Show[Month]	= Show.toStringInstance
}

