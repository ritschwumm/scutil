package scutil.time

import scutil.lang.tc._
import scutil.math.functions._

object Weekday {
	val all:Vector[Weekday]	=
		Vector[Weekday](
			Monday,
			Tuesday,
			Wednesday,
			Thursday,
			Friday,
			Saturday,
			Sunday
		)

	val count	= all.size

	def fromIndex(index:Int):Weekday	=
		moduloInt(index, count) match {
			case 0	=> Monday
			case 1	=> Tuesday
			case 2	=> Wednesday
			case 3	=> Thursday
			case 4	=> Friday
			case 5	=> Saturday
			case 6	=> Sunday
		}

	def ordering(first:Weekday):Ordering[Weekday]	=
		Ordering[Int] on { weekday =>
			if (weekday.index >= first.index)	weekday.index
			else								weekday.index + count
		}

	//------------------------------------------------------------------------------

	case object Monday		extends Weekday
	case object Tuesday		extends Weekday
	case object Wednesday	extends Weekday
	case object Thursday	extends Weekday
	case object Friday		extends Weekday
	case object Saturday	extends Weekday
	case object Sunday		extends Weekday

	//------------------------------------------------------------------------------
	//## typeclass instances

	given WeekdayShow:Show[Weekday]	= Show.toStringInstance
}

sealed abstract class Weekday {
	/** starting at monday with 0 */
	def index:Int	=
		this match {
			case Weekday.Monday		=> 0
			case Weekday.Tuesday	=> 1
			case Weekday.Wednesday	=> 2
			case Weekday.Thursday	=> 3
			case Weekday.Friday		=> 4
			case Weekday.Saturday	=> 5
			case Weekday.Sunday		=> 6
		}

	override def toString:String	=
		this match {
			case Weekday.Monday		=> "Monday"
			case Weekday.Tuesday	=> "Tuesday"
			case Weekday.Wednesday	=> "Wednesday"
			case Weekday.Thursday	=> "Thursday"
			case Weekday.Friday		=> "Friday"
			case Weekday.Saturday	=> "Saturday"
			case Weekday.Sunday		=> "Sunday"
		}
}
