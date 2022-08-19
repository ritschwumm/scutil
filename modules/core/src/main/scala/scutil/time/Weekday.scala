package scutil.time

import scutil.lang.tc.*
import scutil.math.functions.*

object Weekday {
	val all:Vector[Weekday]	= values.toVector

	val count	= all.size

	def fromIndex(index:Int):Weekday	=
		values(moduloInt(index, count))

	def ordering(first:Weekday):Ordering[Weekday]	=
		Ordering[Int] on { weekday =>
			if (weekday.index >= first.index)	weekday.index
			else								weekday.index + count
		}

	//------------------------------------------------------------------------------


	//------------------------------------------------------------------------------
	//## typeclass instances

	given WeekdayShow:Show[Weekday]	= Show.toStringInstance
}

enum Weekday {
	// NOTE the order is important here
	case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday

	/** starting at monday with 0 */
	def index:Int	= this.ordinal

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
