package scutil.time

import scutil.math.functions._

object Weekday {
	val count	= 7
	
	val all:Vector[Weekday]	=
			Vector(
				Monday,
				Tuesday,
				Wednesday,
				Thursday,
				Friday,
				Saturday,
				Sunday
			)
	
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
}

sealed abstract class Weekday {
	/** starting at monday with 0 */
	def index:Int	=
			this match {
				case Monday		=> 0
				case Tuesday	=> 1
				case Wednesday	=> 2
				case Thursday	=> 3
				case Friday		=> 4
				case Saturday	=> 5
				case Sunday		=> 6
			}
}
case object Monday		extends Weekday
case object Tuesday		extends Weekday
case object Wednesday	extends Weekday
case object Thursday	extends Weekday
case object Friday		extends Weekday
case object Saturday	extends Weekday
case object Sunday		extends Weekday
