package scutil.text

import scala.annotation.tailrec

/** format (positive) numbers in a human readable way */
object Human {
	private val zero	= BigDecimal(0)
	private val one		= BigDecimal(1)
	
	//------------------------------------------------------------------------------
	
	/** format with all units, but leave out the biggest zero-valued */
	def full(munit:MUnit, value:BigDecimal):String	=
			decompose(munit, value) map { case (v,l) => v.toString + l } mkString " "
	
	/** value/label pairs starting at the biggest non-zero unit */
	private def decompose(munit:MUnit, value:BigDecimal):List[(BigDecimal,String)]	=
			if (value != zero)	decomposeReverse1(munit, value).reverse dropWhile (_._1 == zero)
			else				List((value, munit.label))

	/** value/label pairs starting at the smallest unit */
	private def decomposeReverse1(munit:MUnit, value:BigDecimal):List[(BigDecimal,String)]	=
			munit cata (
					label => List((value, label)),
					(label, divisor, bigger) => {
						val (div,mod)	= value /% divisor
						(mod, label) :: decomposeReverse1(bigger, div)
					})
			
	//------------------------------------------------------------------------------
	
	/** rounded to the biggest possible unit */
	def rounded(munit:MUnit, commaDigits:Int, value:BigDecimal):String	= {
		val (parts,label)	= biggestNonZero(munit, value)
		val sfmt			= "%." + commaDigits + "f" + label
		val sval			= value / parts
		sfmt format sval
	}
			
	/** returns the MUnit's number of units and label */
	private def biggestNonZero(munit:MUnit, value:BigDecimal, size:BigDecimal = one):(BigDecimal,String)	=
			munit cata (
					label	=> (size, label),
					(label, divisor, bigger) => {
						if (value < divisor)	(size, label)
						else					biggestNonZero(bigger, value / divisor, size * divisor)
					})
					
	//------------------------------------------------------------------------------
			
	sealed trait MUnit {
		def label:String
		def cata[T](last:String=>T, next:(String,BigDecimal,MUnit)=>T):T	=
				this match {
					case LastMUnit(label)					=> last(label)
					case NextMUnit(label, divisor, bigger)	=> next(label, divisor, bigger)
				}
	}
	case class NextMUnit(label:String, divisor:BigDecimal, bigger:MUnit)	extends MUnit
	case class LastMUnit(label:String)										extends MUnit
	
	val binaryTable:MUnit	=
			NextMUnit("", 	1024,
			NextMUnit("k",	1024,
			NextMUnit("M",	1024,
			NextMUnit("G",	1024,
			NextMUnit("T",	1024,
			NextMUnit("P",	1024,
			NextMUnit("E",	1024,
			NextMUnit("Z",	1024,
			LastMUnit("Y")))))))))
		
	val decimalTable:MUnit	=
			NextMUnit("", 	1000,
			NextMUnit("k",	1000,
			NextMUnit("M",	1000,
			NextMUnit("G",	1000,
			NextMUnit("T",	1000,
			NextMUnit("P",	1000,
			NextMUnit("E",	1000,
			NextMUnit("Z",	1000,
			LastMUnit("Y")))))))))
	
	val milliTimeTable:MUnit	=
			NextMUnit("ms",	1000,
			NextMUnit("s",	60,
			NextMUnit("m",	60,
			NextMUnit("h",	24,
			LastMUnit("d")))))
}
