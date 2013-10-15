package scutil.text

import scala.annotation.tailrec

/** format (positive) numbers in a human readable way */
object Human {
	private val zero	= BigDecimal(0)
	private val one		= BigDecimal(1)
	
	//------------------------------------------------------------------------------
	//## convenience functions
	
	def fullBinary(value:BigDecimal):String	=
			full(table.binary, value)
		
	def fullDecimal(value:BigDecimal):String	=
			full(table.decimal, value)
	
	def fullMilliTime(value:BigDecimal):String	=
			full(table.milliTime, value)
		
	def roundedBinary(commaDigits:Int, value:BigDecimal):String	=
			rounded(table.binary, commaDigits, value)
		
	def roundedDecimal(commaDigits:Int, value:BigDecimal):String	=
			rounded(table.decimal, commaDigits, value)
		
	def roundedMilliTime(commaDigits:Int, value:BigDecimal):String	=
			rounded(table.milliTime, commaDigits, value)
		
	//------------------------------------------------------------------------------
	//## full unit sequence
	
	/** format with all units, but leave out the biggest zero-valued */
	def full(ulist:UnitList, value:BigDecimal):String	=
			decompose(ulist, value) map { case (v,l) => v.toString + l } mkString " "
	
	/** value/label pairs starting at the biggest non-zero unit */
	private def decompose(ulist:UnitList, value:BigDecimal):List[(BigDecimal,String)]	=
			if (value != zero)	decomposeReverse1(ulist, value).reverse dropWhile (_._1 == zero)
			else				List((value, ulist.label))

	/** value/label pairs starting at the smallest unit */
	private def decomposeReverse1(ulist:UnitList, value:BigDecimal):List[(BigDecimal,String)]	=
			ulist match {
				case LastUnit(label)	=>
					List((value, label))
				case NextUnit(label, divisor, bigger)	=>
					val (div, mod)	= value /% divisor
					(mod, label) :: decomposeReverse1(bigger, div)
			}
			
	//------------------------------------------------------------------------------
	//## rounded to top-level unit
	
	/** rounded to the biggest possible unit */
	def rounded(ulist:UnitList, commaDigits:Int, value:BigDecimal):String	= {
		val (parts, label)	= biggestNonZero(ulist, value)
		s"%.${commaDigits}f${label}" format (value / parts)
	}
			
	/** returns the UnitList's number of units and label */
	@tailrec
	private def biggestNonZero(ulist:UnitList, value:BigDecimal, size:BigDecimal = one):(BigDecimal,String)	=
			ulist match {
				case LastUnit(label)	=> 
					(size, label)
				case NextUnit(label, divisor, bigger)	=>
					if (value < divisor)	(size, label)
					else					biggestNonZero(bigger, value / divisor, size * divisor)
			}
					
	//------------------------------------------------------------------------------
	//## list of units and divisors
	
	sealed trait UnitList {
		def label:String
	}
	case class NextUnit(label:String, divisor:BigDecimal, bigger:UnitList)	extends UnitList
	case class LastUnit(label:String)										extends UnitList
	
	//------------------------------------------------------------------------------
	//## builder syntax
	
	object syntax {
		val unit	= new {
			def of(name:String):LastUnit	= LastUnit(name)
		}
		
		implicit class UnitListExt(ulist:UnitList) {
			def has(divisor:BigDecimal)	= new {
				def of(name:String):UnitList	= NextUnit(name, divisor, ulist)
			}
		}
	}
	
	//------------------------------------------------------------------------------
	//## predefined tables
	
	object table {
		import syntax._
		
		val binary:UnitList	=
				unit	of "Y"	has 
				1024	of "Z"	has
				1024	of "E"	has
				1024	of "P"	has
				1024	of "T"	has
				1024	of "G"	has
				1024	of "M"	has
				1024	of "k"	has
				1024	of ""
			
		val decimal:UnitList	=
				unit	of "Y"	has 
				1000	of "Z"	has
				1000	of "E"	has
				1000	of "P"	has
				1000	of "T"	has
				1000	of "G"	has
				1000	of "M"	has
				1000	of "k"	has
				1000	of ""
		
		val milliTime:UnitList	=
				unit	of "d"	has 
				24		of "h"	has
				60		of "m"	has
				60		of "s"	has
				1000	of "ms"
	}
}
