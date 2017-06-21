package scutil.bit

import java.lang.{
	Float	=> JFloat,
	Double	=> JDouble
}

import scala.annotation.strictfp

object FloatingPointUtil {
	@strictfp
	def denormalFloat(it:Float):Boolean	=
				 if (it == 0)											false
			else if (it > -JFloat.MIN_NORMAL && it < JFloat.MIN_NORMAL)	true
			else														false
			
	@strictfp
	def denormalDouble(it:Double):Boolean	=
				 if (it == 0)												false
			else if (it > -JDouble.MIN_NORMAL && it < JDouble.MIN_NORMAL)	true
			else															false
			
	def bitsOfFloat(it:Float):(Boolean,Int,Short)	= {
		// positive infinity	0x7f800000
		// negative infinity	0xff800000
		// NaN					0x7fc00000
		val bits		= JFloat  floatToIntBits it
		
		// true means negative
		val sign		= (bits & 0x80000000) != 0
		
		// doesn't include leading one
		val mantissa	= bits &  0x007fffff
		
		// never negative, bias is 127; 0 and 255 have special meanings
		val exponent	= ((bits & 0x7f800000) >> 23).toShort
		
		(sign, mantissa, exponent)
	}
	
	def bitsOfDouble(it:Double):(Boolean,Long,Short)	= {
		// positive infinity	0x7ff0000000000000L
		// negative infinity	0xfff0000000000000L
		// NaN					0x7ff8000000000000L
		val bits		= JDouble doubleToLongBits it
		
		// true means negative
		val sign		= (bits & 0x8000000000000000L) != 0
		
		// doesn't include leading one
		val mantissa	= bits & 0x000fffffffffffffL
		
		// never negative, bias is 1023; 0 and 2047 have special meanings
		val exponent	= ((bits & 0x7ff0000000000000L) >> 52).toShort
		
		(sign, mantissa, exponent)
	}
}
