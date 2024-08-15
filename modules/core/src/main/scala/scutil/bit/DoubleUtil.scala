package scutil.bit

import java.lang.{
	Math	as JMath,
	Double	as JDouble
}

//import scala.annotation.strictfp

object DoubleUtil {
	/*
	@strictfp
	def denormal(it:Double):Boolean	=
		if		(it == 0)												false
		else if	(it > -JDouble.MIN_NORMAL && it < JDouble.MIN_NORMAL)	true
		else															false
	*/

	inline def denormal(it:Double):Boolean	= it != 0d && JMath.getExponent(it) == JDouble.MIN_EXPONENT - 1

	inline def ftz(it:Double):Double	= if (denormal(it))	0d else it

	def bits(it:Double):(Boolean,Long,Short)	= {
		// positive infinity	0x7ff0000000000000L
		// negative infinity	0xfff0000000000000L
		// NaN					0x7ff8000000000000L
		val bits		= JDouble.doubleToLongBits(it)

		// true means negative
		val sign		= (bits & 0x8000000000000000L) != 0

		// doesn't include leading one
		val mantissa	= bits & 0x000fffffffffffffL

		// never negative, bias is 1023; 0 and 2047 have special meanings
		val exponent	= ((bits & 0x7ff0000000000000L) >> 52).toShort

		(sign, mantissa, exponent)
	}
}
