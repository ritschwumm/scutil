package scutil.bit

import java.lang.{
	Math	as JMath,
	Float	as JFloat,
}

//import scala.annotation.strictfp

object FloatUtil {
	/*
	@strictfp
	def denormal(it:Float):Boolean	=
		if		(it == 0)											false
		else if	(it > -JFloat.MIN_NORMAL && it < JFloat.MIN_NORMAL)	true
		else														false
	*/

	inline def denormal(it:Float):Boolean	= it != 0f && JMath.getExponent(it) == JFloat.MIN_EXPONENT - 1

	inline def ftz(it:Float):Float	= if (denormal(it))	0f else it

	def bits(it:Float):(Boolean,Int,Short)	= {
		// positive infinity	0x7f800000
		// negative infinity	0xff800000
		// NaN					0x7fc00000
		val bits		= JFloat.floatToIntBits(it)

		// true means negative
		val sign		= (bits & 0x80000000) != 0

		// doesn't include leading one
		val mantissa	= bits &  0x007fffff

		// never negative, bias is 127; 0 and 255 have special meanings
		val exponent	= ((bits & 0x7f800000) >> 23).toShort

		(sign, mantissa, exponent)
	}
}
