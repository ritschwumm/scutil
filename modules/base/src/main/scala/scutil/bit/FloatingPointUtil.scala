package scutil.bit

import java.lang.{
	Math	=> JMath,
	Float	=> JFloat,
	Double	=> JDouble
}

//import scala.annotation.strictfp

object FloatingPointUtil {
	@deprecated("use FloatUtil.denormal", "0.183.0")
	def denormalFloat(it:Float):Boolean		= JMath.getExponent(it) == JFloat.MIN_EXPONENT	- 1
	@deprecated("use DoubleUtil.denormal", "0.183.0")
	def denormalDouble(it:Double):Boolean	= JMath.getExponent(it) == JDouble.MIN_EXPONENT	- 1

	@deprecated("use FloatUtil.ftz", "0.183.0")
	def ftzFloat(it:Float):Float	= if (denormalFloat(it))	0f else it
	@deprecated("use DoubleUtil.ftz", "0.183.0")
	def ftzDouble(it:Double):Double	= if (denormalDouble(it))	0d else it

	@deprecated("use FloatUtil.bits", "0.183.0")
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

	@deprecated("use DoubleUtil.bits", "0.183.0")
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
