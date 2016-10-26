package scutil

import java.lang.{ Long => JLong, Float => JFloat, Double => JDouble }

import scala.annotation.strictfp

package object bit {
	def nextPow2(it:Long):Long	=
				 if (it == 0)	0
			else if (it == 1)	1
			else 				(JLong highestOneBit (it-1)) << 1
			
	//------------------------------------------------------------------------------

	def unsignedByte(value:Byte):Short	= (value & 0x000000ff).toShort
	def unsignedShort(value:Short):Int	= (value & 0x0000ffff)
	def unsignedInt(value:Int):Long		= (value & 0xffffffffL)
	def unsignedLong(value:Long):BigInt	= {
		val	tmp	= BigInt(value)
		if (tmp >= 0)	tmp
		else			BigInt(Long.MaxValue)*2	- tmp
	}
	
	//------------------------------------------------------------------------------
	
	def swapEndianShort(value:Short):Short	=
			(
				((value << 8) & 0xff00) |
				((value >> 8) & 0x00ff)
			).toShort
			
	def swapEndianInt(value:Int):Int	=
			((value << 24) & 0xff000000)	|
			((value >> 24) & 0x000000ff)	|
			((value <<  8) & 0x00ff0000)	|
			((value >>  8) & 0x0000ff00)
			
	def swapEndianLong(value:Long):Long	=
			((value << 56) & 0xff00000000000000L)	|
			((value >> 56) & 0x00000000000000ffL)	|
			((value << 40) & 0x00ff000000000000L)	|
			((value >> 40) & 0x000000000000ff00L)	|
			((value << 24) & 0x0000ff0000000000L)	|
			((value >> 24) & 0x0000000000ff0000L)	|
			((value <<  8) & 0x000000ff00000000L)	|
			((value >>  8) & 0x00000000ff000000L)
			
	def swapEndianChar(value:Char):Char	=
			(
				((value << 8) & 0xff00) |
				((value >> 8) & 0x00ff)
			).toChar
	
	//------------------------------------------------------------------------------
	
	def maskTestByte(value:Byte, onMask:Byte, offMask:Byte):Boolean =
			(value & (onMask | offMask)) == onMask
		
	def maskTestShort(value:Short, onMask:Short, offMask:Short):Boolean =
			(value & (onMask | offMask)) == onMask
		
	def maskTestInt(value:Int, onMask:Int, offMask:Int):Boolean =
			(value & (onMask | offMask)) == onMask
		
	def maskTestLong(value:Long, onMask:Long, offMask:Long):Boolean =
			(value & (onMask | offMask)) == onMask
		
	def maskTestChar(value:Char, onMask:Char, offMask:Char):Boolean =
			(value & (onMask | offMask)) == onMask
		
	//------------------------------------------------------------------------------
	
	@strictfp
	def denormalFloat(it:Float):Boolean	=
			if (it == 0)												false
			else if (it > -JFloat.MIN_NORMAL && it < JFloat.MIN_NORMAL)	true
			else														false
			
	@strictfp
	def denormalDouble(it:Double):Boolean	=
			if (it == 0)													false
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
