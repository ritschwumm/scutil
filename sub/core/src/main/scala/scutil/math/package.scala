package scutil

import java.lang.{ Math => JMath, Long => JLong, Float => JFloat, Double => JDouble }

import scala.annotation.tailrec
import scala.annotation.strictfp

import scala.math.Pi
import scala.math.abs

package object math {
	val PiHalf		= Pi / 2
	val PiDouble	= Pi * 2
	
	//------------------------------------------------------------------------------
	
	val Log2			= JMath log 2
	val Log2Reciprocal	= 1.0 / Log2
	
	def log2(value:Double):Double	= (JMath log value) * Log2Reciprocal
	def exp2(value:Double):Double	= JMath exp (value * Log2)
	
	def exp10(value:Double):Double	= JMath pow (10, value)
	// def log10(value:Double):Double	= JMath log10 value
	
	def logB(base:Double, value:Double):Double	= (JMath log value) / (JMath log base)
	// def expB(base:Double, value:Double):Double	= JMath pow (base, value)
	
	//------------------------------------------------------------------------------
	
	def nextPow2(it:Long):Long	=
				 if (it == 0)	0
			else if (it == 1)	1
			else 				(JLong highestOneBit (it-1)) << 1
			
	//------------------------------------------------------------------------------

	def clampByte(value:Byte, minValue:Byte, maxValue:Byte):Byte	=
			if (value < minValue) minValue else if (value > maxValue) maxValue else value
	
	def clampShort(value:Short, minValue:Short, maxValue:Short):Short	=
			if (value < minValue) minValue else if (value > maxValue) maxValue else value
			
	def clampInt(value:Int, minValue:Int, maxValue:Int):Int	=
			if (value < minValue) minValue else if (value > maxValue) maxValue else value
		
	def clampLong(value:Long, minValue:Long, maxValue:Long):Long	=
			if (value < minValue) minValue else if (value > maxValue) maxValue else value
			
	def clampFloat(value:Float, minValue:Float, maxValue:Float):Float	=
			if (value < minValue) minValue else if (value > maxValue) maxValue else value
			
	def clampDouble(value:Double, minValue:Double, maxValue:Double):Double	=
			if (value < minValue) minValue else if (value > maxValue) maxValue else value
			
	//------------------------------------------------------------------------------
	
	def max3Byte(a:Byte, b:Byte, c:Byte):Byte			= (JMath max (a, JMath max (b,c))).toByte
	def max3Short(a:Short, b:Short, c:Short):Short		= (JMath max (a, JMath max (b,c))).toShort
	def max3Int(a:Int, b:Int, c:Int):Int				= JMath max (a, JMath max (b,c))
	def max3Long(a:Long, b:Long, c:Long):Long			= JMath max (a, JMath max (b,c))
	def max3Float(a:Float, b:Float, c:Float):Float		= JMath max (a, JMath max (b,c))
	def max3Double(a:Double, b:Double, c:Double):Double	= JMath max (a, JMath max (b,c))
		
	//------------------------------------------------------------------------------

	def min3Byte(a:Byte, b:Byte, c:Byte):Byte			= (JMath min (a, JMath min (b,c))).toByte
	def min3Short(a:Short, b:Short, c:Short):Short		= (JMath min (a, JMath min (b,c))).toShort
	def min3Int(a:Int, b:Int, c:Int):Int				= JMath min (a, JMath min (b,c))
	def min3Long(a:Long, b:Long, c:Long):Long			= JMath min (a, JMath min (b,c))
	def min3Float(a:Float, b:Float, c:Float):Float		= JMath min (a, JMath min (b,c))
	def min3Double(a:Double, b:Double, c:Double):Double	= JMath min (a, JMath min (b,c))
		
	//------------------------------------------------------------------------------
	
	@tailrec
	def gcdByte(a:Byte, b:Byte):Byte	=
			if (b == 0)	abs(a).toByte
			else		gcdByte(b, (a % b).toByte)
			
	@tailrec
	def gcdShort(a:Short, b:Short):Short	=
			if (b == 0)	abs(a).toShort
			else		gcdShort(b, (a % b).toShort)
		
	@tailrec
	def gcdInt(a:Int, b:Int):Int	=
			if (b == 0)	abs(a)
			else		gcdInt(b, a % b)
		
	@tailrec
	def gcdLong(a:Long, b:Long):Long	=
			if (b == 0)	abs(a)
			else		gcdLong(b, a % b)
		
	//------------------------------------------------------------------------------
		
	def lcmByte(a:Byte, b:Byte):Byte	=
			(abs(a * b) / gcdByte(a, b)).toByte
		
	def lcmShort(a:Short, b:Short):Short	=
			(abs(a * b) / gcdShort(a, b)).toShort
		
	def lcmInt(a:Int, b:Int):Int	=
			abs(a * b) / gcdInt(a, b)
		
	def lcmLong(a:Long, b:Long):Long	=
			abs(a * b) / gcdLong(a, b)
		
	//------------------------------------------------------------------------------
	
	def moduloByte(value:Byte, raster:Byte):Byte ={
		val	raw	= value % raster
		(if (raster < 0 && raw > 0 || raster > 0 && raw < 0)	raw + raster else raw).toByte
	}
	
	def moduloShort(value:Short, raster:Short):Short ={
		val	raw	= value % raster
		(if (raster < 0 && raw > 0 || raster > 0 && raw < 0)	raw + raster else raw).toShort
	}
	
	def moduloInt(value:Int, raster:Int):Int = {
		val	raw	= value % raster
		if (raster < 0 && raw > 0 || raster > 0 && raw < 0)	raw + raster else raw
	}
	
	def moduloLong(value:Long, raster:Long):Long = {
		val	raw	= value % raster
		if (raster < 0 && raw > 0 || raster > 0 && raw < 0)	raw + raster else raw
	}

	def moduloFloat(value:Float, raster:Float):Float = {
		val	raw	= value % raster
		if (raster < 0 && raw > 0 || raster > 0 && raw < 0)	raw + raster else raw
	}
	
	def moduloDouble(value:Double, raster:Double):Double ={
		val	raw	= value % raster
		if (raster < 0 && raw > 0 || raster > 0 && raw < 0)	raw + raster else raw
	}
	
	//------------------------------------------------------------------------------
	
	def divUpByte(value:Byte, divisor:Byte):Byte	=
			((value + divisor - 1) / divisor).toByte
		
	def divUpShort(value:Short, divisor:Short):Short	=
			((value + divisor - 1) / divisor).toShort
		
	def divUpInt(value:Int, divisor:Int):Int	=
			(value + divisor - 1) / divisor
		
	def divUpLong(value:Long, divisor:Long):Long	=
			(value + divisor - 1) / divisor
		
	//------------------------------------------------------------------------------
	
	/** ratio 0..1 select a..b */
	def blendToFloat(ratio:Float, a:Float, b:Float):Float	=
			a * (1 - ratio) + b * (0 + ratio)
	
	/** ratio 0..1 select a..b */
	def blendToDouble(ratio:Double, a:Double, b:Double):Double	=
			a * (1 - ratio) + b * (0 + ratio)
	
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
