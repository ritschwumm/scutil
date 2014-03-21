package scutil

import java.lang.{ Math => JMath, Long => JLong }

import scala.annotation.tailrec

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
	
	def nextPow2(it:Long):Long	=
				 if (it == 0)	0
			else if (it == 1)	1
			else 				(JLong highestOneBit (it-1)) << 1
			
	// inverse to log10 in the standard library
	def exp10(value:Double):Double	= JMath pow (10, value)
	
	// inverse to pow in the standard library
	def logB(base:Double, value:Double):Double	= (JMath log value) / (JMath log base)
	
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
			
	def swapEndianByteArray(value:Array[Byte]):Array[Byte]	= {
		val l	= value.length
		val	out	= new Array[Byte](l)
		var i	= 0
		while (i < l) {
			out(i)	= value(l-i-1)
			i	+= 1
		}
		out
	}
	
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
}
