package scutil.math

import scala.annotation.tailrec

import java.lang.Math.*
import java.lang.{ Math as JMath }

object functions {
	val Pi			= JMath.PI
	val PiHalf		= Pi / 2
	val PiDouble	= Pi * 2

	val E			= JMath.E

	//------------------------------------------------------------------------------

	val Log2			= log(2)
	val Log2Reciprocal	= 1.0 / Log2

	def log2(value:Double):Double	= log(value) * Log2Reciprocal
	def exp2(value:Double):Double	= exp(value * Log2)

	val Log10			= log(10)
	val Log10Reciprocal	= 1.0 / Log10

	def exp10(value:Double):Double	= pow(10, value)
	def log10(value:Double):Double	= JMath.log10(value)

	def logB(base:Double, value:Double):Double	= log(value) / log(base)
	def expB(base:Double, value:Double):Double	= pow(base, value)

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

	def max3Byte(a:Byte, b:Byte, c:Byte):Byte			= max(a, max(b,c)).toByte
	def max3Short(a:Short, b:Short, c:Short):Short		= max(a, max(b,c)).toShort
	def max3Int(a:Int, b:Int, c:Int):Int				= max(a, max(b,c))
	def max3Long(a:Long, b:Long, c:Long):Long			= max(a, max(b,c))
	def max3Float(a:Float, b:Float, c:Float):Float		= max(a, max(b,c))
	def max3Double(a:Double, b:Double, c:Double):Double	= max(a, max(b,c))

	//------------------------------------------------------------------------------

	def min3Byte(a:Byte, b:Byte, c:Byte):Byte			= min(a, min (b,c)).toByte
	def min3Short(a:Short, b:Short, c:Short):Short		= min(a, min (b,c)).toShort
	def min3Int(a:Int, b:Int, c:Int):Int				= min(a, min(b,c))
	def min3Long(a:Long, b:Long, c:Long):Long			= min(a, min(b,c))
	def min3Float(a:Float, b:Float, c:Float):Float		= min(a, min(b,c))
	def min3Double(a:Double, b:Double, c:Double):Double	= min(a, min(b,c))

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

	def moduloByte(value:Byte, raster:Byte):Byte = {
		val	raw	= value % raster
		(if (raster < 0 && raw > 0 || raster > 0 && raw < 0)	raw + raster else raw).toByte
	}

	def moduloShort(value:Short, raster:Short):Short = {
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

	def moduloDouble(value:Double, raster:Double):Double = {
		val	raw	= value % raster
		if (raster < 0 && raw > 0 || raster > 0 && raw < 0)	raw + raster else raw
	}

	//------------------------------------------------------------------------------

	def floorDivByte(value:Byte, raster:Byte):Byte	=
		floorDivByte(value, raster).toByte

	def roundDivByte(value:Byte, raster:Byte):Byte   =
		roundDivByte(value, raster).toByte

	def ceilDivByte(value:Byte, raster:Byte):Byte	=
		ceilDivInt(value, raster).toByte

	def floorDivShort(value:Short, raster:Short):Short	=
		floorDivShort(value, raster).toShort

	def roundDivShort(value:Short, raster:Short):Short   =
		roundDivShort(value, raster).toShort

	def ceilDivShort(value:Short, raster:Short):Short	=
		ceilDivInt(value, raster).toShort

	def floorDivInt(value:Int, raster:Int):Int	=
		if (raster > 0) {
			if (value >= 0) (value)					/ raster
			else			(value - raster + 1)	/ raster
		}
		else if (raster < 0) {
			if (value >= 0)	(value - raster - 1)	/ raster
			else			(value)					/ raster
		}
		else throw new ArithmeticException("division by zero")

	def roundDivInt(value:Int, raster:Int):Int   =
		if (raster > 0) {
			if (value >= 0) (value + (raster/2)) / raster
			else            (value - (raster/2)) / raster
		}
		else if (raster < 0) {
			if (value >= 0) (value - (raster/2)) / raster
			else            (value + (raster/2)) / raster
		}
		else throw new ArithmeticException("division by zero")

	def ceilDivInt(value:Int, raster:Int):Int	=
		if (raster > 0) {
			if (value >= 0) (value + raster - 1)	/ raster
			else			(value)					/ raster
		}
		else if (raster < 0) {
			if (value >= 0) (value)					/ raster
			else			(value+ raster + 1)		/ raster
		}
		else throw new ArithmeticException("division by zero")

	def floorDivLong(value:Long, raster:Long):Long	=
		if (raster > 0) {
			if (value >= 0) (value)					/ raster
			else			(value - raster + 1)	/ raster
		}
		else if (raster < 0) {
			if (value >= 0)	(value - raster - 1)	/ raster
			else			(value)					/ raster
		}
		else throw new ArithmeticException("division by zero")

	def roundDivLong(value:Long, raster:Long):Long   =
		if (raster > 0) {
			if (value >= 0) (value + (raster/2)) / raster
			else            (value - (raster/2)) / raster
		}
		else if (raster < 0) {
			if (value >= 0) (value - (raster/2)) / raster
			else            (value + (raster/2)) / raster
		}
		else throw new ArithmeticException("division by zero")

	def ceilDivLong(value:Long, raster:Long):Long	=
		if (raster > 0) {
			if (value >= 0) (value + raster - 1)	/ raster
			else			(value)					/ raster
		}
		else if (raster < 0) {
			if (value >= 0) (value)					/ raster
			else			(value+ raster + 1)		/ raster
		}
		else throw new ArithmeticException("division by zero")

	//------------------------------------------------------------------------------

	/** ratio 0..1 select a..b */
	def lerpFloat(ratio:Float, a:Float, b:Float):Float	=
		a + (b - a) * ratio

	/** ratio 0..1 select a..b */
	def lerpDouble(ratio:Double, a:Double, b:Double):Double	=
		a + (b - a) * ratio

	@deprecated("use lerpFloat", "0.239.0")
	def blendFloat(ratio:Float, a:Float, b:Float):Float			= lerpFloat(ratio, a, b)

	@deprecated("use lerpDouble", "0.239.0")
	def blendDouble(ratio:Double, a:Double, b:Double):Double	= lerpDouble(ratio, a, b)
}
