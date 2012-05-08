package scutil

object Math {
	def clamp(value:Byte, minValue:Byte, maxValue:Byte):Byte	=
			if (value < minValue) minValue else if (value > maxValue) maxValue else value
	
	def clamp(value:Short, minValue:Short, maxValue:Short):Short	=
			if (value < minValue) minValue else if (value > maxValue) maxValue else value
			
	def clamp(value:Int, minValue:Int, maxValue:Int):Int	=
			if (value < minValue) minValue else if (value > maxValue) maxValue else value                    
		
	def clamp(value:Long, minValue:Long, maxValue:Long):Long	=
			if (value < minValue) minValue else if (value > maxValue) maxValue else value
			
	def clamp(value:Float, minValue:Float, maxValue:Float):Float	=
			if (value < minValue) minValue else if (value > maxValue) maxValue else value
			
	def clamp(value:Double, minValue:Double, maxValue:Double):Double	=
			if (value < minValue) minValue else if (value > maxValue) maxValue else value
			
	//------------------------------------------------------------------------------
	
	def modulo(value:Byte, raster:Byte):Byte ={
		val	raw	= value % raster
		(if (raster < 0 && raw > 0 || raster > 0 && raw < 0)	raw + raster else raw).toByte
	}
	
	def modulo(value:Short, raster:Short):Short ={
		val	raw	= value % raster
		(if (raster < 0 && raw > 0 || raster > 0 && raw < 0)	raw + raster else raw).toShort
	}
	
	def modulo(value:Int, raster:Int):Int = {
		val	raw	= value % raster
		if (raster < 0 && raw > 0 || raster > 0 && raw < 0)	raw + raster else raw
	}
	
	def modulo(value:Long, raster:Long):Long = {
		val	raw	= value % raster
		if (raster < 0 && raw > 0 || raster > 0 && raw < 0)	raw + raster else raw
	}

	def modulo(value:Float, raster:Float):Float = {
		val	raw	= value % raster
		if (raster < 0 && raw > 0 || raster > 0 && raw < 0)	raw + raster else raw
	}
	
	def modulo(value:Double, raster:Double):Double ={
		val	raw	= value % raster
		if (raster < 0 && raw > 0 || raster > 0 && raw < 0)	raw + raster else raw
	}
	
	//------------------------------------------------------------------------------
	
	def blend(a:Float, b:Float, ratio:Float):Float	=
			a * (0 + ratio) + b * (1 - ratio) 
	
	def blend(a:Double, b:Double, ratio:Double):Double	=
			a * (0 + ratio) + b * (1 - ratio) 
	
	//------------------------------------------------------------------------------
	
	def unsigned(value:Byte):Short	= (value & 0x000000ff).toShort
	def unsigned(value:Short):Int	= (value & 0x0000ffff)
	def unsigned(value:Int):Long	= (value & 0xffffffffL)
	def unsigned(value:Long):BigInt	= {
		val	tmp	= BigInt(value)
		if (tmp >= 0)	tmp
		else			BigInt(Long.MaxValue)*2	- tmp
	}
	
	//------------------------------------------------------------------------------
	
	def swapEndian(value:Char):Char	= 
			(
				((value << 8) & 0xff00) |
				((value >> 8) & 0x00ff)
			).toChar
			
	def swapEndian(value:Short):Short	= 
			(
				((value << 8) & 0xff00) |
				((value >> 8) & 0x00ff)
			).toShort
			
	def swapEndian(value:Int):Int	=
			((value << 24) & 0xff000000)	|
			((value >> 24) & 0x000000ff)	|
			((value <<  8) & 0x00ff0000)	|
			((value >>  8) & 0x0000ff00)
			
	def swapEndian(value:Long):Long	=
			((value << 56) & 0xff00000000000000L)	|
			((value >> 56) & 0x00000000000000ffL)	|
			((value << 40) & 0x00ff000000000000L)	|
			((value >> 40) & 0x000000000000ff00L)	|
			((value << 24) & 0x0000ff0000000000L)	|
			((value >> 24) & 0x0000000000ff0000L)	|
			((value <<  8) & 0x000000ff00000000L)	|
			((value >>  8) & 0x00000000ff000000L)
			
	def swapEndian(value:Array[Byte]):Array[Byte]	= {
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
	
	def maskTest(value:Byte, onMask:Byte, offMask:Byte):Boolean = 
			(value & (onMask | offMask)) == onMask
		
	def maskTest(value:Short, onMask:Short, offMask:Short):Boolean = 
			(value & (onMask | offMask)) == onMask
		
	def maskTest(value:Int, onMask:Int, offMask:Int):Boolean = 
			(value & (onMask | offMask)) == onMask
		
	def maskTest(value:Long, onMask:Long, offMask:Long):Boolean = 
			(value & (onMask | offMask)) == onMask
		
	def maskTest(value:Char, onMask:Char, offMask:Char):Boolean = 
			(value & (onMask | offMask)) == onMask
}
