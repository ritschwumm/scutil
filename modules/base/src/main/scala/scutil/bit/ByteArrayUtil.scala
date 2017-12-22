package scutil.bit

object ByteArrayUtil {
	def reverse(value:Array[Byte]):Array[Byte]	= {
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
	
	val bytesPerShort	= 2
	val bytesPerInt		= 4
	val bytesPerLong	= 8
	
	//------------------------------------------------------------------------------
	
	def getBigEndianShort(array:Array[Byte], byteOffset:Int):Short	=
			(	((array(byteOffset+0) & 0xff) << 8)	|
				((array(byteOffset+1) & 0xff) << 0)
			).toShort
			
	def getLittleEndianShort(array:Array[Byte], byteOffset:Int):Short	=
			(	((array(byteOffset+0) & 0xff) << 0)	|
				((array(byteOffset+1) & 0xff) << 8)
			).toShort
			
	def getBigEndianInt(array:Array[Byte], byteOffset:Int):Int	=
			(	((array(byteOffset+0) & 0xff) << 24)	|
				((array(byteOffset+1) & 0xff) << 16)	|
				((array(byteOffset+2) & 0xff) <<  8)	|
				((array(byteOffset+3) & 0xff) <<  0)
			)
			
	def getLittleEndianInt(array:Array[Byte], byteOffset:Int):Int	=
			(	((array(byteOffset+0) & 0xff) <<  0)	|
				((array(byteOffset+1) & 0xff) <<  8)	|
				((array(byteOffset+2) & 0xff) << 16)	|
				((array(byteOffset+3) & 0xff) << 24)
			)
			
	def getBigEndianLong(array:Array[Byte], byteOffset:Int):Long	=
			(	((array(byteOffset+0) & 0xffL) << 56)	|
				((array(byteOffset+1) & 0xffL) << 48)	|
				((array(byteOffset+2) & 0xffL) << 40)	|
				((array(byteOffset+3) & 0xffL) << 32)	|
				((array(byteOffset+0) & 0xffL) << 24)	|
				((array(byteOffset+1) & 0xffL) << 16)	|
				((array(byteOffset+2) & 0xffL) <<  8)	|
				((array(byteOffset+3) & 0xffL) <<  0)
			)
			
	def getLittleEndianLong(array:Array[Byte], byteOffset:Int):Long	=
			(	((array(byteOffset+0) & 0xffL) <<  0)	|
				((array(byteOffset+1) & 0xffL) <<  8)	|
				((array(byteOffset+2) & 0xffL) << 16)	|
				((array(byteOffset+3) & 0xffL) << 24)	|
				((array(byteOffset+0) & 0xffL) << 32)	|
				((array(byteOffset+1) & 0xffL) << 40)	|
				((array(byteOffset+2) & 0xffL) << 48)	|
				((array(byteOffset+3) & 0xffL) << 56)
			)
			
	//------------------------------------------------------------------------------
	
	def putBigEndianShort(array:Array[Byte], byteOffset:Int, value:Short) {
		array(byteOffset+0)	= (value >> 8).toByte
		array(byteOffset+1)	= (value >> 0).toByte
	}
	
	def putLittleEndianShort(array:Array[Byte], byteOffset:Int, value:Short) {
		array(byteOffset+0)	= (value >> 0).toByte
		array(byteOffset+1)	= (value >> 8).toByte
	}
	
	def putBigEndianInt(array:Array[Byte], byteOffset:Int, value:Int) {
		array(byteOffset+0)	= (value >> 24).toByte
		array(byteOffset+1)	= (value >> 16).toByte
		array(byteOffset+2)	= (value >>  8).toByte
		array(byteOffset+3)	= (value >>  0).toByte
	}
	
	def putLittleEndianInt(array:Array[Byte], byteOffset:Int, value:Int) {
		array(byteOffset+0)	= (value >>  0).toByte
		array(byteOffset+1)	= (value >>  8).toByte
		array(byteOffset+2)	= (value >> 16).toByte
		array(byteOffset+3)	= (value >> 24).toByte
	}
	
	def putBigEndianLong(array:Array[Byte], byteOffset:Int, value:Long) {
		array(byteOffset+0)	= (value >> 56).toByte
		array(byteOffset+1)	= (value >> 48).toByte
		array(byteOffset+2)	= (value >> 40).toByte
		array(byteOffset+3)	= (value >> 32).toByte
		array(byteOffset+4)	= (value >> 24).toByte
		array(byteOffset+5)	= (value >> 16).toByte
		array(byteOffset+6)	= (value >>  8).toByte
		array(byteOffset+7)	= (value >>  0).toByte
	}
	
	def putLittleEndianLong(array:Array[Byte], byteOffset:Int, value:Long) {
		array(byteOffset+0)	= (value >>  0).toByte
		array(byteOffset+1)	= (value >>  8).toByte
		array(byteOffset+2)	= (value >> 16).toByte
		array(byteOffset+3)	= (value >> 24).toByte
		array(byteOffset+4)	= (value >> 32).toByte
		array(byteOffset+5)	= (value >> 40).toByte
		array(byteOffset+6)	= (value >> 48).toByte
		array(byteOffset+7)	= (value >> 56).toByte
	}
	
	//------------------------------------------------------------------------------
	
	def swapEndianShort(array:Array[Byte], byteOffset:Int) {
		val a	= array(byteOffset+0)
		val b	= array(byteOffset+1)
		array(byteOffset+1)	= a
		array(byteOffset+0)	= b
	}
	
	def swapEndianInt(array:Array[Byte], byteOffset:Int) {
		val a	= array(byteOffset+0)
		val b	= array(byteOffset+1)
		val c	= array(byteOffset+2)
		val d	= array(byteOffset+3)
		array(byteOffset+3)	= a
		array(byteOffset+2)	= b
		array(byteOffset+1)	= c
		array(byteOffset+0)	= d
	}
	
	def swapEndianLong(array:Array[Byte], byteOffset:Int) {
		val a	= array(byteOffset+0)
		val b	= array(byteOffset+1)
		val c	= array(byteOffset+2)
		val d	= array(byteOffset+3)
		val e	= array(byteOffset+4)
		val f	= array(byteOffset+5)
		val g	= array(byteOffset+6)
		val h	= array(byteOffset+7)
		array(byteOffset+7)	= a
		array(byteOffset+6)	= b
		array(byteOffset+5)	= c
		array(byteOffset+4)	= d
		array(byteOffset+3)	= e
		array(byteOffset+2)	= f
		array(byteOffset+1)	= g
		array(byteOffset+0)	= h
	}
	
	//------------------------------------------------------------------------------
	
	def bigEndianShort(value:Short):Array[Byte]	= {
		val out	= new Array[Byte](bytesPerShort)
		putBigEndianShort(out, 0, value)
		out
	}
	
	def littleEndianShort(value:Short):Array[Byte]	= {
		val out	= new Array[Byte](bytesPerShort)
		putLittleEndianShort(out, 0, value)
		out
	}
	
	def bigEndianInt(value:Int):Array[Byte]	= {
		val out	= new Array[Byte](bytesPerInt)
		putBigEndianInt(out, 0, value)
		out
	}
	
	def littleEndianInt(value:Int):Array[Byte]	= {
		val out	= new Array[Byte](bytesPerInt)
		putLittleEndianInt(out, 0, value)
		out
	}
	
	def bigEndianLong(value:Long):Array[Byte]	= {
		val out	= new Array[Byte](bytesPerLong)
		putBigEndianLong(out, 0, value)
		out
	}
	
	def littleEndianLong(value:Long):Array[Byte]	= {
		val out	= new Array[Byte](bytesPerLong)
		putLittleEndianLong(out, 0, value)
		out
	}
}
