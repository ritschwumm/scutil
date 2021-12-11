package scutil.bit

import java.lang.{ Long as JLong }

object functions {
	def nextPow2(it:Long):Long	=
		if		(it == 0)	0
		else if	(it == 1)	1
		else 				JLong.highestOneBit(it-1) << 1

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

	// Short.reverseBytes
	def swapEndianShort(value:Short):Short	=
		(
			((value << 8) & 0xff00) |
			((value >> 8) & 0x00ff)
		).toShort

	// Integer.reverseBytes
	def swapEndianInt(value:Int):Int	=
		((value << 24) & 0xff000000)	|
		((value >> 24) & 0x000000ff)	|
		((value <<  8) & 0x00ff0000)	|
		((value >>  8) & 0x0000ff00)

	// Long.reverseBytes
	def swapEndianLong(value:Long):Long	=
		((value << 56) & 0xff00000000000000L)	|
		((value >> 56) & 0x00000000000000ffL)	|
		((value << 40) & 0x00ff000000000000L)	|
		((value >> 40) & 0x000000000000ff00L)	|
		((value << 24) & 0x0000ff0000000000L)	|
		((value >> 24) & 0x0000000000ff0000L)	|
		((value <<  8) & 0x000000ff00000000L)	|
		((value >>  8) & 0x00000000ff000000L)

	// Character.reverseBytes
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
}
