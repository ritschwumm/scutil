package scutil.bit

import java.util.{ Arrays as JArrays }

object IByteArrayUtil {
	def reverse(value:IArray[Byte]):IArray[Byte]	=
		modify(value, ByteArrayUtil.reverse)

	//------------------------------------------------------------------------------

	val bytesPerShort	= ByteArrayUtil.bytesPerShort
	val bytesPerInt		= ByteArrayUtil.bytesPerInt
	val bytesPerLong	= ByteArrayUtil.bytesPerLong

	//------------------------------------------------------------------------------

	def fromBigEndianShort(value:Short):IArray[Byte]	=
		IArray.unsafeFromArray(ByteArrayUtil.fromBigEndianShort(value))

	def fromLittleEndianShort(value:Short):IArray[Byte]	=
		IArray.unsafeFromArray(ByteArrayUtil.fromLittleEndianShort(value))

	def fromBigEndianInt(value:Int):IArray[Byte]	=
		IArray.unsafeFromArray(ByteArrayUtil.fromBigEndianInt(value))

	def fromLittleEndianInt(value:Int):IArray[Byte]	=
		IArray.unsafeFromArray(ByteArrayUtil.fromLittleEndianInt(value))

	def fromBigEndianLong(value:Long):IArray[Byte]	=
		IArray.unsafeFromArray(ByteArrayUtil.fromBigEndianLong(value))

	def fromLittleEndianLong(value:Long):IArray[Byte]	=
		IArray.unsafeFromArray(ByteArrayUtil.fromLittleEndianLong(value))

	//------------------------------------------------------------------------------

	def toBigEndianShort(array:IArray[Byte]):Short	=
		getBigEndianShort(array, 0)

	def toLittleEndianShort(array:IArray[Byte]):Short	=
		getLittleEndianShort(array, 0)

	def toBigEndianInt(array:IArray[Byte]):Int	=
		getBigEndianInt(array, 0)

	def toLittleEndianInt(array:IArray[Byte]):Int	=
		getLittleEndianInt(array, 0)

	def toBigEndianLong(array:IArray[Byte]):Long	=
		getBigEndianLong(array, 0)

	def toLittleEndianLong(array:IArray[Byte]):Long	=
		getLittleEndianLong(array, 0)

	//------------------------------------------------------------------------------

	def getBigEndianShort(array:IArray[Byte], byteOffset:Int):Short	=
		ByteArrayUtil.getBigEndianShort(unsafeValue(array), byteOffset)

	def getLittleEndianShort(array:IArray[Byte], byteOffset:Int):Short	=
		ByteArrayUtil.getLittleEndianShort(unsafeValue(array), byteOffset)

	def getBigEndianInt(array:IArray[Byte], byteOffset:Int):Int	=
		ByteArrayUtil.getBigEndianInt(unsafeValue(array), byteOffset)

	def getLittleEndianInt(array:IArray[Byte], byteOffset:Int):Int	=
		ByteArrayUtil.getLittleEndianInt(unsafeValue(array), byteOffset)

	def getBigEndianLong(array:IArray[Byte], byteOffset:Int):Long	=
		ByteArrayUtil.getBigEndianLong(unsafeValue(array), byteOffset)

	def getLittleEndianLong(array:IArray[Byte], byteOffset:Int):Long	=
		ByteArrayUtil.getLittleEndianLong(unsafeValue(array), byteOffset)

	//------------------------------------------------------------------------------

	def putBigEndianShort(array:IArray[Byte], byteOffset:Int, value:Short):IArray[Byte]	=
		modify(array, ByteArrayUtil.putBigEndianShort(_, byteOffset, value))

	def putLittleEndianShort(array:IArray[Byte], byteOffset:Int, value:Short):IArray[Byte]	=
		modify(array, ByteArrayUtil.putLittleEndianShort(_, byteOffset, value))

	def putBigEndianInt(array:IArray[Byte], byteOffset:Int, value:Int):IArray[Byte]	=
		modify(array, ByteArrayUtil.putBigEndianInt(_, byteOffset, value))

	def putLittleEndianInt(array:IArray[Byte], byteOffset:Int, value:Int):IArray[Byte]	=
		modify(array, ByteArrayUtil.putLittleEndianInt(_, byteOffset, value))

	def putBigEndianLong(array:IArray[Byte], byteOffset:Int, value:Long):IArray[Byte]	=
		modify(array, ByteArrayUtil.putBigEndianLong(_, byteOffset, value))

	def putLittleEndianLong(array:IArray[Byte], byteOffset:Int, value:Long):IArray[Byte]	=
		modify(array, ByteArrayUtil.putLittleEndianLong(_, byteOffset, value))

	//------------------------------------------------------------------------------

	def swapEndianShort(array:IArray[Byte], byteOffset:Int):IArray[Byte]	=
		modify(array, ByteArrayUtil.swapEndianShort(_, byteOffset))

	def swapEndianInt(array:IArray[Byte], byteOffset:Int):IArray[Byte]	=
		modify(array, ByteArrayUtil.swapEndianInt(_, byteOffset))

	def swapEndianLong(array:IArray[Byte], byteOffset:Int):IArray[Byte]	=
		modify(array, ByteArrayUtil.swapEndianLong(_, byteOffset))

	//------------------------------------------------------------------------------

	private def modify(input:IArray[Byte], func:Array[Byte]=>Unit):IArray[Byte]	= {
		val tmp	= JArrays.copyOf(input.unsafeArray, input.length)
		func(tmp)
		IArray.unsafeFromArray(tmp)
	}

	@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
	private def unsafeValue(it:IArray[Byte]):Array[Byte]	=
		it.asInstanceOf[Array[Byte]]
}
