package scutil.codec

import scutil.lang.*

object Hex {
	val byteStringPrism	= Prism(decodeByteString,	encodeByteString)

	//------------------------------------------------------------------------------

	def encodeByteString(bytes:ByteString):String	=
		encodeImpl(bytes.value)

	private def encodeImpl(bytes:IArray[Byte]):String	=
		bytes map nibbles mkString ""

	def nibbles(it:Byte):String	=
		nibble.charAt((it >> 4) & 0xf).toString	+
		nibble.charAt((it >> 0) & 0xf).toString

	private val nibble	= "0123456789abcdef"

	//------------------------------------------------------------------------------

	private val invalid	= -1

	def decodeByteString(s:String):Option[ByteString]	=
		decodeImpl(s) map ByteString.fromIArray

	private def decodeImpl(s:String):Option[IArray[Byte]]	= {
		// TODO codec Base64 ignores all whitespace here: slow, but would sometimes be useful here, too
		val count	= s.length / 2
		if (s.length != count*2)	return None
		val out	= new Array[Byte](count)
		var i	= 0
		while (i < count) {
			val ch	= s charAt i*2+0
			val cl	= s charAt i*2+1
			val b	= byte(ch, cl)
			if (b == invalid)	return None
			out(i)	= b.toByte
			i	+= 1
		}
		Some(IArray.unsafeFromArray(out))
	}

	private def byte(ch:Char, cl:Char):Int	= {
		val dh	= digit(ch)
		val dl	= digit(cl)
		if (dh != invalid && dl != invalid)	(dh << 4) | (dl << 0)
		else								invalid
	}

	private def digit(c:Char):Int	=
		if		(c >= '0' && c <= '9')	c - '0'
		else if	(c >= 'a' && c <= 'f')	c - 'a' + 10
		else if	(c >= 'A' && c <= 'F')	c - 'A' + 10
		else							invalid
}
