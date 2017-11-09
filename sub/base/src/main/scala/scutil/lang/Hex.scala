package scutil.lang

object Hex {
	val byteArrayPrism	= Prism(decodeByteArray,	encodeByteArray)
	val byteStringPrism	= Prism(decodeByteString,	encodeByteString)
	
	//------------------------------------------------------------------------------
	
	def encodeByteString(bytes:ByteString):String	=
			encodeByteArray(bytes.unsafeValue)
	
	@deprecated("0.124", "use encodeByteArray")
	def string(bytes:Array[Byte]):String	=
			encodeByteArray(bytes)
		
	def encodeByteArray(bytes:Array[Byte]):String	=
			bytes map nibbles mkString ""
		
	def nibbles(it:Byte):String	=
			(nibble charAt ((it >> 4) & 0xf)).toString	+
			(nibble charAt ((it >> 0) & 0xf)).toString
		
	private val nibble	= "0123456789abcdef"
	
	//------------------------------------------------------------------------------
	
	private val invalid	= -1
	
	def decodeByteString(s:String):Option[ByteString]	=
			decodeByteArray(s) map ByteString.unsafeFromByteArray
		
	@deprecated("0.124", "use decodeByteArray")
	def bytes(s:String):Option[Array[Byte]]	= decodeByteArray(s)
	
	def decodeByteArray(s:String):Option[Array[Byte]]	= {
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
		Some(out)
	}
	
	private def byte(ch:Char, cl:Char):Int	= {
		val dh	= digit(ch)
		val dl	= digit(cl)
		if (dh != invalid && dl != invalid)	(dh << 4) | (dl << 0)
		else								invalid
	}
	
	private def digit(c:Char):Int	=
				 if (c >= '0' && c <= '9')	c - '0'
			else if (c >= 'a' && c <= 'f')	c - 'a' + 10
			else if (c >= 'A' && c <= 'F')	c - 'A' + 10
			else invalid
}
