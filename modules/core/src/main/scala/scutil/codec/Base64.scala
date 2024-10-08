package scutil.codec

import scutil.lang.*

/**
encodes and decodes byte arrays into strings using the base64 encoding method.
@see RFC4648
@see javax.xml.bind.DatatypeConverter
*/
object Base64 {
	val byteStringPrism	= Prism(decodeByteString,	encodeByteString)

	//------------------------------------------------------------------------------

	private val whitespaceRE	= """\s+"""

	private val padding:Char	= '='

	// NOTE there's an alternative encoding ending in "-_" for urls and filenames
	private val alphabet:IArray[Char]	= IArray.unsafeFromArray("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".toCharArray)

	private val invalidFlag	= -1.toByte
	private val paddingFlag	= -2.toByte
	private val	table:IArray[Byte]	= {
		val out	= new Array[Byte](256)
		for (i <- 0 until 256)					out(i)	= invalidFlag
		for ((i, v) <- alphabet.zipWithIndex)	out(i)	= v.toByte
		out(padding)	= paddingFlag
		IArray.unsafeFromArray(out)
	}

	private def validInput(s:String):Boolean	=
		(s.length % 4 == 0)	&& {
			var pad	= false
			var i	= 0
			while (i < s.length) {
				val c	= s.charAt(i)
				if (c < 0)				return false
				if (c > table.length)	return false
				val x	= table(c)
				if		(x == invalidFlag)			return false
				else if	(x == paddingFlag && !pad)	pad = true
				else if	(x != paddingFlag &&  pad)	return false
				i = i + 1
			}
			true
		}

	private val emptyOutput	= IArray.empty[Byte]

	//------------------------------------------------------------------------------

	/** standard alphabet, no line feeds, adds padding */
	def encodeByteString(data:ByteString):String =
		encodeImpl(data.value)

	private def encodeImpl(data:IArray[Byte]):String = {
		val	packetsSize		= data.length / 3
		val extraSize		= data.length % 3
		val	output			= new StringBuilder
		var	inputIndex		= 0
		var packetsIndex	= 0
		while (packetsIndex < packetsSize) {
			output append (alphabet(((data(inputIndex+0) >> 2) & 0x3f)))
			output append (alphabet(((data(inputIndex+0) << 4) & 0x30) | ((data(inputIndex+1) >> 4) & 0x0f)))
			output append (alphabet(((data(inputIndex+1) << 2) & 0x3c) | ((data(inputIndex+2) >> 6) & 0x03)))
			output append (alphabet(((data(inputIndex+2)     ) & 0x3f)))
			packetsIndex	+= 1
			inputIndex		+= 3
		}
		if (extraSize == 1) {
			output append (alphabet(((data(inputIndex+0) >> 2) & 0x3f)))
			output append (alphabet(((data(inputIndex+0) << 4) & 0x30)))
			output append padding
			output append padding
		}
		else if (extraSize == 2) {
			output append (alphabet(((data(inputIndex+0) >> 2) & 0x3f)))
			output append (alphabet(((data(inputIndex+0) << 4) & 0x30) | ((data(inputIndex+1) >> 4) & 0x0f)))
			output append (alphabet(((data(inputIndex+1) << 2) & 0x3c)))
			output append padding
		}
		output.result
	}

	//------------------------------------------------------------------------------

	/** standard alphabet, whitespace is ignored, padding is required */
	def decodeByteString(text:String):Option[ByteString] =
		decodeImpl(text).map(ByteString.fromIArray)

	private def decodeImpl(text:String):Option[IArray[Byte]] = {
		// TODO codec ignoring all whitespace input might be stupid
		val cleanText	= text.replaceAll(whitespaceRE, "")
		if (cleanText.length == 0)	return Some(emptyOutput)
		if (!validInput(cleanText))	return None

		val	input		= cleanText.toCharArray
		val	inputSize	= input.length
		val	outputSize	=
			((inputSize + 3 ) / 4 ) * 3 -
			(if (input(inputSize-1) == padding) 1 else 0)	-
			(if (input(inputSize-2) == padding) 1 else 0)
		val	output		= new Array[Byte](outputSize)

		var inputIndex	= 0
		var	outputIndex	= 0
		var	bitShift	= 0
		var	akku		= 0
		while (inputIndex < inputSize) {
			val	value	= table(input(inputIndex))
			// ignores padding as well as invalid chars
			if (value >= 0) {
				bitShift	+=	6
				akku		<<=	6
				akku		|=	value
				if (bitShift >= 8) {
					bitShift	-=	8
					output(outputIndex)	= (akku >> bitShift).toByte
					outputIndex	+= 1
				}
			}
			inputIndex	+= 1
		}
		Some(IArray.unsafeFromArray(output))
	}

	//------------------------------------------------------------------------------
	//## padding helper

	def addPadding(s:String):String	=
		s + (
			"=" * (3 - (s.length + 3) % 4)
		)

	def removePadding(s:String):String	=
		s.replaceAll("=+$", "")

	//------------------------------------------------------------------------------
	//## line break helper

	def breakLines76(s:String):String	=
		breakLines(s, 76)

	def breakLines64(s:String):String	=
		breakLines(s, 64)

	private def breakLines(s:String, length:Int):String	=
		(s grouped length) mkString "\r\n"

	def unbreakLines(s:String):String	=
		s.replaceAll("\r\n", "")
}
