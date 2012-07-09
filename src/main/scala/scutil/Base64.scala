package scutil

import scutil.data.Marshaller

/** 
encodes and decodes byte arrays into strings using the base64 encoding method.
@see RFC4648
@see  javax.xml.bind.DatatypeConverter 
*/
object Base64 extends Marshaller[Array[Byte],String] {
	private val whitespace	= """\s+"""
	private val validInput	= """^[^=]+={0,2}$"""
	private val padding		= '='
	private val encode 		= "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".toCharArray
	private val	decode		= {
		// -1 means "invalid", -2 means "padding"
		val out	= new Array[Byte](256)
		for (i <- 0 until 256)				out(i)		= -1
											out('=')	= -2
		for ((i,v) <- encode.zipWithIndex)	out(i)		= v.toByte
		out
	}
	
	def write(data:Array[Byte]):String = {
		val	packetsSize		= data.length / 3
		val extraSize		= data.length % 3
		var	output			= new StringBuilder
		var	inputIndex		= 0
		var packetsIndex	= 0
		while (packetsIndex < packetsSize) {
			output append (encode(((data(inputIndex+0) >> 2) & 0x3f)))
			output append (encode(((data(inputIndex+0) << 4) & 0x30) | ((data(inputIndex+1) >> 4) & 0x0f)))
			output append (encode(((data(inputIndex+1) << 2) & 0x3c) | ((data(inputIndex+2) >> 6) & 0x03)))
			output append (encode(((data(inputIndex+2)     ) & 0x3f)))
			packetsIndex	+= 1
			inputIndex		+= 3
		}
		if (extraSize == 1) {
			output append (encode(((data(inputIndex+0) >> 2) & 0x3f)))
			output append (encode(((data(inputIndex+0) << 4) & 0x30)))
			output append padding
			output append padding
		}
		else if (extraSize == 2) {
			output append (encode(((data(inputIndex+0) >> 2) & 0x3f)))
			output append (encode(((data(inputIndex+0) << 4) & 0x30) | ((data(inputIndex+1) >> 4) & 0x0f)))
			output append (encode(((data(inputIndex+1) << 2) & 0x3c)))
			output append padding
		}
		output.toString
	}
	
	def read(text:String):Option[Array[Byte]] = {
		val cleanText	= text replaceAll (whitespace, "")
		if (cleanText.length == 0)		return Some(new Array[Byte](0))
		
		val illegalLength	= cleanText.length % 4 != 0
		val	illegalChars	= cleanText exists { it => it < 0 || it >= 256 || decode(it) == -1 }
		val illegalFormat	= !(cleanText matches validInput) 
		if (illegalLength || illegalChars || illegalFormat)	return None
		
		val	input		= cleanText.toCharArray
		val	inputSize	= input.length
		var	outputSize	= {
			val paddings1	= if (input(inputSize-1) == padding) 1 else 0
			val paddings2	= if (input(inputSize-2) == padding) 1 else 0
			((inputSize + 3 ) / 4 ) * 3 - paddings1 - paddings2
		}
		val	output		= new Array[Byte](outputSize)
		
		var inputIndex		= 0
		var	outputIndex		= 0
		var	bitShift		= 0
		var	akku			= 0
		while (inputIndex < inputSize) {
			val	value	= decode(input(inputIndex))
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
		Some(output)
	}
}
