package scutil.number.extension

import java.util.Random

import scutil.lang.*

object RandomExtensions {
	extension (peer:Random) {
		def string(alphabet:String, length:Int):String	= {
			val	out	= new StringBuilder
			var i	= 0
			while (i < length) {
				out	+= alphabet(peer.nextInt(alphabet.length))
				i	+= 1
			}
			out.result
		}

		def byteString(length:Int):ByteString	=
			ByteString.makeWithArray(length)(peer.nextBytes)
	}
}
