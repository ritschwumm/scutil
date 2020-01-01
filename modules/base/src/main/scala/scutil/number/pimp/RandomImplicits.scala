package scutil.number.pimp

import java.util.Random

import scutil.lang._

object RandomImplicits extends RandomImplicits

trait RandomImplicits {
	implicit final class RandomExt(peer:Random) {
		def string(alphabet:String, length:Int):String	= {
			val	out	= new StringBuilder
			var i	= 0
			while (i < length) {
				out	+= alphabet(peer nextInt alphabet.length)
				i	+= 1
			}
			out.toString
		}

		def byteString(length:Int):ByteString	=
			(ByteString makeWithArray length)(peer.nextBytes _)
	}
}
