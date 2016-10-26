package scutil.codec.pimp

import java.nio._
import java.nio.charset._

import scutil.lang._

object CharsetImplicits extends CharsetImplicits

trait CharsetImplicits {
    implicit def toCharSetImplicits(peer:Charset)	= new CharsetExt(peer)
}

final class CharsetExt(peer:Charset) {
	def encodeTried(string:String):Tried[CharacterCodingException,Array[Byte]]	=
			try {
				Win((failingEncoder encode (CharBuffer wrap string)).array)
			}
			catch { case e:CharacterCodingException =>
				Fail(e)
			}
			
	def decodeTried(bytes:Array[Byte]):Tried[CharacterCodingException,String]	=
			try {
				Win((failingDecoder decode (ByteBuffer wrap bytes)).toString)
			}
			catch { case e:CharacterCodingException =>
				Fail(e)
			}
			
	def failingEncoder:CharsetEncoder	= {
		val encoder	= peer.newEncoder
		encoder onMalformedInput		CodingErrorAction.REPORT
		encoder onUnmappableCharacter	CodingErrorAction.REPORT
		encoder
	}
	
	def failingDecoder:CharsetDecoder	= {
		val decoder	= peer.newDecoder
		decoder onMalformedInput		CodingErrorAction.REPORT
		decoder onUnmappableCharacter	CodingErrorAction.REPORT
		decoder
	}
}
