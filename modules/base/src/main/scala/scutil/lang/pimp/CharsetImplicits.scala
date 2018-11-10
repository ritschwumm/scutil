package scutil.lang.pimp

import java.nio._
import java.nio.charset._

import scutil.lang.ByteString

object CharsetImplicits extends CharsetImplicits

trait CharsetImplicits {
	implicit final class CharsetExt(peer:Charset) {
		def encodeEitherByteString(string:String):Either[CharacterCodingException,ByteString]	=
				encodeEitherImpl(string) map ByteString.unsafeFromArray

		def decodeEitherByteString(string:ByteString):Either[CharacterCodingException,String]	=
				decodeEitherImpl(string.unsafeValue)

		private def encodeEitherImpl(string:String):Either[CharacterCodingException,Array[Byte]]	=
				try {
					Right((failingEncoder encode (CharBuffer wrap string)).array)
				}
				catch { case e:CharacterCodingException =>
					Left(e)
				}

		private def decodeEitherImpl(bytes:Array[Byte]):Either[CharacterCodingException,String]	=
				try {
					Right((failingDecoder decode (ByteBuffer wrap bytes)).toString)
				}
				catch { case e:CharacterCodingException =>
					Left(e)
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
}
