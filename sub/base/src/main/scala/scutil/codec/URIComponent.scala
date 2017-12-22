package scutil.codec

import java.nio.charset._

import scala.collection.mutable

import scutil.lang._
import scutil.lang.pimp.CharsetImplicits._
import scutil.lang.pimp.EitherImplicits._

object URIComponent {
	val utf_8	= forCharset(Charsets.utf_8)
	
	def forCharset(charset:Charset):URIComponent	=
			new URIComponent(charset)
}
	
final class URIComponent(charset:Charset) {
	//------------------------------------------------------------------------------
	//## encoding
	
	/** percent-escapes everything except alphabetic, decimal digits, - _ . ! ~ * ' ( ) */
	def encode(s:String):String = {
		val bytes	= s getBytes charset
		val out		= new StringBuilder
		var i	= 0
		while (i < bytes.length) {
			val byte	= bytes(i)
			if (safe(byte)) {
				out append byte.toChar
			}
			else {
				out append '%'
				out append encodeNibble((byte >> 4) & 0xf)
				out append encodeNibble((byte >> 0) & 0xf)
			}
			i	+= 1
		}
		out.toString
	}
	
	@inline
	private def encodeNibble(nibble:Int):Char =
			if (nibble < 10)	(nibble + '0'		).toChar
			else				(nibble + 'A' - 10	).toChar
	
	@inline
	private def safe(byte:Int):Boolean =
			byte >= 'a' && byte <= 'z'	||
			byte >= 'A' && byte <= 'Z'	||
			byte >= '0' && byte <= '9'	||
			byte == '-' ||
			byte == '_' ||
			byte == '.' ||
			byte == '!' ||
			byte == '~' ||
			byte == '*' ||
			byte == '(' ||
			byte == ')' ||
			byte == '\''
	
	//------------------------------------------------------------------------------
	//## decoding
	
	def decode(s:String):Either[URIComponentProblem,String]	= {
		val b	= new mutable.ArrayBuffer[Byte]
		var i	= 0
		while (i<s.length) {
			val c	= s charAt i
			if (c >= 256)	return Left(URIComponentInvalid(i))
			else if (c == '%') {
				i	+= 1
				if (i >= s.length)	return Left(URIComponentInvalid(i))
				val n1	= decodeNibble(s charAt i)
				if (n1 == -1)		return Left(URIComponentInvalid(i))
				
				i	+= 1
				if (i >= s.length)	return Left(URIComponentInvalid(i))
				val n2	= decodeNibble(s charAt i)
				if (n2 == -1)		return Left(URIComponentInvalid(i))
				
				b	+= ((n1 << 4) | (n2 << 0)).toByte
				i	+= 1
			}
			else {
				b	+= c.toByte
				i	+= 1
			}
		}
		charset decodeEitherByteString (ByteString fromArrayBuffer b) mapLeft URIComponentException
	}
	
	private def decodeNibble(nibble:Char):Int	=
				 if (nibble >= '0' && nibble <= '9')	nibble - '0'
			else if (nibble >= 'a' && nibble <= 'f')	nibble - 'a' + 10
			else if (nibble >= 'A' && nibble <= 'F')	nibble - 'A' + 10
			else										-1
}
