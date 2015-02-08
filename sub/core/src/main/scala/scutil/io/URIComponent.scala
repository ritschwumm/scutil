package scutil.io

import java.net._
import java.nio.charset.Charset

import scutil.io.Charsets.utf_8

object URIComponent {
	val charset	= utf_8
	
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
	
	def decode(s:String):String =
			URLDecoder decode (
					s replace ("+", "%2B"),
					charset.name)
}
