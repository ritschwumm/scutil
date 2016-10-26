package scutil.codec

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

import scutil.lang._
import scutil.platform.SystemProperties

/** charsets existing on all platforms */
object Charsets {
	val platform	= Charset forName SystemProperties.file.encoding
	
	val us_ascii	= StandardCharsets.US_ASCII
	val iso_8859_1	= StandardCharsets.ISO_8859_1
	val utf_8		= StandardCharsets.UTF_8
	val utf_16		= StandardCharsets.UTF_16
	val utf_16be	= StandardCharsets.UTF_16BE
	val utf_16le	= StandardCharsets.UTF_16LE
	
	/** Fail when then name is illegal or the charset is not supported */
	def byName(name:String):Tried[IllegalArgumentException,Charset]	=
			Catch.byType[IllegalArgumentException] in { Charset forName name }
}
