package scutil.lang

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

/** charsets existing on all platforms */
object Charsets {
	val us_ascii	= StandardCharsets.US_ASCII
	val iso_8859_1	= StandardCharsets.ISO_8859_1
	val utf_8		= StandardCharsets.UTF_8
	val utf_16		= StandardCharsets.UTF_16
	val utf_16be	= StandardCharsets.UTF_16BE
	val utf_16le	= StandardCharsets.UTF_16LE

	/** Fail when then name is illegal or the Charset is not supported */
	def byName(name:String):Either[IllegalArgumentException,Charset]	=
		Catch.byType[IllegalArgumentException] in { Charset.forName(name) }
}
