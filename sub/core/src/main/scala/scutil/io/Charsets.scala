package scutil.io

import java.nio.charset.Charset

import scutil.lang._
import scutil.platform.SystemProperties

/** charsets existing on all platforms */
object Charsets {
	val platform	= Charset forName SystemProperties.file.encoding
	
	val	us_ascii	= Charset forName "US-ASCII"
	
	val	iso_8859_1	= Charset forName "ISO-8859-1"
	//val	iso_8859_15	= Charset forName "ISO-8859-15"
	
	val	utf_8		= Charset forName "UTF-8"
	val utf_16		= Charset forName "UTF-16"
	val utf_16be	= Charset forName "UTF-16BE"
	val utf_16le	= Charset forName "UTF-16LE"
	
	/** Fail when then name is illegal or the charset is not supported */
	def byName(name:String):Tried[IllegalArgumentException,Charset]	=
			Catch.byType[IllegalArgumentException] in { Charset forName name }
}
