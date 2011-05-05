package scutil

import java.nio.charset.Charset

object Charsets {
	val platform	= Charset forName Platform.fileEncoding
	
	val	us_ascii	= Charset forName "US-ASCII"
	
	val	iso_8859_1	= Charset forName "ISO-8859-1"
	//val	iso_8859_15	= Charset forName "ISO-8859-15"
	
	val	utf_8		= Charset forName "UTF-8"
	val utf_16		= Charset forName "UTF-16"
	val utf_16be	= Charset forName "UTF-16BE"
	val utf_16le	= Charset forName "UTF-16LE"
}
