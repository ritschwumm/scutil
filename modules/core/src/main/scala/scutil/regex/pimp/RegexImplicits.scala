package scutil.regex.pimp

import java.util.regex.Pattern

import scala.util.matching.Regex

import scutil.lang._

object RegexImplicits extends RegexImplicits

trait RegexImplicits {
	implicit final class RegexCompanionExt(peer:Regex.type) {
		def quote(str:String):String	= Pattern quote str
		
		def quoteCharacterClass(str:String):String	= {
			val b	= new StringBuilder
			var i	= 0
			while (i < str.length) {
				val c	= str charAt i
				c match {
					case '\\' | '-' | '^' | '[' | ']'	=> b append '\\'
					case _ 								=>
				}
				b append c
				i	+= 1
			}
			b.toString
		}
	}
	
	implicit final class RegexExt(peer:Regex) {
		def test(s:CharSequence):Boolean	=
				(peer.pattern matcher s).matches
		
		def toPrism:Prism[String,String]	=
				Prism filtered test
	}
}
