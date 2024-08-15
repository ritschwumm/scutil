package scutil.regex.extension

import java.util.regex.*

import scala.util.matching.Regex

import scutil.lang.*

object RegexExtensions {
	extension (peer:Regex.type) {
		def prism:Prism[String,Regex]	=
			Prism(compile(_).toOption, _.pattern.toString)

		def compile(str:String):Either[PatternSyntaxException,Regex]	=
			try		{ Right(new Regex(str)) }
			catch	{ case e:PatternSyntaxException => Left(e) }

		def validate(s:String):Boolean	=
			compile(s).isRight

		//------------------------------------------------------------------------------

		def quoteCharacterClass(str:String):String	= {
			val b	= new StringBuilder
			var i	= 0
			while (i < str.length) {
				val c	= str.charAt(i)
				c match {
					case '\\' | '-' | '^' | '[' | ']'	=> b append '\\'
					case _								=>
				}
				b append c
				i	+= 1
			}
			b.toString
		}
	}

	extension (peer:Regex) {
		def test(s:CharSequence):Boolean	=
			peer.pattern.matcher(s).matches

		def toPrism:Prism[String,String]	=
			Prism.filtered(test)

		def removeAllFrom(s:String):String	=
			peer.replaceAllIn(s, "")
	}
}
