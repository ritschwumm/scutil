package scutil.regex.pimp

import java.util.regex.Pattern

object StringImplicits extends StringImplicits

trait StringImplicits {
	implicit def toRegexStringExt(peer:String) = new StringExt(peer)
}

final class StringExt(peer:String) {
	/** quote to use as a literal in a Regex */
	def quoteRegex:String	= Pattern quote peer
	
	/** quote to use within a character class in a Regex */
	def quoteCharacterClass:String	= {
		val b	= new StringBuilder
		var i	= 0
		while (i < peer.length) {
			peer charAt i match {
				case '\\'	=> b append "\\\\"
				case '-'	=> b append "\\-"
				case '^'	=> b append "\\^"
				case '['	=> b append "\\["
				case ']'	=> b append "\\]"
				case x		=> b += x
			}
			i	+= 1
		}
		b.toString
	}
}
