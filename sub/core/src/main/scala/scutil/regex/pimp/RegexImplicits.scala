package scutil.regex.pimp

import scala.util.matching.Regex

import scutil.lang._

object RegexImplicits extends RegexImplicits

trait RegexImplicits {
	implicit final class RegexExt(peer:Regex) {
		def test(s:CharSequence):Boolean	=
				(peer.pattern matcher s).matches
		
		def toPrism:Prism[String,String]	=
				Prism guarded test
	}
}
