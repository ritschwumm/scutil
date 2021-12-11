package scutil.regex.extension

import scala.language.experimental.macros

import scala.util.matching.Regex

object StringContextImplicits extends StringContextImplicits

trait StringContextImplicits {
	extension (inline peer:StringContext) {
		inline def re(inline parts:Any*):Regex	= ${ RegexMacros.re('peer) }
	}
}
