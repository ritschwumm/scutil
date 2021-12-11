package scutil.regex.extension

import scala.language.experimental.macros

import scala.util.matching.Regex

object StringContextImplicits {
	extension (inline peer:StringContext) {
		inline def re(inline parts:Any*):Regex	= ${ RegexMacros.re('peer) }
	}
}
