package scutil.regex.pimp

import scala.language.experimental.macros

import scala.util.matching.Regex

object StringContextImplicits extends StringContextImplicits

trait StringContextImplicits {
	implicit final class RegexStringContextExt(peer:StringContext) {
		def re():Regex	= macro RegexMacros.reImpl
	}
}
