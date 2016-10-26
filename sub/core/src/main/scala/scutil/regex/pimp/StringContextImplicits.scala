package scutil.regex.pimp

import scala.language.experimental.macros

import scala.util.matching.Regex

object StringContextImplicits extends StringContextImplicits

trait StringContextImplicits {
    implicit def toRegexStringContextExt(peer:StringContext)	= new StringContextExt(peer)
}

final class StringContextExt(peer:StringContext) {
	def re():Regex					= macro RegexMacros.reImpl
}
