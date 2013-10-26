package scutil.pimp

import scala.util.matching.Regex

import scutil.lang._

object RegexImplicits extends RegexImplicits

trait RegexImplicits {
    implicit def toRegexExt(peer:Regex)	= new RegexExt(peer)
}

final class RegexExt(peer:Regex) {
	def matches(s:CharSequence):Boolean	= 
			(peer.pattern matcher s).matches
	
	def asMarshaller:Marshaller[String,String]	= 
			Marshaller guarded matches
}
