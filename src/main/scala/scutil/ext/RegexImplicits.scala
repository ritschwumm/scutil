package scutil.ext

import scala.util.matching.Regex

import scutil.data.Marshaller

object RegexImplicits extends RegexImplicits

trait RegexImplicits {
    implicit def toRegexExt(delegate:Regex)	= new RegexExt(delegate)
}

final class RegexExt(delegate:Regex) {
	def matches(s:CharSequence):Boolean	= 
			delegate.pattern matcher s matches;
	
	def asMarshaller:Marshaller[String,String]	= 
			Marshaller guarded matches
}
