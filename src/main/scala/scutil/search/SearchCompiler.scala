package scutil.search

import java.util.regex.Pattern

import scutil.lang._
import scutil.Implicits._

object SearchCompiler {
	def compileSeq(pattern:SearchPattern):Predicate[Seq[String]]	= {
		val pos	= pattern.positive map compileToken
		val neg	= pattern.negative map compileToken
		ss:Seq[String]	=> (pos forall ss.exists) && !(neg exists ss.exists)
	}
	
	def compile(pattern:SearchPattern):Predicate[String]	= {
		val pos	= pattern.positive map compileToken
		val neg	= pattern.negative map compileToken
		s:String	=> (pos forall { _ apply s }) && !(neg exists { _ apply s })
	}
	
	private def compileToken(token:SearchToken):Predicate[String]	= {
		val pattern	= Pattern compile (
				(token.start cata ("^", "")) + token.text.quoteRegex + (token.end cata ("$", "")), 
				token.caseInsensitive cata (Pattern.CASE_INSENSITIVE, 0))
		s:String	=>  (pattern matcher s).find
	}
}

/*
def matches(key:String):Boolean	= {
	val value	= if (low)	key.toLowerCase		else key
	val pattern	= if (low)	text.toLowerCase	else text
	(!start			|| (value startsWith	pattern))	&&
	(!end			|| (value endsWith	pattern))		&&
	((start || end)	|| (value contains	pattern))
}
*/