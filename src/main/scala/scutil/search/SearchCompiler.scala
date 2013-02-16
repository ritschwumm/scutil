package scutil.search

import java.util.regex.Pattern

import scutil.lang._
import scutil.Implicits._

object SearchCompiler {
	def compileSeq(pattern:SearchPattern):Predicate[Iterable[String]]	= {
		val pos	= pattern.positive map compileToken
		val neg	= pattern.negative map compileToken
		ss:Iterable[String]	=> (pos forall ss.exists) && !(neg exists ss.exists)
	}
	
	def compile(pattern:SearchPattern):Predicate[String]	= {
		val pos	= pattern.positive map compileToken
		val neg	= pattern.negative map compileToken
		s:String	=> (pos forall { _ apply s }) && !(neg exists { _ apply s })
	}
	
	private def compileToken(token:SearchToken):Predicate[String]	= {
		val pattern	= Pattern compile (
				(token.start cataSwapped ("^", "")) + token.text.quoteRegex + (token.end cataSwapped ("$", "")), 
				token.caseInsensitive cataSwapped (Pattern.CASE_INSENSITIVE, 0))
		s:String	=> (pattern matcher s).find
	}
}
