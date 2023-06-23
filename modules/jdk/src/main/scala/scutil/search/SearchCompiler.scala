package scutil.search

import java.util.regex.Pattern

import scala.util.matching.Regex

import scutil.lang.*
import scutil.core.implicits.*

object SearchCompiler {
	def multi(pattern:SearchPattern):Predicate[Iterable[String]]	=
		ss	=> search(pattern, ss.exists _)

	def single(pattern:SearchPattern):Predicate[String]	=
		s	=> search(pattern, s into _)

	private def search(pattern:SearchPattern, here:Predicate[String]=>Boolean) =
		(pattern.positive map token forall here) && !(pattern.negative map token exists here)

	private def token(token:SearchToken):Predicate[String]	= {
		val pattern	=
			Pattern.compile(
				token.start.cata("", "^") + Regex.quote(token.text) + token.end.cata("", "$"),
				// TODO search this is not unicode-aware, for that we'd need UNICODE_CASE
				token.caseInsensitive.cata(0, Pattern.CASE_INSENSITIVE)
			)

		(s:String)	=> pattern.matcher(s).find
	}
}
