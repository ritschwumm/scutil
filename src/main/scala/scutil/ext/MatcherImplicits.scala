package scutil.ext

import java.util.regex.Matcher

object MatcherImplicits extends MatcherImplicits

trait MatcherImplicits {
	implicit def toMatcherExt(delegate:Matcher):MatcherExt	= new MatcherExt(delegate)
}
	
final class MatcherExt(delegate:Matcher) {
	def matchedString:Option[String]	=
			if (delegate.matches)	Some(delegate group 0)
			else 					None
		
	def matchedGroups:Option[Seq[Option[String]]]	=
			if (delegate.matches)	Some(1 to delegate.groupCount map delegate.group map Option.apply toSeq)
			else 					None
}
