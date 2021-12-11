package scutil.regex.extension

import java.util.regex.Matcher

object MatcherImplicits {
	implicit final class MatcherExt(peer:Matcher) {
		def matchedString:Option[String]	=
			if (peer.matches)	Some(peer group 0)
			else 				None

		def matchedGroups:Option[Seq[Option[String]]]	=
			if (peer.matches)	Some((1 to peer.groupCount map peer.group map Option.apply).toVector)
			else 				None
	}
}
