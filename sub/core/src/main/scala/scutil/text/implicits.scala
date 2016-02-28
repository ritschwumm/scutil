package scutil.text

import scutil.text.pimp._

object implicits extends implicits
trait implicits
		extends	MatcherImplicits
		with	RegexImplicits
		with	StringContextImplicits
		with	StringImplicits
		with	StringTokenizerImplicits
