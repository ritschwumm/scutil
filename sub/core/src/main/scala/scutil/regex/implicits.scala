package scutil.regex

import scutil.regex.pimp._

object implicits extends implicits
trait implicits
		extends	MatcherImplicits
		with	RegexImplicits
		with	StringContextImplicits
		with	StringImplicits
