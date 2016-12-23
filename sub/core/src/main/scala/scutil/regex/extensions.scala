package scutil.regex

import scutil.regex.pimp._

object extensions extends extensions
trait extensions
		extends	MatcherImplicits
		with	RegexImplicits
		with	StringContextImplicits
		with	StringImplicits
