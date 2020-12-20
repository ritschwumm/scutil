package scutil.regex

import scutil.regex.extension._

object extensions extends extensions

trait extensions
	extends	MatcherImplicits
	with	RegexImplicits
