package scutil.text

import scutil.text.pimp._

object implicits extends implicits
trait implicits
		extends	disposables
		with	MatcherImplicits
		with	RegexImplicits
		with	StringContextImplicits
		with	StringImplicits
		with	StringTokenizerImplicits
