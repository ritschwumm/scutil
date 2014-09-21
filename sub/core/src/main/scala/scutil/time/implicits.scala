package scutil.time

import scutil.time.pimp._

object implicits extends implicits
trait implicits
		extends	DateImplicits
		with	DateFormatImplicits
		with	LongImplicits
