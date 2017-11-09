package scutil.number.pimp

import scutil.number.BigRational

object IntImplicits extends IntImplicits

trait IntImplicits {
	implicit final class IntExt(peer:Int) {
		def toBigRational:BigRational	=
				BigRational fromLong peer
	}
}
