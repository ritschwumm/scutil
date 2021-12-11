package scutil.number.extension

import scutil.number.BigRational

object IntImplicits {
	implicit final class IntExt(peer:Int) {
		def toBigRational:BigRational	= BigRational fromLong peer
	}
}
