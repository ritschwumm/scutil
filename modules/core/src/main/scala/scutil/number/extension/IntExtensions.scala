package scutil.number.extension

import scutil.number.BigRational

object IntExtensions {
	implicit final class IntExt(peer:Int) {
		def toBigRational:BigRational	= BigRational fromLong peer
	}
}
