package scutil.number.extension

import scutil.number.BigRational

object LongExtensions {
	implicit final class LongExt(peer:Long) {
		def toBigRational:BigRational	= BigRational.fromLong(peer)
	}
}
