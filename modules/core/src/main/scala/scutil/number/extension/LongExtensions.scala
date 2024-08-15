package scutil.number.extension

import scutil.number.BigRational

object LongExtensions {
	extension (peer:Long) {
		def toBigRational:BigRational	= BigRational.fromLong(peer)
	}
}
