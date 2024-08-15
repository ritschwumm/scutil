package scutil.number.extension

import scutil.number.BigRational

object IntExtensions {
	extension (peer:Int) {
		def toBigRational:BigRational	= BigRational.fromLong(peer)
	}
}
