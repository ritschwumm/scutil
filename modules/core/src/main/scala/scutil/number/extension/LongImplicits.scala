package scutil.number.extension

import scutil.number.BigRational

object LongImplicits extends LongImplicits

trait LongImplicits {
	implicit final class LongExt(peer:Long) {
		def toBigRational:BigRational	= BigRational fromLong peer
	}
}
