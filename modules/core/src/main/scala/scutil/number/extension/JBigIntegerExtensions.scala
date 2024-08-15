package scutil.number.extension

import java.math.{ BigInteger	as JBigInteger }
import scutil.number.BigRational

object JBigIntegerExtensions {
	extension (peer:JBigInteger) {
		def toBigRational:BigRational	= BigRational.fromJBigInteger(peer)
	}
}
