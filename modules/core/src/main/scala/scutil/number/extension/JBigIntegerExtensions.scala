package scutil.number.extension

import java.math.{ BigInteger	as JBigInteger }
import scutil.number.BigRational

object JBigIntegerExtensions {
	implicit final class JBigIntegerExt(peer:JBigInteger) {
		def toBigRational:BigRational	= BigRational fromJBigInteger peer
	}
}
