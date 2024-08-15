package scutil.number.extension

import java.math.{ BigDecimal	as JBigDecimal }
import scutil.number.BigRational

object JBigDecimalExtensions {
	extension (peer:JBigDecimal) {
		def toBigRational:BigRational	= BigRational.fromJBigDecimal(peer)
	}
}
