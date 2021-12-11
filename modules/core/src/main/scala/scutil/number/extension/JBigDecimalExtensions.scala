package scutil.number.extension

import java.math.{ BigDecimal	as JBigDecimal }
import scutil.number.BigRational

object JBigDecimalExtensions {
	implicit final class JBigDecimalExt(peer:JBigDecimal) {
		def toBigRational:BigRational	= BigRational fromJBigDecimal peer
	}
}
