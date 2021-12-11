package scutil.number.extension

import java.math.{ BigDecimal	=> JBigDecimal }
import scutil.number.BigRational

object JBigDecimalImplicits {
	implicit final class JBigDecimalExt(peer:JBigDecimal) {
		def toBigRational:BigRational	= BigRational fromJBigDecimal peer
	}
}
