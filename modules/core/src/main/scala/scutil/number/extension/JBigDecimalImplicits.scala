package scutil.number.extension

import java.math.{ BigDecimal	=> JBigDecimal }
import scutil.number.BigRational

object JBigDecimalImplicits extends JBigDecimalImplicits

trait JBigDecimalImplicits {
	implicit final class JBigDecimalExt(peer:JBigDecimal) {
		def toBigRational:BigRational	= BigRational fromJBigDecimal peer
	}
}
