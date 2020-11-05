package scutil.number.extension

import java.math.{ BigInteger	=> JBigInteger }
import scutil.number.BigRational

object JBigIntegerImplicits extends JBigIntegerImplicits

trait JBigIntegerImplicits {
	implicit final class JBigIntegerExt(peer:JBigInteger) {
		def toBigRational:BigRational	= BigRational fromJBigInteger peer
	}
}
