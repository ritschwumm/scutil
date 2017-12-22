package scutil.number

import java.math.{ BigInteger => JBigInteger, BigDecimal => JBigDecimal}

import scutil.lang.tc._

object instances extends instances

trait instances {
	implicit val BigIntShow:Show[BigInt]			= Show.toStringInstance
	implicit val BigDecimalShow:Show[BigDecimal]	= Show.toStringInstance
	
	implicit val JBigIntegerShow:Show[JBigInteger]	= Show.toStringInstance
	implicit val JBigDecimalShow:Show[JBigDecimal]	= Show.toStringInstance
}
