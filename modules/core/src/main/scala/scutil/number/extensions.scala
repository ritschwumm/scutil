package scutil.number

import scutil.number.extension._

object extensions extends extensions
trait extensions
	extends	RandomImplicits
	with	IntImplicits
	with	LongImplicits
	with	JBigIntegerImplicits
	with	JBigDecimalImplicits
