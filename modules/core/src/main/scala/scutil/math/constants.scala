package scutil.math

import java.lang.{ Math as JMath }

object constants {
	val E				= JMath.E

	val Pi				= JMath.PI
	val HalfPi			= Pi / 2
	val TwicePi			= Pi * 2

	val SqrtTwo			= JMath.sqrt(2)
	val SqrtHalf		= JMath.sqrt(0.5)

	val Log2			= JMath.log(2)
	val Log2Reciprocal	= 1.0 / Log2

	val Log10			= JMath.log(10)
	val Log10Reciprocal	= 1.0 / Log10
}
