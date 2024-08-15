package scutil.math

import java.lang.{ Math as JMath }

object constants {
	val Pi			= JMath.PI
	val PiHalf		= Pi / 2
	val PiDouble	= Pi * 2

	val E			= JMath.E

	val Log2			= JMath.log(2)
	val Log2Reciprocal	= 1.0 / Log2

	val Log10			= JMath.log(10)
	val Log10Reciprocal	= 1.0 / Log10
}
