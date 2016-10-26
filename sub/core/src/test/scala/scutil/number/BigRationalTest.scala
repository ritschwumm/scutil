package scutil.number

import org.specs2.mutable._

import scutil.number.implicits._

class BigRationalTest extends Specification {
	"bigrational macros " should {
		"compile" in {
			br"7/11" mustEqual BigRational(7,11)
		}
	}
}
