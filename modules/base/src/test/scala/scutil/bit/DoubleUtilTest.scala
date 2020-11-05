package scutil.bit

import java.lang.{ Double	=> JDouble }

import org.specs2.mutable._

class DoubleUtilTest extends Specification {
	"DoubleUtil" should {
		"detect normal" in {
			DoubleUtil.denormal(JDouble.MIN_NORMAL) mustEqual false
		}

		"detect denormal" in {
			DoubleUtil.denormal(JDouble.MIN_NORMAL / 2) mustEqual true
		}

		"detect +0 as not denormal" in {
			DoubleUtil.denormal(0d) mustEqual false
		}

		"detect -0 as not denormal" in {
			DoubleUtil.denormal(-0d) mustEqual false
		}

		"detect Infinity as not denormal" in {
			DoubleUtil.denormal(1d / 0d) mustEqual false
		}

		"detect NaN as not denormal" in {
			DoubleUtil.denormal(0d / 0d) mustEqual false
		}

		"flush denormal to zero" in {
			DoubleUtil.ftz(JDouble.MIN_NORMAL / 2) mustEqual 0d
		}
	 }
}
