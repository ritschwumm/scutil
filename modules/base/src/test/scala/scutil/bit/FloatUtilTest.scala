package scutil.bit

import java.lang.{ Float	=> JFloat }

import org.specs2.mutable._

class FloatUtilTest extends Specification {
	"FloatUtil" should {
		"detect normal" in {
			FloatUtil.denormal(JFloat.MIN_NORMAL) mustEqual false
		}

		"detect denormal" in {
			FloatUtil.denormal(JFloat.MIN_NORMAL / 2) mustEqual true
		}

		"detect +0 as not denormal" in {
			FloatUtil.denormal(0f) mustEqual false
		}

		"detect -0 as not denormal" in {
			FloatUtil.denormal(-0f) mustEqual false
		}

		"detect Infinity as not denormal" in {
			FloatUtil.denormal(1f / 0f) mustEqual false
		}

		"detect NaN as not denormal" in {
			FloatUtil.denormal(0f / 0f) mustEqual false
		}

		"flush denormal to zero" in {
			FloatUtil.ftz(JFloat.MIN_NORMAL / 2) mustEqual 0f
		}
	}
}
