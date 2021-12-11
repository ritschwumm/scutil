package scutil.bit

import java.lang.{ Float	=> JFloat }

import minitest.*

object FloatUtilTest extends SimpleTestSuite {
	test("FloatUtil should detect normal") {
		assertEquals(
			FloatUtil.denormal(JFloat.MIN_NORMAL),
			false
		)
	}

	test("FloatUtil should detect denormal") {
		assertEquals(
			FloatUtil.denormal(JFloat.MIN_NORMAL / 2),
			true
		)
	}

	test("FloatUtil should detect +0 as not denormal") {
		assertEquals(
			FloatUtil.denormal(0f),
			false
		)
	}

	test("FloatUtil should detect -0 as not denormal") {
		assertEquals(
			FloatUtil.denormal(-0f),
			false
		)
	}

	test("FloatUtil should detect Infinity as not denormal") {
		assertEquals(
			FloatUtil.denormal(1f / 0f),
			false
		)
	}

	test("FloatUtil should detect NaN as not denormal") {
		assertEquals(
			FloatUtil.denormal(0f / 0f),
			false
		)
	}

	test("FloatUtil should flush denormal to zero") {
		assertEquals(
			FloatUtil.ftz(JFloat.MIN_NORMAL / 2),
			0f
		)
	}
}
