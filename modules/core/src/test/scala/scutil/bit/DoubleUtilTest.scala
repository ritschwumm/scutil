package scutil.bit

import java.lang.{ Double	=> JDouble }

import minitest.*

object DoubleUtilTest extends SimpleTestSuite {
	test("DoubleUtil should detect normal") {
		assertEquals(
			DoubleUtil.denormal(JDouble.MIN_NORMAL),
			false
		)
	}

	test("DoubleUtil should detect denormal") {
		assertEquals(
			DoubleUtil.denormal(JDouble.MIN_NORMAL / 2),
			true
		)
	}

	test("DoubleUtil should detect +0 as not denormal") {
		assertEquals(
			DoubleUtil.denormal(0d),
			false
		)
	}

	test("DoubleUtil should detect -0 as not denormal") {
		assertEquals(
			DoubleUtil.denormal(-0d),
			false
		)
	}

	test("DoubleUtil should detect Infinity as not denormal") {
		assertEquals(
			DoubleUtil.denormal(1d / 0d),
			false
		)
	}

	test("DoubleUtil should detect NaN as not denormal") {
		assertEquals(
			DoubleUtil.denormal(0d / 0d),
			false
		)
	}

	test("DoubleUtil should flush denormal to zero") {
		assertEquals(
			DoubleUtil.ftz(JDouble.MIN_NORMAL / 2),
			0d
		)
	}
}
