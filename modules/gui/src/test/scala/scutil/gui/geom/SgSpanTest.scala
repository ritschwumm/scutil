package scutil.gui.geom

import minitest.*

object SgSpanTest extends SimpleTestSuite {
	test("SgSpan should intersect nothing when empty") {
		assertEquals(
			SgSpan.startEnd(1, 1) intersects SgSpan.startEnd(0, 2),
			false
		)
	}
	test("SgSpan should intersect nothing when empty") {
		assertEquals(
			SgSpan.startEnd(0, 2) intersects SgSpan.startEnd(1, 1),
			false
		)
	}

	test("SgSpan should intersect itself") {
		assertEquals(
			SgSpan.startEnd(0, 1) intersects SgSpan.startEnd(0, 1),
			true
		)
	}

	test("SgSpan should not intersect left to left") {
		assertEquals(
			SgSpan.startEnd(0, 1) intersects SgSpan.startEnd(2, 4),
			false
		)
	}
	test("SgSpan should not intersect left to start") {
		assertEquals(
			SgSpan.startEnd(0, 2) intersects SgSpan.startEnd(2, 4),
			false
		)
	}
	test("SgSpan should intersect left to inside") {
		assertEquals(
			SgSpan.startEnd(0, 3) intersects SgSpan.startEnd(2, 4),
			true
		)
	}
	test("SgSpan should intersect left to end") {
		assertEquals(
			SgSpan.startEnd(0, 4) intersects SgSpan.startEnd(2, 4),
			true
		)
	}
	test("SgSpan should intersect left to right") {
		assertEquals(
			SgSpan.startEnd(0, 5) intersects SgSpan.startEnd(2, 4),
			true
		)
	}

	test("SgSpan should intersect start to inside") {
		assertEquals(
			SgSpan.startEnd(2, 3) intersects SgSpan.startEnd(2, 4),
			true
		)
	}
	test("SgSpan should intersect start to end") {
		assertEquals(
			SgSpan.startEnd(2, 4) intersects SgSpan.startEnd(2, 4),
			true
		)
	}
	test("SgSpan should intersect start to right") {
		assertEquals(
			SgSpan.startEnd(2, 5) intersects SgSpan.startEnd(2, 4),
			true
		)
	}

	test("SgSpan should intersect inside to end") {
		assertEquals(
			SgSpan.startEnd(3, 4) intersects SgSpan.startEnd(2, 4),
			true
		)
	}
	test("SgSpan should intersect inside to right") {
		assertEquals(
			SgSpan.startEnd(3, 5) intersects SgSpan.startEnd(2, 4),
			true
		)
	}

	test("SgSpan should not intersect end to right") {
		assertEquals(
			SgSpan.startEnd(4, 5) intersects SgSpan.startEnd(2, 4),
			false
		)
	}

	test("SgSpan should not intersect right to right") {
		assertEquals(
			SgSpan.startEnd(5, 6) intersects SgSpan.startEnd(2, 4),
			false
		)
	}

	//------------------------------------------------------------------------------

	test("SgSpan should intersect nothing when empty") {
		assertEquals(
			SgSpan.startEnd(1, 1) intersect SgSpan.startEnd(0, 2),
			None
		)
	}
	test("SgSpan should intersect nothing when empty") {
		assertEquals(
			SgSpan.startEnd(0, 2) intersect SgSpan.startEnd(1, 1),
			None
		)
	}

	test("SgSpan should intersect itself") {
		assertEquals(
			SgSpan.startEnd(0, 1) intersect SgSpan.startEnd(0, 1),
			Some(SgSpan.startEnd(0, 1))
		)
	}

	test("SgSpan should not intersect left to left") {
		assertEquals(
			SgSpan.startEnd(0, 1) intersect SgSpan.startEnd(2, 4),
			None
		)
	}
	test("SgSpan should not intersect left to start") {
		assertEquals(
			SgSpan.startEnd(0, 2) intersect SgSpan.startEnd(2, 4),
			None
		)
	}
	test("SgSpan should intersect left to inside") {
		assertEquals(
			SgSpan.startEnd(0, 3) intersect SgSpan.startEnd(2, 4),
			Some(SgSpan.startEnd(2,3))
		)
	}
	test("SgSpan should intersect left to end") {
		assertEquals(
			SgSpan.startEnd(0, 4) intersect SgSpan.startEnd(2, 4),
			Some(SgSpan.startEnd(2,4))
		)
	}
	test("SgSpan should intersect left to right") {
		assertEquals(
			SgSpan.startEnd(0, 5) intersect SgSpan.startEnd(2, 4),
			Some(SgSpan.startEnd(2,4))
		)
	}

	test("SgSpan should intersect start to inside") {
		assertEquals(
			SgSpan.startEnd(2, 3) intersect SgSpan.startEnd(2, 4),
			Some(SgSpan.startEnd(2,3))
		)
	}
	test("SgSpan should intersect start to end") {
		assertEquals(
			SgSpan.startEnd(2, 4) intersect SgSpan.startEnd(2, 4),
			Some(SgSpan.startEnd(2,4))
		)
	}
	test("SgSpan should intersect start to right") {
		assertEquals(
			SgSpan.startEnd(2, 5) intersect SgSpan.startEnd(2, 4),
			Some(SgSpan.startEnd(2,4))
		)
	}

	test("SgSpan should intersect inside to end") {
		assertEquals(
			SgSpan.startEnd(3, 4) intersect SgSpan.startEnd(2, 4),
			Some(SgSpan.startEnd(3,4))
		)
	}
	test("SgSpan should intersect inside to right") {
		assertEquals(
			SgSpan.startEnd(3, 5) intersect SgSpan.startEnd(2, 4),
			Some(SgSpan.startEnd(3,4))
		)
	}

	test("SgSpan should not intersect end to right") {
		assertEquals(
			SgSpan.startEnd(4, 5) intersect SgSpan.startEnd(2, 4),
			None
		)
	}

	test("SgSpan should not intersect right to right") {
		assertEquals(
			SgSpan.startEnd(5, 6) intersect SgSpan.startEnd(2, 4),
			None
		)
	}
}
