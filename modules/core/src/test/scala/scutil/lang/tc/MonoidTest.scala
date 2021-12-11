package scutil.lang.tc

import minitest._

import scutil.lang.tc.MonoidSyntax._

object MonoidTest extends SimpleTestSuite {
	test("monoid should support times 0 on string") {
		assertEquals(
			"hallo".times(0),
			""
		)
	}

	test("monoid should support times n on string") {
		assertEquals(
			"hallo".times(3),
			"hallohallohallo"
		)
	}

	//------------------------------------------------------------------------------

	// TODO tc times() exists as a pimped method on Seq, too
	test("monoid should support times 0 on vector") {
		assertEquals(
			Vector(1,2,3).times(0),
			Vector()
		)
	}

	test("monoid should support times n on vector") {
		assertEquals(
			Vector(1,2,3).times(3),
			Vector(1,2,3,1,2,3,1,2,3)
		)
	}
}
