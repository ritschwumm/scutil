package scutil.collection

import minitest._

import scutil.collection.implicits._

object ListImplicitsTest extends SimpleTestSuite {
	test("unprefix should leave the prefix empty for one empty list") {
		assertEquals(
			Nil unprefix List(1),
			((Nil, Nil, List(1)))
		)
	}

	test("unprefix should leave the prefix empty for another empty list") {
		assertEquals(
			List(1) unprefix Nil,
			((Nil, List(1), Nil))
		)
	}

	test("unprefix should find a common prefix") {
		assertEquals(
			List(1,2,10,11) unprefix List(1,2,20,21),
			((List(1,2), List(10,11), List(20,21)))
		)
	}
}
