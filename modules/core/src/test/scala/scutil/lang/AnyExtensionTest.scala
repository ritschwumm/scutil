package scutil.lang

import minitest.*

import scutil.core.implicits.*

object AnyExtensionTest extends SimpleTestSuite {
	test("one puts into a Vector") {
		assertEquals(
			"foo".one(Vector),
			Vector("foo")
		)
	}

	test("one puts into a List") {
		assertEquals(
			"foo".one(List),
			List("foo")
		)
	}
}
