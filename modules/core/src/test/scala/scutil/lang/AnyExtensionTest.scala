package scutil.lang

import minitest._

import scutil.core.implicits._

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
