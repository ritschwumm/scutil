package scutil.collection

import minitest.*

import scutil.collection.implicits.*

object IterableFactoryExtensionsTest extends SimpleTestSuite {
	test("unfoldSimple should work on List") {
		assertEquals(
			List.unfoldSimple(1) { it =>
				if (it < 5)	Some(it + 1) else None
			},
			List(2,3,4,5)
		)
	}

	test("unfoldSimple should work on Vector") {
		assertEquals(
			Vector.unfoldSimple(1) { it =>
				if (it < 5)	Some(it + 1) else None
			},
			Vector(2,3,4,5)
		)
	}
}
