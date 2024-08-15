package scutil.lang

import minitest.*

object CompanionTest extends SimpleTestSuite {
	object Target {
		def hello = "world"
	}
	class Target {}

	//-----------------------------------------------------------------------------

	test("Companion should work") {
		assertEquals(
			Companion.of[Target].hello,
			"world"
		)
	}
}
