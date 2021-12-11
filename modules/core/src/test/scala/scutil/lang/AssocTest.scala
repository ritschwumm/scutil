package scutil.lang

import minitest.*

import scutil.lang.assoc.*

object AssocTest extends SimpleTestSuite {
	test("arrow should construct") {
		val a	= "a" -> 1 -> 3L
		assertEquals(
			a,
			(("a",1),3L)
		)
	}

	test("arrow should destruct") {
		val a	= (("a",1),3L)
		val b	= a match { case x -> y -> z => ((x,y),z) }
		assertEquals(a, b)
	}

	test("arrow should type") {
		val a	= (("a",1),3L)
		typed[ String -> Int -> Long ](a)
		assertEquals(a, a)
	}
}
