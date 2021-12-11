package scutil.lang

import minitest.*

import scutil.lang.assocRight.*

object AssocRightTest extends SimpleTestSuite {
	test("arrow should construct right-associative") {
		val a	= "a" ->: 1 ->: 2L
		assertEquals(
			a,
			("a", (1, 2L))
		)
	}

	test("arrow should destruct right-associative") {
		val a	= "a" ->: 1 ->: 2L
		val b	= a match { case x ->: y ->: z => (x,y,z) }
		assertEquals(
			b,
			("a", 1, 2L)
		)
	}

	test("arrow should type right-associative") {
		val a	= ("a",(1,2L))
		typed[ String ->: Int ->: Long ](a)
		assertEquals(a, a)
	}
}
