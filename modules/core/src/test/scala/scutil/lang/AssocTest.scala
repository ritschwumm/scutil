package scutil.lang

import minitest._

import scutil.lang.assoc._

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

	//------------------------------------------------------------------------------

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
