package scutil.lang

import minitest.*

import scutil.core.implicits.*

object TupleNSyntaxTest extends SimpleTestSuite {
	val f	= (a:Int)	=> a*a

	test("tuple apply syntax should work for 1 element") {
		assertEquals(
			Tuple1(f).applyN(Tuple1(2)),
			Tuple1(4)
		)
	}

	test("tuple apply syntax should work for 2 elements") {
		assertEquals(
			Tuple2(f, f).applyN(Tuple2(2, 3)),
			Tuple2(4, 9)
		)
	}

	test("tuple apply syntax should work for 3 elements") {
		assertEquals(
			Tuple3(f, f, f).applyN(Tuple3(2, 3, 4)),
			Tuple3(4, 9, 16)
		)
	}
}
