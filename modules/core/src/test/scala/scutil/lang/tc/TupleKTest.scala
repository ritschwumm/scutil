package scutil.lang.tc

import minitest.*

// import scutil.core.implicits.*
import scutil.lang.*
import scutil.lang.tc.TupleK

object TupleKTest extends SimpleTestSuite {
	test("TupleK.productN should work") {
		assertEquals(
			TupleK.productN((Vector(1), Vector("a", "b"))),
			Vector((1, "a"), (1, "b"))
		)
	}

	test("TupleK.mapN should work") {
		assertEquals(
			TupleK.mapN((Vector(1), Vector("a", "b")))((a, b) => a.toString + " " + b),
			Vector("1 a", "1 b")
		)
	}

	test("TupleK.traverseN should work") {
		assertEquals(
			TupleK.traverseN(1, "a")([A] => (a:A) => Vector(a)),
			Vector((1, "a"))
		)
	}

	test("TupleK.mapK should work") {
		// TODO dotty has to be in its own variable - why?
		// TODO dotty without the type-ascription, this fails, too
		// val nat1:Vector ~> Option	= [A] => (a:Vector[A]) => a.headOption
		val nat2	= [A] => (a:Vector[A]) => a.headOption
		val nat3:Vector ~> Option	= nat2

		assertEquals(
			TupleK.mapK((Vector(1), Vector("a", "b")))(nat3),
			(Some(1), Some("a"))
		)
	}

	test("TupleK.traverseK should work") {
		// TODO dotty has to be in its own variable - why?
		// TODO dotty without the type-ascription, this fails, too
		val nat:Vector ~> Option	= [A] => (a:Vector[A]) => a.headOption

		assertEquals(
			TupleK.traverseK((Vector(1), Vector("a", "b")))(nat),
			Some((1, "a"))
		)
	}
}
