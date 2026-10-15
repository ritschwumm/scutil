package scutil.lang

import minitest.*

import scutil.core.implicits.*
import scutil.lang.extension.TupleExtensions

object TupleExtensionsTest extends SimpleTestSuite {
	type TestS	= (String, Int, Boolean)
	val s:TestS	= ("hello", 1, true)

	type TestF	= (String => Int, Int => Boolean, Boolean => String)
	val f:TestF	= (
		(_:String)	=> 1,
		(_:Int)		=> false,
		(_:Boolean)	=> "world",
	)

	type TestT	= (Number, Boolean, String)
	val t:TestT	= (1, false, "world")

	val tt:TupleExtensions.Applied[TestS, TestF]	= (1, false, "world")

	//-----------------------------------------------------------------------------

	test("Tuple.ap should work for 1 element") {
		assertEquals(
			Tuple1("hello").ap(Tuple1(
				(_:String)	=> 1)
			),
			Tuple1(1)
		)
	}

	test("Tuple.ap should work for 2 elements") {
		assertEquals(
			Tuple2("hello", 1).ap(Tuple2(
				(_:String)	=> 1,
				(_:Int)		=> false,
			)),
			Tuple2(1, false)
		)
	}

	test("Tuple.ap should work for 3 elements") {
		assertEquals(
			Tuple3("hello", 1, true).ap(Tuple3(
				(_:String)	=> 1,
				(_:Int)		=> false,
				(_:Boolean)	=> "world"
			)),
			Tuple3(1, false, "world")
		)
	}

	//-----------------------------------------------------------------------------

	test("Tuple.productN should deal with different types") {
		assertEquals(
			(Vector(1), Vector("1"), Vector(true)).productN,
			Vector((1, "1", true))
		)
	}

	//-----------------------------------------------------------------------------

	test("Tuple.productN should zip 1 element") {
		assertEquals(
			Tuple1(Vector(1)).productN,
			Vector(Tuple1(1))
		)
	}

	test("Tuple.productN should zip 2 elements") {
		assertEquals(
			(Vector(1), Vector(2)).productN,
			Vector((1, 2))
		)
	}

	test("Tuple.productN should zip 3 elements") {
		assertEquals(
			(Vector(1), Vector(2), Vector(3)).productN,
			Vector((1, 2, 3))
		)
	}

	//-----------------------------------------------------------------------------

	test("Tuple.productN should zip vector") {
		assertEquals(
			(Vector(1), Vector(2), Vector(3)).productN,
			Vector((1, 2, 3))
		)
	}

	test("Tuple.productN should zip option") {
		assertEquals(
			(Option.some(1), Option.some(2), Option.some(3)).productN,
			Some((1, 2, 3))
		)
	}

	test("Tuple.productN should zip either") {
		assertEquals(
			(Either.right[String, Int](1), Either.right[String, Int](2), Either.right[String, Int](3)).productN,
			Right((1, 2, 3))
		)
	}

	test("Tuple.productN should zip validated") {
		assertEquals(
			(Validated.valid[String, Int](1), Validated.valid[String, Int](2), Validated.valid[String, Int](3)).productN,
			Validated.valid((1, 2, 3))
		)
	}

	//-----------------------------------------------------------------------------

	test("Tuple.mapN should deal with different types") {
		assertEquals(
			(Vector(1), Vector("1"), Vector(true)).mapN((a, b, c) => (c, b, a)),
			Vector((true, "1", 1))
		)
	}

	//-----------------------------------------------------------------------------

	test("Tuple.mapN should map 1 element") {
		assertEquals(
			Tuple1(Vector(1)).mapN((a) => Tuple1(a)),
			Vector(Tuple1(1))
		)
	}

	test("Tuple.mapN should map 2 elements") {
		assertEquals(
			(Vector(1), Vector(2)).mapN((a, b) => (a, b)),
			Vector((1, 2))
		)
	}

	test("Tuple.mapN should map 3 elements") {
		assertEquals(
			(Vector(1), Vector(2), Vector(3)).mapN((a, b, c) => (a, b, c)),
			Vector((1, 2, 3))
		)
	}

	//-----------------------------------------------------------------------------

	test("Tuple.traverseN should work") {
		assertEquals(
			(1, "1", true).traverseN([A] => (a:A) => Vector(a)),
			Vector((1, "1", true))
		)
	}
}
