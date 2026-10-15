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

	test("tuple transform syntax should work for 1 element") {
		assertEquals(
			Tuple1("hello").ap(Tuple1(
				(_:String)	=> 1)
			),
			Tuple1(1)
		)
	}

	test("tuple transform syntax should work for 2 elements") {
		assertEquals(
			Tuple2("hello", 1).ap(Tuple2(
				(_:String)	=> 1,
				(_:Int)		=> false,
			)),
			Tuple2(1, false)
		)
	}

	test("tuple transform syntax should work for 3 elements") {
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

	test("applicative tuple syntax should zip 1 element") {
		assertEquals(
			Tuple1(Vector(1)).zipN,
			Vector(Tuple1(1))
		)
	}

	test("applicative tuple syntax should zip 2 elements") {
		assertEquals(
			(Vector(1), Vector(2)).zipN,
			Vector((1,2))
		)
	}

	test("applicative tuple syntax should zip 3 elements") {
		assertEquals(
			(Vector(1), Vector(2), Vector(3)).zipN,
			Vector((1,2,3))
		)
	}

	//-----------------------------------------------------------------------------

	test("applicative tuple syntax should zip vector") {
		assertEquals(
			(Vector(1), Vector(2), Vector(3)).zipN,
			Vector((1,2,3))
		)
	}

	test("applicative tuple syntax should zip option") {
		assertEquals(
			(Option.some(1), Option.some(2), Option.some(3)).zipN,
			Some((1,2,3))
		)
	}

	test("applicative tuple syntax should zip either") {
		assertEquals(
			(Either.right[String,Int](1), Either.right[String,Int](2), Either.right[String,Int](3)).zipN,
			Right((1,2,3))
		)
	}

	test("applicative tuple syntax should zip validated") {
		assertEquals(
			(Validated.valid[String,Int](1), Validated.valid[String,Int](2), Validated.valid[String,Int](3)).zipN,
			Validated.valid((1,2,3))
		)
	}

	//-----------------------------------------------------------------------------

	test("applicative tuple syntax should map 1 element") {
		assertEquals(
			Tuple1(Vector(1)).mapN((a) => Tuple1(a)),
			Vector(Tuple1(1))
		)
	}

	test("applicative tuple syntax should map 2 elements") {
		assertEquals(
			(Vector(1), Vector(2)).mapN((a,b) => (a,b)),
			Vector((1,2))
		)
	}

	test("applicative tuple syntax should map 3 elements") {
		assertEquals(
			(Vector(1), Vector(2), Vector(3)).mapN((a,b,c) => (a,b,c)),
			Vector((1,2,3))
		)
	}
}
