package scutil.lang

import minitest.*

object OptionalTest extends SimpleTestSuite {
	final case class A(x:String, b:B)
	final case class B(y:String, c:Int)

	//------------------------------------------------------------------------------

	test("Optional should trivial get should return unit") {
		assertEquals(
			Optional.trivial[String].get(""),
			Some(())
		)
	}

	test("Optional should trivial set should not change anything") {
		assertEquals(
			Optional.trivial[String].set(())("foo"),
			"foo"
		)
	}

	test("Optional should identity get should return the original") {
		assertEquals(
			Optional.identity[String].get("foo"),
			Some("foo")
		)
	}

	test("Optional should identity set should return the set value") {
		assertEquals(
			Optional.identity[String].set("new")("old"),
			"new"
		)
	}

	//------------------------------------------------------------------------------

	test("Optionals.indexed should change an existing item") {
		assertEquals(
			Optionals.indexed[Vector, Int](2).set(99)(Vector(1, 2, 3, 4)),
			Vector(1, 2, 99, 4)
		)
	}

	test("Optionals.indexed should not explode with a non-existing item") {
		assertEquals(
			Optionals.indexed[Vector, Int](7).set(99)(Vector(1, 2, 3, 4)),
			Vector(1, 2, 3, 4)
		)
	}

	//------------------------------------------------------------------------------

	test("Optionals.map should change an existing item") {
		assertEquals(
			Optionals.map[String, Int]("two").set(99)(Map("one" -> 1, "two" -> 2)),
			Map("one" -> 1, "two" -> 99)
		)
	}

	test("Optionals.map should not insert a missing item") {
		assertEquals(
			Optionals.map[String, Int]("three").set(99)(Map("one" -> 1, "two" -> 2)),
			Map("one" -> 1, "two" -> 2)
		)
	}
}
