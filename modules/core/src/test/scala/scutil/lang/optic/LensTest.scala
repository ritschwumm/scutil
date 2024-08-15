package scutil.lang

import minitest.*

object LensTest extends SimpleTestSuite {
	final case class A(x:String, b:B)
	final case class B(y:String, c:Int)

	test("Lens Should just work") {
		val a	= A("a", B("b", 1))

		val Ab	= Lens((a:A) => a.b, (b:B)		=> (a:A)	=> a.copy(b=b))
		val Bc	= Lens((b:B) => b.c, (c:Int)	=> (b:B)	=> b.copy(c=c))

		val ABc	= Ab.andThen(Bc)

		val result	= ABc.set(2)(a)

		assertEquals(
			result,
			A("a", B("b",2))
		)
	}

	test("Lens Should trivial get should return unit") {
		assertEquals(
			Lens.trivial[String].get(""),
			()
		)
	}

	test("Lens Should trivial set should not change anything") {
		assertEquals(
			Lens.trivial[String].set(())("foo"),
			"foo"
		)
	}

	test("Lens Should identity get should return the original") {
		assertEquals(
			Lens.identity[String].get("foo"),
			"foo"
		)
	}

	test("Lens Should identity set should return the set value") {
		assertEquals(
			Lens.identity[String].set("new")("old"),
			"new"
		)
	}

	//------------------------------------------------------------------------------

	test("Lenses.map should add a non-existing item") {
		assertEquals(
			Lenses.map[String,Int]("three").set(Some(99))(Map("one" -> 1, "two" -> 2)),
			Map("one" -> 1, "two" -> 2, "three" -> 99)
		)
	}

	test("Lenses.map should change an existing item") {
		assertEquals(
			Lenses.map[String,Int]("two").set(Some(99))(Map("one" -> 1, "two" -> 2)),
			Map("one" -> 1, "two" -> 99)
		)
	}

	test("Lenses.map should remove an existing item") {
		assertEquals(
			Lenses.map[String,Int]("two").set(None)(Map("one" -> 1, "two" -> 2)),
			Map("one" -> 1)
		)
	}
}
