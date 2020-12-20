package scutil.lang

import minitest._

final case class Foo(a:Int, b:String)
final case class Bar[T](t:T)

object LensGenTest extends SimpleTestSuite {
	test("LensGen should work with simple case class getters") {
		val container	= Foo(4711, "hallo")
		val lens		= Lens.Gen[Foo].a
		assertEquals(
			lens get container,
			4711
		)
	}

	test("LensGen should work with simple case class setters") {
		val container	= Foo(4711, "hallo")
		val lens		= Lens.Gen[Foo].a
		assertEquals(
			lens set 1337 apply container,
			Foo(1337, "hallo")
		)
	}

	test("LensGen should work with type parameterized case class getters") {
		val container	= Bar("test")
		val lens		= Lens.Gen[Bar[String]].t
		assertEquals(
			lens get container,
			"test"
		)
	}

	test("LensGen should work with type parameterized case class setters") {
		val container	= Bar("test")
		val lens		= Lens.Gen[Bar[String]].t
		assertEquals(
			lens set "haha" apply container,
			Bar("haha")
		)
	}

	test("LensGen should allow splitting lenser and lens creation") {
		val container	= Foo(4711, "hallo")
		val lenses		= Lens.Gen[Foo]
		val lens		= lenses.a
		assertEquals(
			lens set 1337 apply container,
			Foo(1337, "hallo")
		)
	}

	test("LensGen should work as Lens.Gen") {
		val container	= Foo(4711, "hallo")
		val lens		= Lens.Gen[Foo].a
		assertEquals(
			lens set 1337 apply container,
			Foo(1337, "hallo")
		)
	}
}
