package scutil.lang

import scala.annotation.nowarn

import minitest.*

final case class Foo(a:Int, b:String)
final case class Bar[T](t:T)

object LensGenTest extends SimpleTestSuite {
	val  L1 = Lens.Gen[Foo]

	test("LensGen should work as an object") {
		val container	= Foo(4711, "hallo")
		val lens		= L1.a
		assertEquals(
			lens.get(container),
			4711
		)
	}

	//------------------------------------------------------------------------------

	type X = Foo
	val  L2 = Lens.Gen[X]

	test("LensGen should work as an object constructed through a type parameter") {
		val container	= Foo(4711, "hallo")
		val lens		= L2.a
		assertEquals(
			lens.get(container),
			4711
		)
	}

	//------------------------------------------------------------------------------

	test("LensGen should work with simple case class getters") {
		val container	= Foo(4711, "hallo")
		val lens		= (Lens.Gen[Foo].a) : @nowarn
		assertEquals(
			lens.get(container),
			4711
		)
	}

	test("LensGen should work with simple case class setters") {
		val container	= Foo(4711, "hallo")
		val lens		= (Lens.Gen[Foo].a) : @nowarn
		assertEquals(
			lens.set(1337)(container),
			Foo(1337, "hallo")
		)
	}

	test("LensGen should work with type parameterized case class getters") {
		val container	= Bar("test")
		val lens		= (Lens.Gen[Bar[String]].t) : @nowarn
		assertEquals(
			lens.get(container),
			"test"
		)
	}

	test("LensGen should work with type parameterized case class setters") {
		val container	= Bar("test")
		val lens		= (Lens.Gen[Bar[String]].t) : @nowarn
		assertEquals(
			lens.set("haha")(container),
			Bar("haha")
		)
	}

	test("LensGen should allow splitting lenser and lens creation") {
		val container	= Foo(4711, "hallo")
		val lenses		= Lens.Gen[Foo]
		val lens		= lenses.a
		assertEquals(
			lens.set(1337)(container),
			Foo(1337, "hallo")
		)
	}

	test("LensGen should work as Lens.Gen") {
		val container	= Foo(4711, "hallo")
		val lens		= (Lens.Gen[Foo].a) : @nowarn
		assertEquals(
			lens.set(1337)(container),
			Foo(1337, "hallo")
		)
	}
}
