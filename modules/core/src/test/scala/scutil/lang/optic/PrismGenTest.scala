package scutil.lang

import scala.annotation.nowarn

import minitest.*

// NOTE the @nowarn are necessary dues to https://github.com/scala/scala3/issues/17753
object PrismGenTest extends SimpleTestSuite {
	object Foo {
		val P	= PrismGen[Foo]
	}
	enum Foo {
		case Obj
		case Nullary()
		case Unary(a:Int)
		case Binary(a:String, b:Boolean)
	}

	object Bar {
		def P[T]	= PrismGen[Bar[T]]
	}

	enum Bar[T] {
		case Nullary()
		case Unary(a:T)
		case Binary(a:T, b:T)
	}

	//-----------------------------------------------------------------------------

	test("missing variants are not compiled") {
		assertDoesNotCompile("Foo.P.DoesNotExists")
	}

	//-----------------------------------------------------------------------------

	test("get on generated prisms should work for object case") {
		assertEquals(
			( Foo.P.Obj.get(Foo.Obj) ) : @nowarn,
			Some(Foo.Obj)
		)
	}

	test("get on generated prisms should work for nullary case") {
		assertEquals(
			( Foo.P.Nullary.get(Foo.Nullary()) ) : @nowarn,
			Some(Foo.Nullary())
		)
	}

	test("get on generated prisms should work for unary case") {
		assertEquals(
			( Foo.P.Unary.get(Foo.Unary(1)) ) : @nowarn,
			Some(Foo.Unary(1))
		)
	}

	test("get on generated prisms should work for binary case") {
		assertEquals(
			( Foo.P.Binary.get(Foo.Binary("a", true)) ) : @nowarn,
			Some(Foo.Binary("a", true))
		)
	}

	//-----------------------------------------------------------------------------

	test("get on generated prisms should reject mismatch") {
		assertEquals(
			( Foo.P.Unary.get(Foo.Binary("a", true)) ) : @nowarn,
			None
		)
	}

	//-----------------------------------------------------------------------------

	test("set on generated prisms should work for object case") {
		assertEquals(
			( Foo.P.Obj.set(Foo.Obj) ) : @nowarn,
			Foo.Obj
		)
	}

	test("set on generated prisms should work for nullary case") {
		assertEquals(
			( Foo.P.Nullary.set(Foo.Nullary()) ) : @nowarn,
			Foo.Nullary()
		)
	}

	test("set on generated prisms should work for unary case") {
		assertEquals(
			( Foo.P.Unary.set(Foo.Unary(1)) ) : @nowarn,
			Foo.Unary(1)
		)
	}

	test("set on generated prisms should work for binary case") {
		assertEquals(
			( Foo.P.Binary.set(Foo.Binary("a", true)) ) : @nowarn,
			Foo.Binary("a", true)
		)
	}

	//-----------------------------------------------------------------------------

	test("get on generated prisms should work for generic enum") {
		assertEquals(
			( Bar.P.Unary.get(Bar.Unary(1)) ) : @nowarn,
			Some(Bar.Unary(1))
		)
	}

	test("set on generated prisms should work for generic enum") {
		assertEquals(
			( Bar.P.Unary.set(Bar.Unary(1)) ) : @nowarn,
			Bar.Unary(1)
		)
	}


}
