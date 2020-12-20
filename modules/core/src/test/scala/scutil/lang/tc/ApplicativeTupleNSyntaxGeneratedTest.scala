package scutil.lang.tc

import minitest._

import scutil.core.implicits._
import scutil.lang._

object ApplicativeTupleNSyntaxGeneratedTest extends SimpleTestSuite {
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

	test("applicative tuple syntax should map 2 elements") {
		assertEquals(
			(Vector(1), Vector(2)) mapN ((a,b) => (a,b)),
			Vector((1,2))
		)
	}

	test("applicative tuple syntax should map 3 elements") {
		assertEquals(
			(Vector(1), Vector(2), Vector(3)) mapN ((a,b,c) => (a,b,c)),
			Vector((1,2,3))
		)
	}

	//------------------------------------------------------------------------------

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
			(Validated.good[String,Int](1), Validated.good[String,Int](2), Validated.good[String,Int](3)).zipN,
			Good((1,2,3))
		)
	}
}
