package scutil.lang

import minitest.*

import scutil.lang.implicits.*

object OptionExtensionsTest extends SimpleTestSuite {
	test("flattenMany should work with Some Vector") {
		val input:Option[Vector[Int]]	= Some(Vector(1,2,3))
		assertEquals(
			input.flattenMany,
			Vector(1,2,3)
		)
	}

	test("flattenMany should work with None Vector") {
		val input:Option[Vector[Int]]	= None
		assertEquals(
			input.flattenMany,
			Vector()
		)
	}

	test("flattenMany should work with Some List") {
		val input:Option[List[Int]]	= Some(List(1,2,3))
		assertEquals(
			input.flattenMany,
			List(1,2,3)
		)
	}

	test("flattenMany should work with None List") {
		val input:Option[List[Int]]	= None
		assertEquals(
			input.flattenMany,
			List()
		)
	}

	test("flattenMany should work with Some Seq") {
		val input:Option[Seq[Int]]	= Some(Seq(1,2,3))
		assertEquals(
			input.flattenMany,
			Seq(1,2,3)
		)
	}

	test("flattenMany should work with None Seq") {
		val input:Option[Seq[Int]]	= None
		assertEquals(
			input.flattenMany,
			Seq()
		)
	}

	//------------------------------------------------------------------------------

	test("flatMapMany should work with Some Vector") {
		val input:Option[Int]	= Some(1)
		assertEquals(
			input.flatMapMany(_ => Vector(1,2,3)),
			Vector(1,2,3)
		)
	}

	test("flatMapMany should work with None Vector") {
		val input:Option[Vector[Int]]	= None
		assertEquals(
			input.flatMapMany(_ => Vector(1,2,3)),
			Vector()
		)
	}

	test("flatMapMany should work with Some List") {
		val input:Option[Int]	= Some(1)
		assertEquals(
			input.flatMapMany(_ => List(1,2,3)),
			List(1,2,3)
		)
	}

	test("flatMapMany should work with None List") {
		val input:Option[List[Int]]	= None
		assertEquals(
			input.flatMapMany(_ => List(1,2,3)),
			List()
		)
	}

	test("flatMapMany should work with Some Seq") {
		val input:Option[Int]	= Some(1)
		assertEquals(
			input.flatMapMany(_ => Seq(1,2,3)),
			Seq(1,2,3)
		)
	}

	test("flatMapMany should work with None Seq") {
		val input:Option[Seq[Int]]	= None
		assertEquals(
			input.flatMapMany(_ => Seq(1,2,3)),
			Seq()
		)
	}
}
