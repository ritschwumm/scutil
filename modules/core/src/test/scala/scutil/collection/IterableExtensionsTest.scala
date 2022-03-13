package scutil.collection

import minitest.*

import scutil.lang.*
import scutil.collection.implicits.*

object IterableExtensionsTest extends SimpleTestSuite {
	test("iterable extension should support Iterable.toSeq") {
		val in:Iterable[Int]	= List(1,2,3)
		val out:Seq[Int]		= in.toSeq
		assertEquals(
			out,
			List(1,2,3)
		)
	}

	test("iterable extension should return the correct type and values for partitionEither") {
		val in:List[Either[Int,Long]]	= List(Left(1), Right(2L))
		val out:(List[Int], List[Long])	= in.partitionEither
		assertEquals(
			out,
			((List(1), List(2L)))
		)
	}

	test("iterable extension should return the correct type and values for partitionValidated") {
		val in:List[Validated[Int,Long]]	= List[Validated[Int,Long]](Validated.invalid(1), Validated.valid(2L))
		val out:(List[Int], List[Long])	= in.partitionValidated
		assertEquals(
			out,
			((List(1), List(2L)))
		)
	}

	test("iterable extension should return the correct type and values for validateValidated") {
		val in:List[Validated[Int,Long]]	= List[Validated[Int,Long]](Validated.invalid(1), Validated.valid(2L))
		val out:Validated[List[Int],List[Long]]	= in.validateValidated
		assertEquals(
			out,
			Validated.invalid(List(1))
		)
	}

	test("iterable extension should return the correct type and values for fproduct") {
		val in:List[Int]			= List(1,2,3)
		val out:List[(Int,Long)]	= in fproduct (_ * 2)
		assertEquals(
			out,
			List((1,2L),(2,4L), (3,6L))
		)
	}

	test("intersperse should work with 0 elements") {
		assertEquals(
			List().intersperse(0),
			List()
		)
	}

	test("intersperse should work with 1 element") {
		assertEquals(
			List(1).intersperse(0),
			List(1)
		)
	}

	test("intersperse should work with 2 elements") {
		assertEquals(
			List(1,2).intersperse(0),
			List(1,0,2)
		)
	}

	test("intersperse should work with 3 elements") {
		assertEquals(
			List(1,2,3).intersperse(0),
			List(1,0,2,0,3)
		)
	}

}
