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

	test("iterable extension should return the correct type and values for fproductIterable") {
		val in:List[Int]			= List(1,2,3)
		val out:List[(Int,Long)]	= in.fproductIterable(_ * 2L)
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

	test("zipTail should just work") {
		assertEquals(
			Vector(1,2,3).zipTail,
			Vector((1,2),(2,3))
		)
	}

	test("partitionEither should just work") {
		assertEquals(
			Vector(Left(1),Right("1"),Left(2),Right("3")).partitionEither,
			(Vector(1,2), Vector("1","3"))
		)
	}

	test("partitionValidated should just work") {
		assertEquals(
			Vector(Validated.invalid(1),Validated.valid("1"),Validated.invalid(2),Validated.valid("3")).partitionValidated,
			(Vector(1,2), Vector("1","3"))
		)
	}

	test("validateEither should reject errors") {
		assertEquals(
			Vector(Left(1),Right("1"),Left(2),Right("3")).validateEither,
			Left(Vector(1,2))
		)
	}

	test("validateEither accept all rights") {
		assertEquals(
			Vector(Right("1"),Right("3")).validateEither,
			Right(Vector("1","3"))
		)
	}

	test("validateValidated should reject errors") {
		assertEquals(
			Vector(Validated.invalid(1),Validated.valid("1"),Validated.invalid(2),Validated.valid("3")).validateValidated,
			Validated.Invalid(Vector(1,2))
		)
	}

	test("validateValidated accept all valids") {
		assertEquals(
			Vector(Validated.valid("1"),Validated.valid("3")).validateValidated,
			Validated.Valid(Vector("1","3"))
		)
	}

	test("singleOption works for Array") {
		assertEquals(
			Array(1).singleOption,
			Some(1)
		)
	}


	//------------------------------------------------------------------------------

	test("groupMapPaired works for Vector") {
		val input		= Vector("a" -> 1, "b" -> 2, "a" -> 3)
		val grouped		= input.groupMapPaired(it => (it._1, it._2))
		typed[Map[String,Vector[Int]]](grouped)
		val expected	= Map("a" -> Vector(1,3), "b" -> Vector(2))
		assertEquals(grouped, expected)
	}

	test("groupMapPaired works for Set") {
		val input		= Set("a" -> 1, "b" -> 2, "a" -> 3)
		val grouped		= input.groupMapPaired(it => (it._1, it._2))
		typed[Map[String,Set[Int]]](grouped)
		val expected	= Map("a" -> Set(1,3), "b" -> Set(2))
		assertEquals(grouped, expected)
	}
}
