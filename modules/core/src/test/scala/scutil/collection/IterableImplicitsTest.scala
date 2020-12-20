package scutil.collection

import minitest._

import scutil.lang._
import scutil.collection.implicits._

object IterableImplicitsTest extends SimpleTestSuite {
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
		val in:List[Validated[Int,Long]]	= List[Validated[Int,Long]](Bad(1), Good(2L))
		val out:(List[Int], List[Long])	= in.partitionValidated
		assertEquals(
			out,
			((List(1), List(2L)))
		)
	}

	test("iterable extension should return the correct type and values for validateValidated") {
		val in:List[Validated[Int,Long]]	= List[Validated[Int,Long]](Bad(1), Good(2L))
		val out:Validated[List[Int],List[Long]]	= in.validateValidated
		assertEquals(
			out,
			Bad(List(1))
		)
	}

	test("iterable extension should return the correct type and values for zipBy") {
		val in:List[Int]			= List(1,2,3)
		val out:List[(Int,Long)]	= in zipBy (_ * 2)
		assertEquals(
			out,
			List((1,2L),(2,4L), (3,6L))
		)
	}
}
