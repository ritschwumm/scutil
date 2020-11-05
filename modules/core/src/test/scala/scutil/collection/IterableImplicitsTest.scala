package scutil.collection

import org.specs2.mutable._

import scutil.lang._
import scutil.collection.implicits._

class IterableImplicitsTest extends Specification {
	"iterable extension" should {
		"support Iterable.toSeq" in {
			val in:Iterable[Int]	= List(1,2,3)
			val out:Seq[Int]		= in.toSeq
			out mustEqual List(1,2,3)
		}

		"return the correct type and values for partitionEither" in {
			val in:List[Either[Int,Long]]	= List(Left(1), Right(2L))
			val out:(List[Int], List[Long])	= in.partitionEither
			out mustEqual ((List(1), List(2L)))
		}

		"return the correct type and values for partitionValidated" in {
			val in:List[Validated[Int,Long]]	= List[Validated[Int,Long]](Bad(1), Good(2L))
			val out:(List[Int], List[Long])	= in.partitionValidated
			out mustEqual ((List(1), List(2L)))
		}

		"return the correct type and values for validateValidated" in {
			val in:List[Validated[Int,Long]]	= List[Validated[Int,Long]](Bad(1), Good(2L))
			val out:Validated[List[Int],List[Long]]	= in.validateValidated
			out mustEqual Bad(List(1))
		}

		"return the correct type and values for zipBy" in {
			val in:List[Int]			= List(1,2,3)
			val out:List[(Int,Long)]	= in zipBy (_ * 2)
			out mustEqual List((1,2L),(2,4L), (3,6L))
		}
	}
}
