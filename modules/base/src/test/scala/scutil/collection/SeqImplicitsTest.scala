package scutil.collection

import org.specs2.mutable._

import scutil.lang._
import scutil.collection.implicits._

class SeqImplicitsTest extends Specification {
	"equivalentSpans" should {
		def criterium(a:String, b:String):Boolean	=
				(a charAt 0) == (b charAt 0)

		"be empty for empty input" in {
			//(Seq.empty[String] equivalentSpans criterium) must haveTheSameElementsAs(Seq.empty)
			(Seq.empty[String] equivalentSpans criterium) mustEqual Seq.empty
		}
		"be simple for a 1-element input" in {
			(Seq("hallo") equivalentSpans criterium) mustEqual Seq(Seq("hallo"))
		}
		"group together 2 equivalent elements" in {
			(Seq("hallo", "hello") equivalentSpans criterium) mustEqual Seq(Seq("hallo", "hello"))
		}
		"group separate 2 non-equivalent elements" in {
			(Seq("hallo", "ballo") equivalentSpans criterium) mustEqual Seq(Seq("hallo"), Seq("ballo"))
		}
		"leave 1 non-equivalent element at the end" in {
			(Seq("hallo", "hello", "ballo") equivalentSpans criterium) mustEqual Seq(Seq("hallo", "hello"), Seq("ballo"))
		}
	}

	/*
	"splitAround" should {
		"be empty for empty input" in {
			(Seq.empty[Int] splitAround 1) must haveTheSameElementsAs(Seq.empty)
		}

		"be simple for a 1-element input" in {
			(Seq(0) splitAround 1) must haveTheSameElementsAs(Seq(Seq(0)))
		}

		"split into two for a single separator" in {
			(Seq(1) splitAround 1) must haveTheSameElementsAs(Seq(Seq(),Seq()))
		}

		"split an empty Seq before a leading separator" in {
			(Seq(1,2) splitAround 1) must haveTheSameElementsAs(Seq(Seq(),Seq(2)))
		}

		"split an empty Seq after a trailing separator" in {
			(Seq(0,1) splitAround 1) must haveTheSameElementsAs(Seq(Seq(0),Seq()))
		}

		"split a simple Seq correctly" in {
			(Seq(0,1,2) splitAround 1) must haveTheSameElementsAs(Seq(Seq(0),Seq(2)))
		}

		"create an empty Seq between two adjacent separators" in {
			(Seq(0,1,1,2) splitAround 1) must haveTheSameElementsAs(Seq(Seq(0),Seq(),Seq(2)))
		}
	}
	*/

	"adjacents" should {
		"work with 0 elements" in {
			Seq.empty[Int].adjacents mustEqual Seq.empty
		}
		"work with 1 element" in {
			Seq(1).adjacents mustEqual Seq((None,1,None))
		}
		"work with 2 elements" in {
			Seq(1,2).adjacents mustEqual Seq((None,1,Some(2)), (Some(1),2,None))
		}
		"work with 3 elements" in {
			Seq(1,2,3).adjacents mustEqual Seq((None,1,Some(2)), (Some(1),2,Some(3)), (Some(2),3,None))
		}

		"return the right type" in {
			val a	= Vector(1,2,3).adjacents
			typed[ Vector[(Option[Int],Int,Option[Int])] ](a)
			a mustEqual Seq((None,1,Some(2)), (Some(1),2,Some(3)), (Some(2),3,None))
		}
	}

	"splitWhere" should {
		val p:Int=>Boolean	= _ == 1

		"be empty for empty input" in {
			(Seq.empty[Int] splitWhere p) mustEqual Seq.empty
		}

		"be simple for a 1-element input" in {
			(Seq(0) splitWhere p) mustEqual Seq(Right(Seq(0)))
		}

		"split into two for a single separator" in {
			(Seq(1) splitWhere p) mustEqual Seq(Right(Seq()),Left(1), Right(Seq()))
		}

		"split an empty Seq before a leading separator" in {
			(Seq(1,2) splitWhere p) mustEqual Seq(Right(Seq()),Left(1),Right(Seq(2)))
		}

		"split an empty Seq after a trailing separator" in {
			(Seq(0,1) splitWhere p) mustEqual Seq(Right(Seq(0)),Left(1),Right(Seq()))
		}

		"split  simple Seq correctly" in {
			(Seq(0,1,2) splitWhere p) mustEqual Seq(Right(Seq(0)),Left(1),Right(Seq(2)))
		}

		"create an empty Seq between two adjacent separators" in {
			(Seq(0,1,1,2) splitWhere p) mustEqual Seq(Right(Seq(0)),Left(1),Right(Seq()),Left(1),Right(Seq(2)))
		}
	}

	"moveAt" should {
		"fail without enough elements" in {
			Seq().moveAt(0,0) mustEqual None
		}

		"move from start to end" in {
			Seq(1,2,3).moveAt(0,3) mustEqual Some(Seq(2,3,1))
		}

		"move from end to start" in {
			Seq(1,2,3).moveAt(2,0) mustEqual Some(Seq(3,1,2))
		}

		"not move to gap left" in {
			Seq(1,2,3,4).moveAt(1,1) mustEqual None
		}

		"not move to gap right" in {
			Seq(1,2,3,4).moveAt(1,2) mustEqual None
		}

		"move to gap further left" in {
			Seq(1,2,3,4).moveAt(1,0) mustEqual Some(Seq(2,1,3,4))
		}

		"move to gap further right" in {
			Seq(1,2,3,4).moveAt(1,3) mustEqual Some(Seq(1,3,2,4))
		}
	}

	"zipTail" should {
		"just work" in {
			Vector(1,2,3).zipTail mustEqual Vector((1,2),(2,3))
		}
	}
}
