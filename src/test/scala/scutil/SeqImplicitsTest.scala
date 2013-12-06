package scutil

import org.specs2.mutable._

import implicits._

class SeqImplicitsTest extends Specification {
	"equivalentSpans" should {
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
		
		"split an empty seq before a leading separator" in {
			(Seq(1,2) splitAround 1) must haveTheSameElementsAs(Seq(Seq(),Seq(2)))
		}
		
		"split an empty seq after a trailing separator" in {
			(Seq(0,1) splitAround 1) must haveTheSameElementsAs(Seq(Seq(0),Seq()))
		}
		
		"split simple seqs correctly" in {
			(Seq(0,1,2) splitAround 1) must haveTheSameElementsAs(Seq(Seq(0),Seq(2)))
		}
		
		"create an empty seq between two adjacent separators" in {
			(Seq(0,1,1,2) splitAround 1) must haveTheSameElementsAs(Seq(Seq(0),Seq(),Seq(2)))
		}
	}
	*/
	
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
		
		"split an empty seq before a leading separator" in {
			(Seq(1,2) splitWhere p) mustEqual Seq(Right(Seq()),Left(1),Right(Seq(2)))
		}
		
		"split an empty seq after a trailing separator" in {
			(Seq(0,1) splitWhere p) mustEqual Seq(Right(Seq(0)),Left(1),Right(Seq()))
		}
		
		"split simple seqs correctly" in {
			(Seq(0,1,2) splitWhere p) mustEqual Seq(Right(Seq(0)),Left(1),Right(Seq(2)))
		}
		
		"create an empty seq between two adjacent separators" in {
			(Seq(0,1,1,2) splitWhere p) mustEqual Seq(Right(Seq(0)),Left(1),Right(Seq()),Left(1),Right(Seq(2)))
		}
	}
	
	private def criterium(a:String,b:String):Boolean	=
			(a charAt 0) == (b charAt 0)
}
