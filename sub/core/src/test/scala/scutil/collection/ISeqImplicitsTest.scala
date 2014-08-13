package scutil.collection

import org.specs2.mutable._

import scutil.lang.ISeq
import scutil.collection.implicits._

class ISeqImplicitsTest extends Specification {
	"equivalentSpans" should {
		"be empty for empty input" in {
			//(ISeq.empty[String] equivalentSpans criterium) must haveTheSameElementsAs(ISeq.empty)
			(ISeq.empty[String] equivalentSpans criterium) mustEqual ISeq.empty
		}
		"be simple for a 1-element input" in {
			(ISeq("hallo") equivalentSpans criterium) mustEqual ISeq(ISeq("hallo"))
		}
		"group together 2 equivalent elements" in {
			(ISeq("hallo", "hello") equivalentSpans criterium) mustEqual ISeq(ISeq("hallo", "hello"))
		}
		"group separate 2 non-equivalent elements" in {
			(ISeq("hallo", "ballo") equivalentSpans criterium) mustEqual ISeq(ISeq("hallo"), ISeq("ballo"))
		}
		"leave 1 non-equivalent element at the end" in {
			(ISeq("hallo", "hello", "ballo") equivalentSpans criterium) mustEqual ISeq(ISeq("hallo", "hello"), ISeq("ballo"))
		}
	}
	
	/*
	"splitAround" should {
		"be empty for empty input" in {
			(ISeq.empty[Int] splitAround 1) must haveTheSameElementsAs(ISeq.empty)
		}
		
		"be simple for a 1-element input" in {
			(ISeq(0) splitAround 1) must haveTheSameElementsAs(ISeq(ISeq(0)))
		}
		
		"split into two for a single separator" in {
			(ISeq(1) splitAround 1) must haveTheSameElementsAs(ISeq(ISeq(),ISeq()))
		}
		
		"split an empty ISeq before a leading separator" in {
			(ISeq(1,2) splitAround 1) must haveTheSameElementsAs(ISeq(ISeq(),ISeq(2)))
		}
		
		"split an empty ISeq after a trailing separator" in {
			(ISeq(0,1) splitAround 1) must haveTheSameElementsAs(ISeq(ISeq(0),ISeq()))
		}
		
		"split a simple ISeq correctly" in {
			(ISeq(0,1,2) splitAround 1) must haveTheSameElementsAs(ISeq(ISeq(0),ISeq(2)))
		}
		
		"create an empty ISeq between two adjacent separators" in {
			(ISeq(0,1,1,2) splitAround 1) must haveTheSameElementsAs(ISeq(ISeq(0),ISeq(),ISeq(2)))
		}
	}
	*/
	
	"splitWhere" should {
		val p:Int=>Boolean	= _ == 1
		
		"be empty for empty input" in {
			(ISeq.empty[Int] splitWhere p) mustEqual ISeq.empty
		}
		
		"be simple for a 1-element input" in {
			(ISeq(0) splitWhere p) mustEqual ISeq(Right(ISeq(0)))
		}
		
		"split into two for a single separator" in {
			(ISeq(1) splitWhere p) mustEqual ISeq(Right(ISeq()),Left(1), Right(ISeq()))
		}
		
		"split an empty ISeq before a leading separator" in {
			(ISeq(1,2) splitWhere p) mustEqual ISeq(Right(ISeq()),Left(1),Right(ISeq(2)))
		}
		
		"split an empty ISeq after a trailing separator" in {
			(ISeq(0,1) splitWhere p) mustEqual ISeq(Right(ISeq(0)),Left(1),Right(ISeq()))
		}
		
		"split  simple ISeq correctly" in {
			(ISeq(0,1,2) splitWhere p) mustEqual ISeq(Right(ISeq(0)),Left(1),Right(ISeq(2)))
		}
		
		"create an empty ISeq between two adjacent separators" in {
			(ISeq(0,1,1,2) splitWhere p) mustEqual ISeq(Right(ISeq(0)),Left(1),Right(ISeq()),Left(1),Right(ISeq(2)))
		}
	}
	
	private def criterium(a:String,b:String):Boolean	=
			(a charAt 0) == (b charAt 0)
}
