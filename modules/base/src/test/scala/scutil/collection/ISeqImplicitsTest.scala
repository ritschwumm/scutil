package scutil.collection

import org.specs2.mutable._

import scutil.lang.ISeq
import scutil.collection.implicits._

class ISeqImplicitsTest extends Specification {
	"equivalentSpans" should {
		def criterium(a:String, b:String):Boolean	=
				(a charAt 0) == (b charAt 0)
		
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
	
	"adjacents" should {
		"work with 0 elements" in {
			ISeq.empty[Int].adjacents mustEqual ISeq.empty
		}
		"work with 1 element" in {
			ISeq(1).adjacents mustEqual ISeq((None,1,None))
		}
		"work with 2 elements" in {
			ISeq(1,2).adjacents mustEqual ISeq((None,1,Some(2)), (Some(1),2,None))
		}
		"work with 3 elements" in {
			ISeq(1,2,3).adjacents mustEqual ISeq((None,1,Some(2)), (Some(1),2,Some(3)), (Some(2),3,None))
		}
	}
	
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
	
	"moveAt" should {
		"fail without enough elements" in {
			ISeq() moveAt (0,0) mustEqual None
		}
		
		"move from start to end" in {
			ISeq(1,2,3) moveAt (0,3) mustEqual Some(ISeq(2,3,1))
		}
		
		"move from end to start" in {
			ISeq(1,2,3) moveAt (2,0) mustEqual Some(ISeq(3,1,2))
		}
		
		"not move to gap left" in {
			ISeq(1,2,3,4) moveAt (1,1) mustEqual None
		}
		
		"not move to gap right" in {
			ISeq(1,2,3,4) moveAt (1,2) mustEqual None
		}
		
		"move to gap further left" in {
			ISeq(1,2,3,4) moveAt (1,0) mustEqual Some(ISeq(2,1,3,4))
		}
		
		"move to gap further right" in {
			ISeq(1,2,3,4) moveAt (1,3) mustEqual Some(ISeq(1,3,2,4))
		}
	}
}
