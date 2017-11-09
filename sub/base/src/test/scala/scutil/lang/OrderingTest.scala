package scutil.lang

import org.specs2.mutable._

import scutil.base.implicits._

class OrderingTest extends Specification {
	"iseqOrdering" should {
		"order by base order (same)" in {
			ISeq(ISeq(1), ISeq(2), ISeq(1), ISeq(2)) sorted (Ordering sequence[Int] true) mustEqual
			ISeq(ISeq(1), ISeq(1), ISeq(2), ISeq(2))
		}
		
		"order missing first" in {
			ISeq(ISeq(1), ISeq(1,2), ISeq(1), ISeq(1,2)) sorted (Ordering sequence[Int] true) mustEqual
			ISeq(ISeq(1), ISeq(1), ISeq(1,2), ISeq(1,2))
		}
		
		"order missing last" in {
			ISeq(ISeq(1), ISeq(1,2), ISeq(1), ISeq(1,2)) sorted (Ordering sequence[Int] false) mustEqual
			ISeq(ISeq(1,2), ISeq(1,2), ISeq(1), ISeq(1))
		}
		
		"order left to right" in {
			ISeq(ISeq(1,1), ISeq(1,2), ISeq(2,1), ISeq(2,2), ISeq(1,1), ISeq(1,2), ISeq(2,1), ISeq(2,2)) sorted (Ordering sequence[Int] false) mustEqual
			ISeq(ISeq(1,1), ISeq(1,1), ISeq(1,2), ISeq(1,2), ISeq(2,1), ISeq(2,1), ISeq(2,2), ISeq(2,2))
		}
	}
}
