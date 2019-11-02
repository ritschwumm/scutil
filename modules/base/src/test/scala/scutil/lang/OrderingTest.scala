package scutil.lang

import org.specs2.mutable._

import scutil.base.implicits._

class OrderingTest extends Specification {
	"seqOrdering" should {
		"order by base order (same)" in {
			Seq(Seq(1), Seq(2), Seq(1), Seq(2)) sorted (Ordering sequence[Int] true) mustEqual
			Seq(Seq(1), Seq(1), Seq(2), Seq(2))
		}

		"order missing first" in {
			Seq(Seq(1), Seq(1,2), Seq(1), Seq(1,2)) sorted (Ordering sequence[Int] true) mustEqual
			Seq(Seq(1), Seq(1), Seq(1,2), Seq(1,2))
		}

		"order missing last" in {
			Seq(Seq(1), Seq(1,2), Seq(1), Seq(1,2)) sorted (Ordering sequence[Int] false) mustEqual
			Seq(Seq(1,2), Seq(1,2), Seq(1), Seq(1))
		}

		"order left to right" in {
			Seq(Seq(1,1), Seq(1,2), Seq(2,1), Seq(2,2), Seq(1,1), Seq(1,2), Seq(2,1), Seq(2,2)) sorted (Ordering sequence[Int] false) mustEqual
			Seq(Seq(1,1), Seq(1,1), Seq(1,2), Seq(1,2), Seq(2,1), Seq(2,1), Seq(2,2), Seq(2,2))
		}
	}
}
