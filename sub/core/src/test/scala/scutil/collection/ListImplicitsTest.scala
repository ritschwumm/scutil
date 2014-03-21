package scutil.collection

import org.specs2.mutable._

import scutil.collection.implicits._

class ListImplicitsTest extends Specification {
	"unprefix" should {
		"leave the prefix empty for one empty list" in {
			Nil unprefix List(1) mustEqual (Nil, Nil, List(1))
		}
		"leave the prefix empty for another empty list" in {
			List(1) unprefix Nil mustEqual (Nil, List(1), Nil)
		}
		"find a common prefix" in {
			List(1,2,10,11) unprefix List(1,2,20,21) mustEqual (List(1,2), List(10,11), List(20,21))
		}
	}
}
