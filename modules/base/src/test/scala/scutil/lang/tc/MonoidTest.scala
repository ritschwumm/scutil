package scutil.lang.tc

import org.specs2.mutable._

import scutil.base.implicits._

class MonoidTest extends Specification {
	"monoid" should {
		"support times 0 on string" in {
			"hallo".times(0) mustEqual ""
		}
		"support times n on string" in {
			"hallo".times(3) mustEqual "hallohallohallo"
		}

		// TODO tc times() exists as a pimped method on Seq, too
		"support times 0 on vector" in {
			Vector(1,2,3).times(0) mustEqual Vector()
		}
		"support times n on vector" in {
			Vector(1,2,3).times(3) mustEqual Vector(1,2,3,1,2,3,1,2,3)
		}
	}
}
