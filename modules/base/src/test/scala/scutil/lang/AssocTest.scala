package scutil.lang

import org.specs2.mutable._

import scutil.lang.assoc._

class AssocTest extends Specification {
	"arrow" should {
		"construct" in {
			val a	= "a" -> 1 -> 3L
			a mustEqual ((("a",1),3L))
		}
		"destruct" in {
			val a	= (("a",1),3L)
			val b	= a match { case x -> y -> z => ((x,y),z) }
			a mustEqual b
		}
		"type" in {
			val a	= (("a",1),3L)
			typed[ String -> Int -> Long ](a)
			a mustEqual a
		}

		//------------------------------------------------------------------------------

		"construct right-associative" in {
			val a	= "a" ->: 1 ->: 2L
			a mustEqual (("a", (1, 2L)))
		}

		"destruct right-associative" in {
			val a	= "a" ->: 1 ->: 2L
			val b	= a match { case x ->: y ->: z => (x,y,z) }
			b mustEqual (("a", 1, 2L))
		}

		"type right-associative" in {
			val a	= ("a",(1,2L))
			typed[ String ->: Int ->: Long ](a)
			a mustEqual a
		}
	}
}
