package scutil.lang.tc

import org.specs2.mutable._

import scutil.base.implicits._
import scutil.lang._

class ApplicativeTupleNSyntaxGeneratedTest extends Specification {
	"applicative tuple syntax" should {
		"zip 2 elements" in {
			(Vector(1), Vector(2)).zipN mustEqual Vector((1,2))
		}
		"zip 3 elements" in {
			(Vector(1), Vector(2), Vector(3)).zipN mustEqual Vector((1,2,3))
		}
		"map 2 elements" in {
			(Vector(1), Vector(2)) mapN ((a,b) => (a,b)) mustEqual Vector((1,2))
		}
		"map 3 elements" in {
			(Vector(1), Vector(2), Vector(3)) mapN ((a,b,c) => (a,b,c)) mustEqual Vector((1,2,3))
		}

		"zip vector" in {
			(Vector(1), Vector(2), Vector(3)).zipN mustEqual Vector((1,2,3))
		}
		"zip option" in {
			(Option.some(1), Option.some(2), Option.some(3)).zipN mustEqual Some((1,2,3))
		}
		"zip either" in {
			(Either.right[String,Int](1), Either.right[String,Int](2), Either.right[String,Int](3)).zipN mustEqual Right((1,2,3))
		}
		"zip validated" in {
			(Validated.good[String,Int](1), Validated.good[String,Int](2), Validated.good[String,Int](3)).zipN mustEqual Good((1,2,3))
		}
	}
}
