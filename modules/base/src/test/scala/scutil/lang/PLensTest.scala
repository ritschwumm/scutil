package scutil.lang

import org.specs2.mutable._

object PLensTest extends Specification {
	final case class A(x:String, b:B)
	final case class B(y:String, c:Int)

	"PLens" should {
		"trivial get should return unit" in {
			PLens.trivial[String] get "" mustEqual Some(())
		}

		"trivial set should not change anything" in {
			PLens.trivial[String] set (()) apply "foo" mustEqual Some("foo")
		}

		"identity get should return the original" in {
			PLens.identity[String] get "foo" mustEqual Some("foo")
		}

		"identity set should return the set value" in {
			PLens.identity[String] set "new" apply "old" mustEqual Some("new")
		}
	}
}
