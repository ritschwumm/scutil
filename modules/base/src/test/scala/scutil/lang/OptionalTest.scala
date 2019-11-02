package scutil.lang

import org.specs2.mutable._

object OptionalTest extends Specification {
	final case class A(x:String, b:B)
	final case class B(y:String, c:Int)

	"Optional" should {
		"trivial get should return unit" in {
			Optional.trivial[String] get "" mustEqual Some(())
		}

		"trivial set should not change anything" in {
			Optional.trivial[String] set (()) apply "foo" mustEqual "foo"
		}

		"identity get should return the original" in {
			Optional.identity[String] get "foo" mustEqual Some("foo")
		}

		"identity set should return the set value" in {
			Optional.identity[String] set "new" apply "old" mustEqual "new"
		}
	}
}
