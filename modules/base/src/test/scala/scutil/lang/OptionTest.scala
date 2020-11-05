package scutil.lang

import org.specs2.mutable._

import scutil.base.implicits._

class OptionTest extends Specification {
	"Option extension and ApplicativeExtension" should {
		"not clash" in {
			// both OptionImplicits and ApplicativeSyntax provide a tuple method
			Option(1) tuple Option(2) mustEqual Option((1,2))
		}
	}
}
