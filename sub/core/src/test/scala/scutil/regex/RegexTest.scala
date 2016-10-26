package scutil.regex

import org.specs2.mutable._

import scutil.regex.implicits._

class RegexTest extends Specification {
	"regex macro" should {
		"just work" in {
			re".*".pattern.pattern mustEqual ".*".r.pattern.pattern
		}
	}
}
