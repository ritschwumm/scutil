package scutil.lang

import org.specs2.mutable._

import scutil.core.implicits._

class StringTest extends Specification {
	"String extension" should {
		"parse 'false' to Boolean" in {
			"false".parseBoolean mustEqual Right(false)
		}
		"parse 'true' to Boolean" in {
			"true".parseBoolean mustEqual Right(true)
		}
		"parse 'FALSE' to Boolean" in {
			"FALSE".parseBoolean mustEqual Right(false)
		}
		"parse 'TRUE' to Boolean" in {
			"TRUE".parseBoolean mustEqual Right(true)
		}

		"parse 'False' to Boolean" in {
			"False".parseBoolean mustEqual Right(false)
		}
		"parse 'True' to Boolean" in {
			"True".parseBoolean mustEqual Right(true)
		}
		"parse 'fAlse' to Boolean" in {
			"fAlse".parseBoolean mustEqual Right(false)
		}
		"parse 'tRrue' to Boolean" in {
			"tRue".parseBoolean mustEqual Right(true)
		}
		"parse 'faLse' to Boolean" in {
			"faLse".parseBoolean mustEqual Right(false)
		}
		"parse 'trUe' to Boolean" in {
			"trUe".parseBoolean mustEqual Right(true)
		}
		"parse 'falSe' to Boolean" in {
			"falSe".parseBoolean mustEqual Right(false)
		}
		"parse 'truE' to Boolean" in {
			"truE".parseBoolean mustEqual Right(true)
		}
		"parse 'falsE' to Boolean" in {
			"falsE".parseBoolean mustEqual Right(false)
		}
	}
}
