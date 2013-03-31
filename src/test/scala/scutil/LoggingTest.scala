package scutil

import org.specs2.mutable._

import scutil.log._

class LoggingTest extends Specification {
	"Logging" should {
		"just work" in {
			Foo.test()
			FooBar.test()
		}
	}
	
	object Foo extends Logging {
		def test() {
			INFO("logging works")
		}
	}
}

object FooBar extends Logging {
	def test() {
		INFO("logging works")
	}
}