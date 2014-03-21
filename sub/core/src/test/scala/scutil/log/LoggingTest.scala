package scutil.log

import org.specs2.mutable._

class LoggingTest extends Specification {
	"Logging" should {
		"just work" in {
			Foo.test()
			FooBar.test()
			success
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
