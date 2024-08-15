package scutil.log

import minitest.*

trait TestLogging extends Logging {
	override val logHandler:TestLogHandler	= new TestLogHandler
}

class TestLogHandler extends DefaultLogHandler {
	var strings	= Vector.empty[String]

	def reset():Unit = {
		strings	= Vector.empty
	}

	override def print(s:String):Unit = {
		strings	:+= s
	}
}

//------------------------------------------------------------------------------

object LoggingTest extends SimpleTestSuite with TestLogging {
	test("Logging should just work") {
		logHandler.reset()

		INFO("logging works")

		assert(
			logHandler.strings(0).matches("""^INFO\t\[.*\]\tLoggingTest.scala:\d+\tlogging works$""")
		)
	}
}
