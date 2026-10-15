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

	//-----------------------------------------------------------------------------

	test("LogValue takes single values") {
		logHandler.reset()

		INFO("string", 1, false)

		assert(
			logHandler.strings(0).matches("""^INFO\t\[.*\]\tLoggingTest.scala:\d+\tstring\t1\tfalse$""")
		)
	}

	test("LogValue takes multi-values") {
		logHandler.reset()

		INFO("string", Vector(1, 1))

		assert(
			logHandler.strings(0).matches("""^INFO\t\[.*\]\tLoggingTest.scala:\d+\tstring\t1\t1$""")
		)
	}

	test("LogValue.of works for values") {
		LogValue.of("test")
		assert(true)
	}

	test("LogValue.of works for arrays") {
		LogValue.of(Vector("foo", "bar"))
		assert(true)
	}

	test("LogValue.ofN works") {
		LogValue.ofN("foo", 1)
		assert(true)
	}
}
