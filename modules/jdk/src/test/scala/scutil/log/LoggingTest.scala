package scutil.log

import org.specs2.mutable._

class LoggingTest extends Specification with TestLogging {
	"Logging" should {
		"just work" in {
			logHandler.reset()

			INFO("logging works")

			logHandler.strings(0) must be matching """INFO\t\[.*\]\tLoggingTest.scala:\d+\tlogging works"""
		}
	}
}

//------------------------------------------------------------------------------

trait TestLogging extends Logging {
	override val logHandler	= new TestLogHandler
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
