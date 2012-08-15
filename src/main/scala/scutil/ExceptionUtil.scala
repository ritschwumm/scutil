package scutil

import scutil.log._

object ExceptionUtil {
	def logAllExceptions() {
		Thread setDefaultUncaughtExceptionHandler LoggingUncaughtExceptionHandler
	}
	
	def logThreadExceptions(thread:Thread) {
		thread setUncaughtExceptionHandler LoggingUncaughtExceptionHandler
	}
	
	/** replacement for logThreadExceptions(EDT) which does work in java 7, but not java 6 */
	def logAWTExceptions() {
		System setProperty ("sun.awt.exception.handler", classOf[Java6AWTExceptionHandler].getName)
	}
}

object LoggingUncaughtExceptionHandler extends Thread.UncaughtExceptionHandler with Logging {
	def uncaughtException(thread:Thread, throwable:Throwable) { 
		ERROR("uncaught exception", thread, throwable)
	}
}
	
/** must have a public no-args constructor and a handle method with the right signature */
final class Java6AWTExceptionHandler extends Logging {
	def handle(throwable:Throwable) {
		try {
			ERROR("uncaught awt exception", throwable)
		}
		catch {
			// an exception escaping here would cause an infinite loop
			case e	=> e.printStackTrace()
		}
	}
}
