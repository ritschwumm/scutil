package scutil

import scutil.lang._

object ExceptionUtil {
	def logAllExceptions(onError:(Thread,Throwable)=>Unit) {
		Thread setDefaultUncaughtExceptionHandler new LoggingUncaughtExceptionHandler(onError)
	}
	
	def logThreadExceptions(thread:Thread, onError:(Thread,Throwable)=>Unit) {
		thread setUncaughtExceptionHandler new LoggingUncaughtExceptionHandler(onError)
	}
	
	/** replacement for logThreadExceptions(EDT) which does work in java 7, but not java 6 */
	def logAWTExceptions(onError:(Thread,Throwable)=>Unit) {
		java6AwtHandler	= onError
		System setProperty ("sun.awt.exception.handler", classOf[Java6AWTExceptionHandler].getName)
	}
	
	private[scutil]
	var java6AwtHandler:(Thread,Throwable)=>Unit	= null
}

private final class LoggingUncaughtExceptionHandler(onError:(Thread,Throwable)=>Unit) extends Thread.UncaughtExceptionHandler {
	def uncaughtException(thread:Thread, throwable:Throwable) { 
		onError(thread, throwable)
	}
}
	
/** must have a public no-args constructor and a handle method with the right signature */
private final class Java6AWTExceptionHandler {
	def handle(throwable:Throwable) {
		try {
			if (ExceptionUtil.java6AwtHandler != null) {
				ExceptionUtil.java6AwtHandler apply (Thread.currentThread, throwable)
			}
		}
		catch {	case e:Throwable	=> 
			// NOTE a Throwable escaping here would cause an infinite loop
			e.printStackTrace()
		}
	}
}
