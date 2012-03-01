package scutil

import scutil.time._

object ThreadUtil {
	def sleepFor(duration:Duration) {
		Thread sleep duration.millis
	}
	
	def installDefaultUncaughtExceptionHandler(handler:(Thread,Throwable)=>Unit) {
		Thread setDefaultUncaughtExceptionHandler threadExceptionHandler(handler)
	}
	
	def installUncaughtExceptionHandler(thread:Thread)(handler:(Thread,Throwable)=>Unit) {
		thread setUncaughtExceptionHandler threadExceptionHandler(handler)
	}
	
	def threadExceptionHandler(handler:(Thread,Throwable)=>Unit) =
			new Thread.UncaughtExceptionHandler {
				def uncaughtException(t:Thread, e:Throwable) { 
					handler(t, e) 
				}
			}
}
